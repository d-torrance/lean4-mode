;;; lean4-rpc.el --- Lean interactive RPC sessions -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jan Mas Rovira
;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: MIT

;; Derived from neo-lean-mode <https://github.com/janmasrovira/neo-lean-mode>,
;; file neo-lean-rpc.el, by Jan Mas Rovira, which is MIT licensed; see
;; LICENSES/MIT.txt.  Re-derived against Lean's own protocol documentation in
;; src/Lean/Server/ProtocolOverview.lean.

;;; Commentary:

;; Lean's interactive features -- goals whose subterms can be hovered and
;; jumped to, collapsible traces, goal diffs, user widgets -- are not plain
;; LSP requests.  They are calls into the running file worker, made over a
;; session established with `$/lean/rpc/connect' and carried by
;; `$/lean/rpc/call'.  This library is that session layer; it renders nothing.
;;
;; Three properties of the protocol drive the design:
;;
;; Sessions expire.  The server drops a session it has not heard from for 30
;; seconds, so a keep-alive runs well inside that.  A session also dies
;; whenever the file worker restarts, which happens routinely -- editing an
;; import does it -- and is reported as one of four error codes.  Callers
;; should never have to care, so a dead session is reconnected once,
;; transparently, and the call retried.
;;
;; Calls carry a position.  `$/lean/rpc/call' extends
;; TextDocumentPositionParams because Lean resolves user-registered RPC
;; methods against the environment as it stands at that point in the file.
;; Hence `lean4-rpc-open' returns a position-bound handle rather than a bare
;; session.
;;
;; Results contain references to server-side objects.  These are reference
;; counted: the server hands out an `RpcRef' and waits to be told, once per
;; time it was served, that we are done with it.  Emacs has no finalizers to
;; hang that on, so release is explicit and batched; see `lean4-rpc-release'.
;; Leaking is safe but costs the server memory for as long as the file stays
;; open.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)

(require 'lean4-eglot)

;;;; Wire format

;; An `RpcRef' is a JSON object with a single numeric field.  Wire format v0
;; called it "p", which could collide with a field of the same name in
;; user data (leanprover/vscode-lean4#712); v1 renamed it to "__rpcref".  We
;; advertise v1 and honour whatever the server says it settled on, since a
;; toolchain older than Lean 4.30 will not know about v1 at all.

(defconst lean4-rpc-wire-format "v1"
  "The `RpcRef' wire format this client asks for.")

(defconst lean4-rpc--ref-key-v1 :__rpcref)
(defconst lean4-rpc--ref-key-v0 :p)

(defun lean4-rpc--negotiated-ref-key (connection)
  "Return the plist key CONNECTION uses inside an `RpcRef'.
Read from the server's own `experimental.rpcProvider.rpcWireFormat'
capability, defaulting to v0, which is what a server that has never
heard of the capability will be speaking."
  (let* ((capabilities (eglot--capabilities connection))
         (provider (thread-first capabilities
                                 (plist-get :experimental)
                                 (plist-get :rpcProvider)))
         (format (plist-get provider :rpcWireFormat)))
    (if (equal format "v1") lean4-rpc--ref-key-v1 lean4-rpc--ref-key-v0)))

(defun lean4-rpc-ref-p (object ref-key)
  "Return non-nil if OBJECT is an `RpcRef' encoded under REF-KEY."
  (and (listp object)
       (keywordp (car-safe object))
       (plist-member object ref-key)
       t))

;;;; Error codes

;; From Lean's Lean/Data/JsonRpc.lean.  Each of these means the session no
;; longer exists and every reference obtained through it is worthless.
(defconst lean4-rpc-needs-reconnect -32900)
(defconst lean4-rpc-content-modified -32801)
(defconst lean4-rpc-worker-exited -32901)
(defconst lean4-rpc-worker-crashed -32902)

(defconst lean4-rpc--dead-codes
  (list lean4-rpc-needs-reconnect
        lean4-rpc-content-modified
        lean4-rpc-worker-exited
        lean4-rpc-worker-crashed)
  "Error codes meaning the RPC session must be re-established.")

(defun lean4-rpc-dead-code-p (code)
  "Return non-nil if CODE says the RPC session is gone."
  (and (integerp code) (memq code lean4-rpc--dead-codes) t))

(defun lean4-rpc--error-code (error)
  "Return the JSON-RPC code of ERROR, however jsonrpc.el wrapped it."
  (or (plist-get error :code)
      (plist-get (plist-get error :error) :code)))

;;;; Sessions

(defconst lean4-rpc-keepalive-interval 20
  "Seconds between `$/lean/rpc/keepAlive' notifications.
Lean's file worker expires a session after 30 seconds of silence, so
this leaves a margin for a busy Emacs.")

(defconst lean4-rpc-release-batch-size 100
  "Number of stale references to accumulate before releasing them.")

(defconst lean4-rpc-release-delay 0.1
  "Seconds to wait before flushing accumulated stale references.")

(cl-defstruct (lean4-rpc-session (:constructor lean4-rpc--session-create))
  "An RPC session bound to one document on one server connection."
  connection                       ; the Eglot/jsonrpc connection
  uri                              ; document URI this session belongs to
  ref-key                          ; plist key for RpcRefs on this connection
  id                               ; server-assigned session id
  (settled nil)                    ; handshake has resolved, one way or other
  error                            ; non-nil if the handshake failed
  keepalive-timer
  (pending nil)                    ; thunks queued until the handshake settles
  (garbage nil)                    ; refs awaiting release
  release-timer
  (closed nil))

(defvar lean4-rpc--sessions (make-hash-table :test #'equal)
  "Live sessions, keyed by document URI.")

(defun lean4-rpc--flush-pending (session)
  "Run and clear the thunks queued on SESSION."
  (let ((thunks (nreverse (lean4-rpc-session-pending session))))
    (setf (lean4-rpc-session-pending session) nil)
    (mapc #'funcall thunks)))

(defun lean4-rpc--start-keepalive (session)
  "Begin sending keep-alive notifications for SESSION."
  (setf (lean4-rpc-session-keepalive-timer session)
        (run-with-timer
         lean4-rpc-keepalive-interval lean4-rpc-keepalive-interval
         (lambda ()
           (if (or (lean4-rpc-session-closed session)
                   (null (lean4-rpc-session-id session)))
               (lean4-rpc--stop-keepalive session)
             ;; The connection may have died between timer ticks; that is
             ;; not worth reporting, the next call will reconnect.
             (ignore-errors
               (jsonrpc-notify
                (lean4-rpc-session-connection session)
                :$/lean/rpc/keepAlive
                (list :uri (lean4-rpc-session-uri session)
                      :sessionId (lean4-rpc-session-id session)))))))))

(defun lean4-rpc--stop-keepalive (session)
  "Cancel SESSION's keep-alive timer."
  (when (timerp (lean4-rpc-session-keepalive-timer session))
    (cancel-timer (lean4-rpc-session-keepalive-timer session)))
  (setf (lean4-rpc-session-keepalive-timer session) nil))

(defun lean4-rpc--close (session)
  "Close SESSION: stop its timers and drop it from the registry.
Queued references are abandoned rather than released -- the server has
already discarded everything belonging to a dead session."
  (setf (lean4-rpc-session-closed session) t)
  (lean4-rpc--stop-keepalive session)
  (when (timerp (lean4-rpc-session-release-timer session))
    (cancel-timer (lean4-rpc-session-release-timer session)))
  (setf (lean4-rpc-session-release-timer session) nil
        (lean4-rpc-session-garbage session) nil)
  (when (eq session (gethash (lean4-rpc-session-uri session) lean4-rpc--sessions))
    (remhash (lean4-rpc-session-uri session) lean4-rpc--sessions)))

(defun lean4-rpc--connect (connection uri)
  "Open a session for URI on CONNECTION and register it at once.
Registering before the handshake resolves lets callers queue work
against a session that does not exist yet, which is the common case:
the first call is what triggers the connect."
  (let ((session (lean4-rpc--session-create
                  :connection connection
                  :uri uri
                  :ref-key (lean4-rpc--negotiated-ref-key connection))))
    (puthash uri session lean4-rpc--sessions)
    (cl-flet ((fail (error)
                (setf (lean4-rpc-session-error session) error
                      (lean4-rpc-session-settled session) t)
                (lean4-rpc--close session)
                ;; Still flush: queued calls must fail rather than hang.
                (lean4-rpc--flush-pending session)))
      ;; The handshake itself can signal if the server exited between the
      ;; caller deciding to connect and this call being made.
      (condition-case error
          (jsonrpc-async-request
           connection :$/lean/rpc/connect (list :uri uri)
           :success-fn
           (lambda (result)
             (setf (lean4-rpc-session-id session) (plist-get result :sessionId)
                   (lean4-rpc-session-settled session) t)
             (lean4-rpc--start-keepalive session)
             (lean4-rpc--flush-pending session))
           :error-fn #'fail)
        (error (fail (list :code lean4-rpc-worker-exited
                           :message (error-message-string error))))))
    session))

(defun lean4-rpc--session-for (connection uri)
  "Return a usable session for URI on CONNECTION, creating one if needed.
A session belonging to a previous connection is discarded: after a
server restart the id it holds means nothing."
  (let ((existing (gethash uri lean4-rpc--sessions)))
    (if (and existing
             (not (lean4-rpc-session-closed existing))
             (not (lean4-rpc-session-error existing))
             (eq (lean4-rpc-session-connection existing) connection)
             (lean4-rpc-connection-live-p connection))
        existing
      (lean4-rpc--connect connection uri))))

(defun lean4-rpc-close-all (&optional connection)
  "Close every session, or only those belonging to CONNECTION."
  (let (doomed)
    (maphash (lambda (_uri session)
               (when (or (null connection)
                         (eq (lean4-rpc-session-connection session) connection))
                 (push session doomed)))
             lean4-rpc--sessions)
    (mapc #'lean4-rpc--close doomed)))

;;;; Releasing references

(defun lean4-rpc-release (session refs)
  "Note that REFS obtained through SESSION are no longer needed.
The release is batched: Lean reference counts what it hands out, and a
notification per subterm of a goal would be thousands of notifications
per keystroke."
  (when (and refs (not (lean4-rpc-session-closed session)))
    (setf (lean4-rpc-session-garbage session)
          (append refs (lean4-rpc-session-garbage session)))
    (if (>= (length (lean4-rpc-session-garbage session))
            lean4-rpc-release-batch-size)
        (lean4-rpc--flush-garbage session)
      (unless (timerp (lean4-rpc-session-release-timer session))
        (setf (lean4-rpc-session-release-timer session)
              (run-with-timer lean4-rpc-release-delay nil
                              #'lean4-rpc--flush-garbage session))))))

(defun lean4-rpc--flush-garbage (session)
  "Send `$/lean/rpc/release' for everything SESSION has accumulated."
  (when (timerp (lean4-rpc-session-release-timer session))
    (cancel-timer (lean4-rpc-session-release-timer session)))
  (setf (lean4-rpc-session-release-timer session) nil)
  (let ((refs (lean4-rpc-session-garbage session)))
    (setf (lean4-rpc-session-garbage session) nil)
    (when (and refs
               (not (lean4-rpc-session-closed session))
               (lean4-rpc-session-id session))
      (ignore-errors
        (jsonrpc-notify
         (lean4-rpc-session-connection session)
         :$/lean/rpc/release
         (list :uri (lean4-rpc-session-uri session)
               :sessionId (lean4-rpc-session-id session)
               :refs (vconcat refs)))))))

;;;; Calls

(cl-defstruct (lean4-rpc-handle (:constructor lean4-rpc--handle-create))
  "A position-bound handle for making RPC calls."
  session
  position)                        ; TextDocumentPositionParams plist

(defun lean4-rpc-position-params ()
  "Return TextDocumentPositionParams for point.
The character offset is counted the way LSP wants it, in UTF-16 code
units, which is not the same as Emacs columns in a Lean file."
  (eglot--TextDocumentPositionParams))

(defun lean4-rpc-open (&optional position)
  "Return an RPC handle bound to POSITION, defaulting to point.
Requires a Lean server managing the current buffer."
  (let* ((position (or position (lean4-rpc-position-params)))
         (connection (eglot--current-server-or-lose))
         (uri (plist-get (plist-get position :textDocument) :uri)))
    (lean4-rpc--handle-create
     :session (lean4-rpc--session-for connection uri)
     :position position)))

(defun lean4-rpc-handle-ref-key (handle)
  "Return the `RpcRef' plist key in force for HANDLE."
  (lean4-rpc-session-ref-key (lean4-rpc-handle-session handle)))

(defun lean4-rpc-connection-live-p (connection)
  "Return non-nil if CONNECTION can still carry a request."
  (and connection
       (or (not (fboundp 'jsonrpc-running-p))
           (jsonrpc-running-p connection))))

(defun lean4-rpc-handle-live-p (handle)
  "Return non-nil if HANDLE can still be used."
  (and handle
       (let ((session (lean4-rpc-handle-session handle)))
         (and (not (lean4-rpc-session-closed session))
              (lean4-rpc-connection-live-p
               (lean4-rpc-session-connection session))))))

(defun lean4-rpc--send (session position method params success failure)
  "Send METHOD with PARAMS on a settled SESSION at POSITION."
  (cond
   ((lean4-rpc-session-error session)
    (funcall failure (lean4-rpc-session-error session)))
   ((lean4-rpc-session-closed session)
    (funcall failure (list :code lean4-rpc-needs-reconnect
                           :message "Lean RPC session is closed")))
   ;; A server that has exited takes its sessions with it.  Report that the
   ;; way a dead session is reported rather than letting jsonrpc signal:
   ;; callers here are display code and hover popups, and an error raised
   ;; from a timer or a post-command hook is much worse than a blank.
   ((not (lean4-rpc-connection-live-p (lean4-rpc-session-connection session)))
    (lean4-rpc--close session)
    (funcall failure (list :code lean4-rpc-worker-exited
                           :message "Lean server is not running")))
   (t
    (condition-case error
        (jsonrpc-async-request
         (lean4-rpc-session-connection session)
         :$/lean/rpc/call
         (append position
                 (list :sessionId (lean4-rpc-session-id session)
                       :method method
                       :params (or params eglot--{})))
         :success-fn success
         :error-fn (lambda (error)
                     (when (lean4-rpc-dead-code-p (lean4-rpc--error-code error))
                       (lean4-rpc--close session))
                     (funcall failure error))
         :timeout-fn (lambda ()
                       (funcall failure (list :code -32000
                                              :message "timed out"))))
      (error
       (lean4-rpc--close session)
       (funcall failure (list :code lean4-rpc-worker-exited
                              :message (error-message-string error))))))))

(defun lean4-rpc-call (handle method params success &optional failure)
  "Call Lean RPC METHOD with PARAMS through HANDLE.

SUCCESS receives the result.  FAILURE, if given, receives the error
plist; otherwise errors are dropped, which is usually right for a
display that will be refreshed on the next keystroke anyway.

A call made while the session is still connecting is queued.  A call
that fails because the session died is retried once against a fresh
session -- the file worker restarts often enough that surfacing it to
callers would mean every caller reimplementing this."
  (let ((failure (or failure #'ignore))
        (retried nil))
    (letrec
        ((on-failure
          (lambda (error)
            (if (and (not retried)
                     (lean4-rpc-dead-code-p (lean4-rpc--error-code error)))
                (let ((session (lean4-rpc-handle-session handle)))
                  (setq retried t)
                  (lean4-rpc--close session)
                  (setf (lean4-rpc-handle-session handle)
                        (lean4-rpc--session-for
                         (lean4-rpc-session-connection session)
                         (lean4-rpc-session-uri session)))
                  (funcall attempt))
              (funcall failure error))))
         (attempt
          (lambda ()
            (let ((session (lean4-rpc-handle-session handle))
                  (position (lean4-rpc-handle-position handle)))
              (if (lean4-rpc-session-settled session)
                  (lean4-rpc--send session position method params
                                   success on-failure)
                (push (lambda ()
                        (lean4-rpc--send session position method params
                                         success on-failure))
                      (lean4-rpc-session-pending session)))))))
      (funcall attempt))))

(defconst lean4-rpc-sync-timeout 2.0
  "Seconds to wait for a synchronous RPC call before giving up.
Only user-initiated commands wait; nothing on a redisplay path does.")

(defun lean4-rpc-call-sync (handle method params)
  "Call METHOD with PARAMS through HANDLE and wait for the result.

Returns the result, or signals if the call fails or does not answer
within `lean4-rpc-sync-timeout'.  Reserved for commands the user
invoked, such as jumping to a definition, where an answer is the whole
point and where `xref' and friends require one synchronously.  Display
code must use `lean4-rpc-call' instead: blocking while Lean elaborates a
Mathlib file would freeze Emacs for seconds."
  (let ((deadline (+ (float-time) lean4-rpc-sync-timeout))
        result failure done)
    (lean4-rpc-call handle method params
                    (lambda (value) (setq result value done t))
                    (lambda (error) (setq failure error done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (cond (failure (error "Lean RPC call %s failed: %S" method failure))
          ((not done) (error "Lean RPC call %s timed out" method))
          (t result))))

;;;; The methods Lean provides

;; Registered by `Server.registerBuiltinRpcProcedure' in
;; src/Lean/Server/FileWorker/WidgetRequests.lean and
;; src/Lean/Widget/UserWidget.lean.

(defun lean4-rpc-get-interactive-goals (handle success &optional failure)
  "Request the tactic goals at HANDLE's position.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getInteractiveGoals"
                  (lean4-rpc-handle-position handle) success failure))

(defun lean4-rpc-get-interactive-term-goal (handle success &optional failure)
  "Request the expected type at HANDLE's position.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getInteractiveTermGoal"
                  (lean4-rpc-handle-position handle) success failure))

(defun lean4-rpc-get-interactive-diagnostics (handle success &optional failure
                                                     start-line end-line)
  "Request HANDLE's interactive diagnostics, optionally limited by lines.
START-LINE and END-LINE are zero-based; END-LINE is exclusive.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getInteractiveDiagnostics"
                  (if (and start-line end-line)
                      (list :lineRange (list :start start-line :end end-line))
                    eglot--{})
                  success failure))

(defun lean4-rpc-info-to-interactive (handle info success &optional failure)
  "Expand INFO, a subterm reference from HANDLE, into a type and docstring.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
                  info success failure))

(defun lean4-rpc-get-go-to-location (handle kind info success &optional failure)
  "Ask HANDLE where the subterm INFO is defined.
KIND is one of the strings \"declaration\", \"definition\" or \"type\".
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getGoToLocation"
                  (list :kind kind :info info) success failure))

(defun lean4-rpc-lazy-trace-children (handle children success &optional failure)
  "Expand CHILDREN, an unexpanded trace node from HANDLE, into messages.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.lazyTraceChildrenToInteractive"
                  children success failure))

(defun lean4-rpc-get-widgets (handle success &optional failure)
  "Request the user widgets active at HANDLE's position.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getWidgets"
                  (plist-get (lean4-rpc-handle-position handle) :position)
                  success failure))

(defun lean4-rpc-get-widget-source (handle hash success &optional failure)
  "Request from HANDLE the source of the widget module identified by HASH.
SUCCESS receives the result; FAILURE, if given, receives any error."
  (lean4-rpc-call handle "Lean.Widget.getWidgetSource"
                  (list :hash hash
                        :pos (plist-get (lean4-rpc-handle-position handle)
                                        :position))
                  success failure))

(provide 'lean4-rpc)
;;; lean4-rpc.el ends here
