;;; lean4-rpc-test.el --- Tests for the Lean RPC session layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; No subprocess and no server: jsonrpc is stubbed so that the session
;; state machine -- handshake queueing, reconnection, reference batching --
;; can be driven directly, including the failure paths a real server would
;; only produce occasionally.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-rpc)

(defvar lean4-rpc-test--sent nil
  "Requests and notifications the stub recorded, newest last.")

(defvar lean4-rpc-test--connect-response nil
  "How the stub answers `$/lean/rpc/connect'.
Either (:success PLIST) or (:error PLIST), or nil to leave the
handshake outstanding so that queueing can be observed.")

(defvar lean4-rpc-test--call-responses nil
  "Queue of answers the stub gives to `$/lean/rpc/call', consumed in turn.")

(defun lean4-rpc-test--capabilities (wire-format)
  "Return server capabilities advertising WIRE-FORMAT."
  (list :experimental (list :rpcProvider (list :rpcWireFormat wire-format))))

(defmacro lean4-rpc-test--with-stub (wire-format &rest body)
  "Evaluate BODY with jsonrpc stubbed and the registry empty.
WIRE-FORMAT is what the pretend server advertises.  `connection' is
bound to a stand-in object."
  (declare (indent 1) (debug (form body)))
  `(let ((lean4-rpc--sessions (make-hash-table :test #'equal))
         (lean4-rpc-test--sent nil)
         (lean4-rpc-test--connect-response nil)
         (lean4-rpc-test--call-responses nil)
         (connection (make-symbol "stub-connection")))
     (cl-letf (((symbol-function 'eglot--capabilities)
                (lambda (_connection) (lean4-rpc-test--capabilities ,wire-format)))
               ;; The stand-in connection is not a real jsonrpc object, so
               ;; answer the liveness check on its behalf.  Tests about a
               ;; dead server override this again.
               ((symbol-function 'jsonrpc-running-p) (lambda (_connection) t))
               ((symbol-function 'jsonrpc-notify)
                (lambda (_connection method params)
                  (push (list method params) lean4-rpc-test--sent)))
               ((symbol-function 'jsonrpc-async-request)
                (lambda (_connection method params &rest handlers)
                  (push (list method params) lean4-rpc-test--sent)
                  (let ((success (plist-get handlers :success-fn))
                        (failure (plist-get handlers :error-fn)))
                    (pcase method
                      (:$/lean/rpc/connect
                       (pcase lean4-rpc-test--connect-response
                         (`(:success ,result) (funcall success result))
                         (`(:error ,error) (funcall failure error))
                         (_ nil)))       ; leave it hanging
                      (:$/lean/rpc/call
                       (pcase (pop lean4-rpc-test--call-responses)
                         (`(:success ,result) (funcall success result))
                         (`(:error ,error) (funcall failure error))
                         (_ nil))))))))
       ,@body)))

(defun lean4-rpc-test--methods ()
  "Return the methods the stub has seen, oldest first."
  (mapcar #'car (reverse lean4-rpc-test--sent)))

;;;; Pure predicates

(ert-deftest lean4-rpc-dead-codes ()
  "The four codes meaning the session is gone are recognised, others not."
  (dolist (code (list lean4-rpc-needs-reconnect lean4-rpc-content-modified
                      lean4-rpc-worker-exited lean4-rpc-worker-crashed))
    (should (lean4-rpc-dead-code-p code)))
  (should-not (lean4-rpc-dead-code-p -32601))   ; method not found
  (should-not (lean4-rpc-dead-code-p nil))
  (should-not (lean4-rpc-dead-code-p "-32900")))

(ert-deftest lean4-rpc-error-code-is-found-either-way ()
  "The code is read whether or not jsonrpc wrapped the error."
  (should (= (lean4-rpc--error-code '(:code -32900)) -32900))
  (should (= (lean4-rpc--error-code '(:error (:code -32900))) -32900))
  (should-not (lean4-rpc--error-code '(:message "no code here"))))

(ert-deftest lean4-rpc-ref-key-follows-the-server ()
  "The reference key is whatever the server says it negotiated."
  (lean4-rpc-test--with-stub "v1"
    (should (eq (lean4-rpc--negotiated-ref-key connection) :__rpcref)))
  (lean4-rpc-test--with-stub "v0"
    (should (eq (lean4-rpc--negotiated-ref-key connection) :p)))
  ;; A toolchain older than Lean 4.30 does not advertise this at all.
  (lean4-rpc-test--with-stub nil
    (should (eq (lean4-rpc--negotiated-ref-key connection) :p))))

(ert-deftest lean4-rpc-ref-recognition ()
  "A reference is recognised under the key in force."
  (should (lean4-rpc-ref-p '(:__rpcref "3") :__rpcref))
  (should-not (lean4-rpc-ref-p '(:__rpcref "3") :p))
  (should-not (lean4-rpc-ref-p "not a ref" :__rpcref))
  (should-not (lean4-rpc-ref-p nil :__rpcref)))

;;;; Session lifecycle

(ert-deftest lean4-rpc-connects-once-per-document ()
  "Two handles on one document share a session."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((first (lean4-rpc--session-for connection "file:///a.lean"))
          (second (lean4-rpc--session-for connection "file:///a.lean")))
      (should (eq first second))
      (should (equal (lean4-rpc-test--methods) '(:$/lean/rpc/connect))))))

(ert-deftest lean4-rpc-reconnects-for-a-new-connection ()
  "A session from a previous connection is not reused.
After the server restarts, the id it holds means nothing."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let* ((old (lean4-rpc--session-for connection "file:///a.lean"))
           (restarted (make-symbol "restarted-connection"))
           (new (lean4-rpc--session-for restarted "file:///a.lean")))
      (should-not (eq old new)))))

(ert-deftest lean4-rpc-queues-calls-during-the-handshake ()
  "A call made before the handshake resolves is queued, then sent."
  (lean4-rpc-test--with-stub "v1"
    ;; Leave connect outstanding.
    (setq lean4-rpc-test--connect-response nil)
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (results nil))
      (lean4-rpc-call handle "Test.method" nil
                      (lambda (result) (push result results)))
      ;; Nothing sent yet beyond the connect itself.
      (should (equal (lean4-rpc-test--methods) '(:$/lean/rpc/connect)))
      ;; Now let the handshake succeed, as the stub's connect callback would.
      (let ((session (lean4-rpc-handle-session handle)))
        (setf (lean4-rpc-session-id session) 7
              (lean4-rpc-session-settled session) t)
        (setq lean4-rpc-test--call-responses '((:success (:ok t))))
        (lean4-rpc--flush-pending session))
      (should (equal (lean4-rpc-test--methods)
                     '(:$/lean/rpc/connect :$/lean/rpc/call)))
      (should (equal results '((:ok t)))))))

(ert-deftest lean4-rpc-queued-calls-run-in-order ()
  "Queued calls are issued in the order they were made."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response nil)
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean")))))
      (dolist (name '("first" "second" "third"))
        (lean4-rpc-call handle name nil #'ignore))
      (let ((session (lean4-rpc-handle-session handle)))
        (setf (lean4-rpc-session-id session) 7
              (lean4-rpc-session-settled session) t)
        (setq lean4-rpc-test--call-responses
              '((:success nil) (:success nil) (:success nil)))
        (lean4-rpc--flush-pending session))
      (should (equal (mapcar (lambda (entry) (plist-get (cadr entry) :method))
                             (seq-filter (lambda (entry)
                                           (eq (car entry) :$/lean/rpc/call))
                                         (reverse lean4-rpc-test--sent)))
                     '("first" "second" "third"))))))

(ert-deftest lean4-rpc-failed-handshake-fails-queued-calls ()
  "A handshake that fails does not leave callers waiting forever."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response
          '(:error (:code -32603 :message "no")))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (failures nil))
      (lean4-rpc-call handle "Test.method" nil #'ignore
                      (lambda (error) (push error failures)))
      (should failures))))

(ert-deftest lean4-rpc-retries-once-on-a-dead-session ()
  "A dead session is re-established and the call retried, transparently.
The file worker restarts whenever an import changes, so callers should
never have to handle this themselves."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (results nil)
          (failures nil))
      (setq lean4-rpc-test--call-responses
            `((:error (:code ,lean4-rpc-needs-reconnect))
              (:success (:ok t))))
      (lean4-rpc-call handle "Test.method" nil
                      (lambda (result) (push result results))
                      (lambda (error) (push error failures)))
      (should-not failures)
      (should (equal results '((:ok t))))
      ;; connect, call, reconnect, call
      (should (equal (lean4-rpc-test--methods)
                     '(:$/lean/rpc/connect :$/lean/rpc/call
                       :$/lean/rpc/connect :$/lean/rpc/call))))))

(ert-deftest lean4-rpc-gives-up-after-one-retry ()
  "A session that dies twice is reported rather than retried forever."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (failures nil))
      (setq lean4-rpc-test--call-responses
            `((:error (:code ,lean4-rpc-needs-reconnect))
              (:error (:code ,lean4-rpc-needs-reconnect))))
      (lean4-rpc-call handle "Test.method" nil #'ignore
                      (lambda (error) (push error failures)))
      (should (= (length failures) 1)))))

(ert-deftest lean4-rpc-ordinary-errors-are-not-retried ()
  "An error that is not about the session is passed straight through."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (failures nil))
      (setq lean4-rpc-test--call-responses '((:error (:code -32601))))
      (lean4-rpc-call handle "Nope" nil #'ignore
                      (lambda (error) (push error failures)))
      (should (= (length failures) 1))
      (should (= (length (seq-filter (lambda (entry)
                                       (eq (car entry) :$/lean/rpc/connect))
                                     lean4-rpc-test--sent))
                 1)))))

;;;; Releasing references

(ert-deftest lean4-rpc-release-is-batched ()
  "A handful of references is held back rather than sent immediately.
A notification per subterm would be thousands per keystroke."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((session (lean4-rpc--session-for connection "file:///a.lean")))
      (lean4-rpc-release session '((:__rpcref "1") (:__rpcref "2")))
      (should-not (memq :$/lean/rpc/release (lean4-rpc-test--methods)))
      (should (= (length (lean4-rpc-session-garbage session)) 2))
      ;; Flushing sends them all at once.
      (lean4-rpc--flush-garbage session)
      (should (memq :$/lean/rpc/release (lean4-rpc-test--methods)))
      (should-not (lean4-rpc-session-garbage session)))))

(ert-deftest lean4-rpc-release-flushes-when-full ()
  "Exceeding the batch size flushes without waiting for the timer."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((session (lean4-rpc--session-for connection "file:///a.lean")))
      (lean4-rpc-release session
                         (mapcar (lambda (n) (list :__rpcref (number-to-string n)))
                                 (number-sequence 1 lean4-rpc-release-batch-size)))
      (should (memq :$/lean/rpc/release (lean4-rpc-test--methods)))
      (should-not (lean4-rpc-session-garbage session)))))

(ert-deftest lean4-rpc-closing-abandons-references ()
  "References are dropped, not released, when the session dies.
The server has already discarded everything belonging to it."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((session (lean4-rpc--session-for connection "file:///a.lean")))
      (lean4-rpc-release session '((:__rpcref "1")))
      (lean4-rpc--close session)
      (should-not (lean4-rpc-session-garbage session))
      (should-not (memq :$/lean/rpc/release (lean4-rpc-test--methods)))
      ;; And it is out of the registry, so the next call reconnects.
      (should-not (gethash "file:///a.lean" lean4-rpc--sessions)))))

;;;; A server that has gone away

(ert-deftest lean4-rpc-dead-connection-fails-rather-than-signals ()
  "A call on a connection that has exited reports, it does not signal.

Callers here are display code, ElDoc and post-command hooks.  An error
raised from one of those is far worse than a blank goal buffer, and a
Lean server exiting -- or being restarted -- is routine."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (failures nil))
      (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) nil)))
        (should-not (lean4-rpc-handle-live-p handle))
        (lean4-rpc-call handle "Test.method" nil #'ignore
                        (lambda (error) (push error failures))))
      (should failures)
      (should (lean4-rpc-dead-code-p
               (lean4-rpc--error-code (car failures)))))))

(ert-deftest lean4-rpc-jsonrpc-signal-becomes-a-failure ()
  "An error raised by jsonrpc itself is reported through the callback."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let ((handle (lean4-rpc--handle-create
                   :session (lean4-rpc--session-for connection "file:///a.lean")
                   :position '(:textDocument (:uri "file:///a.lean"))))
          (failures nil))
      (cl-letf (((symbol-function 'jsonrpc-async-request)
                 (lambda (&rest _) (error "Process not running: killed"))))
        (lean4-rpc-call handle "Test.method" nil #'ignore
                        (lambda (error) (push error failures))))
      (should (= (length failures) 1)))))

(ert-deftest lean4-rpc-live-handle-predicate ()
  "A handle is live only while its session and connection both are."
  (lean4-rpc-test--with-stub "v1"
    (setq lean4-rpc-test--connect-response '(:success (:sessionId 7)))
    (let* ((session (lean4-rpc--session-for connection "file:///a.lean"))
           (handle (lean4-rpc--handle-create :session session :position nil)))
      (should (lean4-rpc-handle-live-p handle))
      (lean4-rpc--close session)
      (should-not (lean4-rpc-handle-live-p handle))))
  (should-not (lean4-rpc-handle-live-p nil)))

(provide 'lean4-rpc-test)
;;; lean4-rpc-test.el ends here
