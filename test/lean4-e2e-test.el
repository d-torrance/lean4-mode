;;; lean4-e2e-test.el --- End-to-end tests against a real Lean server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; These start a real `lake serve' against test/fixture and are therefore
;; slow and dependent on a Lean toolchain.  They are tagged `:e2e' and are
;; not part of `make test'; run them with `make e2e'.
;;
;; Nothing here sleeps for a fixed duration.  Waiting is done by pumping the
;; process filter until a stated condition holds or a deadline passes, so the
;; tests are as fast as the machine allows and fail with a description of
;; what never became true rather than with a mystery assertion failure.

;;; Code:

(require 'ert)
(require 'flymake)
(require 'lean4-mode)
(require 'lean4-rpc)
(require 'lean4-render)

(defconst lean4-e2e--fixture-directory
  (expand-file-name
   "fixture/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the Lean project the end-to-end tests run against.")

(defconst lean4-e2e--fixture-file
  (expand-file-name "Fixture.lean" lean4-e2e--fixture-directory))

;; Line numbers in Fixture.lean, zero-based, as the protocol counts them.
(defconst lean4-e2e--sorry-line 7
  "Line of the `sorry' in the fixture.
The goal stands *at* the token: just past it Lean reports \"no goals\",
so these tests place point at the start of the line's indentation.")
(defconst lean4-e2e--error-line 9 "Line of the type error in the fixture.")

(defconst lean4-e2e--timeout 180
  "Seconds to allow for the server to start and elaborate the fixture.
Generous because a cold start compiles the toolchain's index.")

(defun lean4-e2e--wait-until (description predicate)
  "Pump process output until PREDICATE returns non-nil.
Signals an error naming DESCRIPTION if `lean4-e2e--timeout' passes
first.  Returns the value PREDICATE finally returned."
  (let ((deadline (+ (float-time) lean4-e2e--timeout))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      ;; A short timeout keeps this responsive without busy-waiting; the
      ;; work happens in the process filter this call runs.
      (accept-process-output nil 0.1))
    (unless value
      (error "Timed out after %ss waiting for %s"
             lean4-e2e--timeout description))
    value))

(defmacro lean4-e2e--with-fixture (&rest body)
  "Open the fixture with a running Lean server and evaluate BODY.
Point starts at `point-min'.  The server is shut down afterwards."
  (declare (indent 0) (debug (body)))
  `(let ((buffer (find-file-noselect lean4-e2e--fixture-file)))
     (unwind-protect
         (with-current-buffer buffer
           (should (derived-mode-p 'lean4-mode))
           ;; Not `eglot-ensure': it defers the connection to
           ;; `post-command-hook', which never runs under --batch.
           (let ((eglot-sync-connect t))
             (apply #'eglot--connect (eglot--guess-contact)))
           (should (eglot-current-server))
           ;; Diagnostics arrive incrementally while Lean elaborates, and
           ;; the type error is on the last line, so waiting for it is a
           ;; reliable signal that the whole file has been processed.
           (lean4-e2e--wait-until
            "the fixture to be fully elaborated"
            (lambda ()
              (flymake-start)
              (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
           (goto-char (point-min))
           ,@body)
       (when-let* ((server (with-current-buffer buffer (eglot-current-server))))
         (eglot-shutdown server nil nil 'preserve-buffers))
       (kill-buffer buffer))))

(defun lean4-e2e--error-p (diagnostic)
  "Return non-nil if Flymake DIAGNOSTIC is an error.
Eglot labels its diagnostics `eglot-error' and friends rather than with
Flymake's own `:error', so compare severities instead of symbols."
  (= (flymake--severity (flymake-diagnostic-type diagnostic))
     (flymake--severity :error)))

(defun lean4-e2e--goto-line (line)
  "Move point to zero-based LINE."
  (goto-char (point-min))
  (forward-line line))

(ert-deftest lean4-e2e-server-starts ()
  "The server starts and adopts the buffer."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (should (eglot-managed-p))
    (should (cl-typep (eglot-current-server) 'lean4-eglot-lsp-server))))

(ert-deftest lean4-e2e-diagnostics-reach-flymake ()
  "Lean's errors and warnings arrive as Flymake diagnostics.
The fixture has exactly one of each: a `sorry' warning and a type
mismatch."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let* ((diagnostics (flymake-diagnostics))
           (errors (seq-filter #'lean4-e2e--error-p diagnostics)))
      (should (= (length errors) 1))
      (should (string-match-p
               "[Tt]ype mismatch"
               (lean4-diagnostic-message (car errors))))
      (should (seq-find
               (lambda (d) (string-search "sorry" (lean4-diagnostic-message d)))
               diagnostics)))))

(ert-deftest lean4-e2e-diagnostics-carry-lean-full-range ()
  "The raw LSP diagnostic is reachable, and Lean's `fullRange' with it.
This is what lets the info buffer show a message while point is anywhere
inside the declaration it is about, not just on the underlined line."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let ((diagnostic (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
      (should (lean4-diagnostic-lsp-data diagnostic))
      (should (lean4-diagnostic-full-range diagnostic))
      (should (= (lean4-diagnostic-full-start-line diagnostic)
                 lean4-e2e--error-line)))))

(ert-deftest lean4-e2e-plain-goal-at-sorry ()
  "`$/lean/plainGoal' returns the goal standing at the `sorry'."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let (result done)
      (jsonrpc-async-request
       (eglot-current-server) :$/lean/plainGoal
       (eglot--TextDocumentPositionParams)
       :success-fn (lambda (r) (setq result r done t))
       :error-fn (lambda (e) (setq result e done t)))
      (lean4-e2e--wait-until "the goal request to return" (lambda () done))
      (let ((goals (plist-get result :goals)))
        (should (> (length goals) 0))
        ;; `string-search', not `string-match-p': "+" is a regexp
        ;; metacharacter, so goal text is not its own regexp.
        (should (string-search "⊢" (elt goals 0)))
        (should (string-search "2 + 2 = 4" (elt goals 0)))))))

(ert-deftest lean4-e2e-file-progress-clears ()
  "File-progress overlays are gone once elaboration finishes.
A leftover overlay means the fringe never learned the file was done,
which is what happens when the URI-to-buffer lookup silently matches
nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--wait-until
     "the progress overlays to clear"
     (lambda ()
       (lean4-fringe-update-progress-overlays)
       (not (seq-find (lambda (ov) (eq (overlay-get ov 'face) 'lean4-fringe-face))
                      (overlays-in (point-min) (point-max))))))
    (should (seq-empty-p lean4-fringe-data))))

(ert-deftest lean4-e2e-info-buffer-shows-the-goal ()
  "The info buffer renders the goal at point.

`lean4-info-buffer-active' refreshes only when the info buffer is on
display *and* the Lean buffer is the one in the selected window, so both
windows have to exist for real.  Under --batch there is a single window,
and plain `display-buffer' would put the info buffer into it, leaving no
window showing the source."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (delete-other-windows)
    (set-window-buffer (selected-window) (current-buffer))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (set-window-buffer (split-window) lean4-info-buffer-name)
    (unwind-protect
        (progn
          (should (get-buffer-window lean4-info-buffer-name t))
          (should (lean4-info-buffer-active lean4-info-buffer-name))
          (lean4-e2e--goto-line lean4-e2e--sorry-line)
          (back-to-indentation)
          (lean4-info-buffer-refresh)
          (lean4-e2e--wait-until
           "the goal to reach the info buffer"
           (lambda ()
             (with-current-buffer lean4-info-buffer-name
               (string-search "2 + 2 = 4" (buffer-string)))))
          (with-current-buffer lean4-info-buffer-name
            ;; `magit-insert-heading' appends the child count, so the
            ;; heading reads "Goals (1)".
            (should (string-search "Goals" (buffer-string)))
            ;; The server's own wording, not Eglot's "Lean 4: " prefixed
            ;; version, which is useful in the echo area but noise here.
            (should-not (string-search "Lean 4: " (buffer-string)))))
      (delete-other-windows))))

;;;; Interactive RPC

(defun lean4-e2e--rpc (call)
  "Run CALL, a function of (SUCCESS FAILURE), and return its result.
Signals if the call fails or never answers."
  (let (result failure done)
    (funcall call
             (lambda (value) (setq result value done t))
             (lambda (error) (setq failure error done t)))
    (lean4-e2e--wait-until "the RPC call to return" (lambda () done))
    (when failure
      (error "RPC call failed: %S" failure))
    result))

(ert-deftest lean4-e2e-rpc-negotiates-wire-format-v1 ()
  "The server agrees to the v1 reference encoding we ask for.
Version 0 names the field \"p\", which can collide with user data."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (should (eq (lean4-rpc-handle-ref-key (lean4-rpc-open)) :__rpcref))))

(ert-deftest lean4-e2e-rpc-interactive-goals-render ()
  "Interactive goals come back and render to the expected proposition."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (result (lean4-e2e--rpc
                    (lambda (success failure)
                      (lean4-rpc-get-interactive-goals handle success failure))))
           (rendered (lean4-render-goals (plist-get result :goals)))
           ;; The goal prefix and any hypothesis names are Lean4-Mode's own
           ;; text, not part of the term, so only the type is expected to
           ;; be covered by subterms.
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get result :goals) 0) :type))))
      (should rendered)
      (should (string-search "2 + 2 = 4" (substring-no-properties rendered)))
      (should (equal (substring-no-properties type) "2 + 2 = 4"))
      ;; Every character of the term must carry the subterm covering it,
      ;; or hovering and jumping cannot work.
      (dotimes (index (length type))
        (should (get-text-property index 'lean4-subexpr-pos type))
        (should (get-text-property index 'lean4-info type))))))

(ert-deftest lean4-e2e-rpc-subterm-hover ()
  "A subterm reference expands into its type.
This is what the goal buffer shows when point is on a subterm."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (goals (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-get-interactive-goals handle success failure))))
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get goals :goals) 0) :type)))
           ;; The first character of "2 + 2 = 4" is a numeral.
           (info (get-text-property 0 'lean4-info type))
           (popup (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-info-to-interactive handle info
                                                    success failure)))))
      (should info)
      (should popup)
      ;; The popup reports the numeral's type.
      (should (string-search
               "Nat"
               (substring-no-properties
                (lean4-render-tagged-text (plist-get popup :type))))))))

(ert-deftest lean4-e2e-rpc-go-to-location ()
  "A subterm can say where it is defined.
This is what backs jumping from the goal buffer to a definition."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (goals (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-get-interactive-goals handle success failure))))
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get goals :goals) 0) :type)))
           ;; Index 2 is the "+", whose head symbol is a real definition.
           (info (get-text-property 2 'lean4-info type))
           (locations (lean4-e2e--rpc
                       (lambda (success failure)
                         (lean4-rpc-get-go-to-location handle "definition" info
                                                       success failure)))))
      (should (> (length locations) 0))
      (should (plist-get (elt locations 0) :targetUri)))))

(ert-deftest lean4-e2e-rpc-survives-a-restart ()
  "A call after the file worker restarts reconnects instead of failing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let ((handle (lean4-rpc-open)))
      ;; Prime the session.
      (lean4-e2e--rpc (lambda (success failure)
                        (lean4-rpc-get-interactive-goals handle success failure)))
      ;; Closing and reopening the document restarts the file worker, which
      ;; invalidates the session exactly as editing an import would.
      (lean4-refresh-file-dependencies)
      (lean4-e2e--wait-until
       "the file to be elaborated again"
       (lambda ()
         (flymake-start)
         (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
      (let ((result (lean4-e2e--rpc
                     (lambda (success failure)
                       (lean4-rpc-get-interactive-goals handle
                                                        success failure)))))
        (should (lean4-render-goals (plist-get result :goals)))))))

;;;; The interactive info buffer

(defmacro lean4-e2e--with-info-window (&rest body)
  "Show the info buffer beside the Lean buffer and evaluate BODY.
Both windows must exist for `lean4-info-buffer-active' to be satisfied;
see `lean4-e2e-info-buffer-shows-the-goal'."
  (declare (indent 0) (debug (body)))
  `(progn
     (delete-other-windows)
     (set-window-buffer (selected-window) (current-buffer))
     (lean4-ensure-info-buffer lean4-info-buffer-name)
     ;; The buffer outlives each test, and every test connects a fresh
     ;; server.  Left as it was, its contents would satisfy the next test's
     ;; wait immediately, and its handle would point at a dead connection.
     (with-current-buffer lean4-info-buffer-name
       (let ((inhibit-read-only t)) (erase-buffer))
       (setq lean4-info--handle nil
             lean4-info--source-buffer nil))
     (set-window-buffer (split-window) lean4-info-buffer-name)
     (unwind-protect (progn ,@body)
       (delete-other-windows))))

(defun lean4-e2e--show-goal-at (line)
  "Put point on zero-based LINE and wait for the info buffer to catch up."
  (lean4-e2e--goto-line line)
  (back-to-indentation)
  (lean4-info-buffer-refresh)
  (lean4-e2e--wait-until
   "the goal to reach the info buffer"
   (lambda ()
     (with-current-buffer lean4-info-buffer-name
       (string-search "⊢" (buffer-string))))))

(ert-deftest lean4-e2e-info-buffer-goals-are-interactive ()
  "Goals in the info buffer carry per-subterm information.
Without this, nothing in the buffer can be hovered or jumped from."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'lean4-info))
        (should (get-text-property (point) 'lean4-subexpr-pos))))))

(ert-deftest lean4-e2e-info-buffer-subterm-bounds ()
  "The subterm under point spans the whole of the term it belongs to.
Point on the `+' should select `2 + 2', not just the operator."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        ;; Move onto the "+".
        (goto-char (+ (match-beginning 0) 2))
        (let ((bounds (lean4-info-subterm-bounds)))
          (should bounds)
          (should (equal (buffer-substring-no-properties
                          (car bounds) (cdr bounds))
                         "2 + 2")))))))

(ert-deftest lean4-e2e-info-buffer-eldoc-reports-a-type ()
  "ElDoc reports the type of the subterm under point."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        (goto-char (match-beginning 0))
        (let (reported)
          (should (lean4-info-eldoc-function
                   (lambda (text &rest _) (setq reported text))))
          (lean4-e2e--wait-until "ElDoc to report a type"
                                 (lambda () reported))
          (should (string-search "Nat" reported)))))))

(ert-deftest lean4-e2e-info-buffer-xref-finds-a-definition ()
  "xref resolves the subterm under point to a source location.
This is what makes \\[xref-find-definitions] work in the goal buffer."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        ;; The "+" resolves to the definition of addition.
        (goto-char (+ (match-beginning 0) 2))
        (should (eq (lean4-info-xref-backend) 'lean4-info))
        (let ((items (xref-backend-definitions 'lean4-info nil)))
          (should items)
          (should (xref-item-location (car items))))))))

(ert-deftest lean4-e2e-info-buffer-reports-accomplished-goals ()
  "A finished proof says so rather than showing nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      ;; Just past the `rfl' that closes the first theorem.
      (lean4-e2e--goto-line 4)
      (end-of-line)
      (lean4-info-buffer-refresh)
      (lean4-e2e--wait-until
       "the info buffer to report the proof is done"
       (lambda ()
         (with-current-buffer lean4-info-buffer-name
           (string-search "goals accomplished" (buffer-string))))))))

(provide 'lean4-e2e-test)
;;; lean4-e2e-test.el ends here
