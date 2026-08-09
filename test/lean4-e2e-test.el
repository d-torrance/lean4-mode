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

(provide 'lean4-e2e-test)
;;; lean4-e2e-test.el ends here
