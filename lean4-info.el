;;; lean4-info.el --- Emacs mode for Lean theorem prover  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Gabriel Ebner. All rights reserved.
;; Copyright (C) 2023 Buster Copley
;; Copyright (C) 2024 Paul D. Nelson
;; Copyright (C) 2026 Lean4-Mode contributors

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;; Commentary:

;; The `*Lean Goal*' buffer: the proof state and the messages at point, the
;; counterpart of VS Code's InfoView.
;;
;; Goals are fetched with `$/lean/plainGoal' and `$/lean/plainTermGoal',
;; which the Lean server documents as existing for editors without an
;; interactive InfoView.  They return pre-rendered strings, so nothing here
;; can offer hovering or navigation on a subterm; that needs Lean's RPC
;; layer and arrives later.
;;
;; Diagnostics come from Flymake, since that is what Eglot reports to.  The
;; raw LSP object is recovered from the Flymake diagnostic in order to read
;; Lean's non-standard `fullRange': Lean underlines only the first line of a
;; multi-line error, but the InfoView shows the message whenever point is
;; anywhere within its full extent.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'seq)
(require 'magit-section)

(require 'lean4-eglot)
(require 'lean4-settings)
(require 'lean4-syntax)

(defgroup lean4-info nil
  "Lean4-Mode Info."
  :group 'lean4)

;; Lean Info Mode (for "*lean4-info*" buffer)
;; Automode List
;;;###autoload
(define-derived-mode lean4-info-mode prog-mode "Lean-Info"
  "Major mode for Lean4-Mode Info Buffer."
  :syntax-table lean4-syntax-table
  :group 'lean4
  (setq-local font-lock-defaults lean4-info-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-indent-function 'common-lisp-indent-function))

(defconst lean4-info-buffer-name "*Lean Goal*")

(defvar-local lean4-goals nil
  "Goals reported for point, as a vector of pre-rendered strings.")

(defvar-local lean4-term-goal nil
  "Expected type at point, as a pre-rendered string.")

(defun lean4-ensure-info-buffer (buffer)
  "Create BUFFER if it does not exist.
Also choose settings used for the *Lean Goal* buffer."
  (unless (get-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (buffer-disable-undo)
      (magit-section-mode)
      (set-syntax-table lean4-syntax-table)
      (setq buffer-read-only t))))

(defun lean4-toggle-info-buffer (buffer)
  "Create or delete BUFFER.
The buffer is supposed to be the *Lean Goal* buffer."
  (if-let* ((window (get-buffer-window buffer)))
      (quit-window nil window)
    (lean4-ensure-info-buffer buffer)
    (display-buffer buffer)))

(defun lean4-info-buffer-active (buffer)
  "Check whether given info BUFFER should show info for current buffer."
  (and
   ;; info buffer visible (on any frame)
   (get-buffer-window buffer t)
   ;; current window of current buffer is selected (i.e., in focus)
   (eq (current-buffer) (window-buffer))))

;;;; Diagnostics

(defun lean4-diagnostic-lsp-data (diagnostic)
  "Return the raw LSP object behind Flymake DIAGNOSTIC, or nil.
Eglot stashes the server's original `Diagnostic' there, which is the
only way to reach Lean's extensions to it."
  (alist-get 'eglot-lsp-diag (flymake-diagnostic-data diagnostic)))

(defun lean4-diagnostic-full-range (diagnostic)
  "Return the `fullRange' of Flymake DIAGNOSTIC, falling back to `range'.
Lean reports two extents per diagnostic: `range', which is what gets
underlined, and `fullRange', which covers the whole construct the
message is about.  Servers older than the extension send only `range'."
  (let ((lsp (lean4-diagnostic-lsp-data diagnostic)))
    (or (plist-get lsp :fullRange)
        (plist-get lsp :range))))

(defun lean4-diagnostic-full-start-line (diagnostic)
  "Return the zero-based line DIAGNOSTIC's full range starts on."
  (thread-first (lean4-diagnostic-full-range diagnostic)
                (plist-get :start)
                (plist-get :line)))

(defun lean4-diagnostic-full-end-line (diagnostic)
  "Return the zero-based line DIAGNOSTIC's full range ends on."
  (thread-first (lean4-diagnostic-full-range diagnostic)
                (plist-get :end)
                (plist-get :line)))

(defun lean4-diagnostic-message (diagnostic)
  "Return the message text of Flymake DIAGNOSTIC.
Prefers the server's own wording: Eglot prefixes the text it hands
Flymake with the server name, which is useful in the echo area but only
noise in a buffer that shows nothing but Lean diagnostics."
  (or (plist-get (lean4-diagnostic-lsp-data diagnostic) :message)
      (let ((text (flymake-diagnostic-text diagnostic)))
        ;; Eglot 1.12 stores a string here; later versions store the list
        ;; (SOURCE CODE MESSAGE).
        (if (listp text) (car (last text)) text))))

(defun lean4-info--split-diagnostics (diagnostics line)
  "Partition DIAGNOSTICS relative to zero-based LINE.
Returns a list (ABOVE HERE BELOW).  A diagnostic is \"here\" when LINE
falls within its full range, which is how a message about a multi-line
declaration stays visible while point moves through it."
  (let (above here below)
    (dolist (diagnostic diagnostics)
      (cond
       ((< (lean4-diagnostic-full-end-line diagnostic) line)
        (push diagnostic above))
       ((<= (lean4-diagnostic-full-start-line diagnostic) line)
        (push diagnostic here))
       (t (push diagnostic below))))
    (list (nreverse above) (nreverse here) (nreverse below))))

;;;; Rendering

(defun lean4-info--fontify-string (text)
  "Return TEXT fontified as Lean source."
  (with-temp-buffer
    (delay-mode-hooks (lean4-info-mode))
    (insert text)
    (font-lock-ensure)
    (buffer-string)))

(defun lean4-info--error-button-action (data)
  "Jump to the source location a diagnostic button points at.
DATA is the button's `button-data', a list (BUFFER LINE COLUMN) with
LINE counted from one and COLUMN from zero."
  (let ((buffer (nth 0 data))
        (line (nth 1 data))
        (column (nth 2 data)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char column))))

(defun lean4-info--insert-highlight-inaccessible-names (&rest text)
  "Insert TEXT, dimming the names Lean marks as inaccessible.
When `lean4-highlight-inaccessible-names' is non-nil, a name suffixed
with the dagger Lean uses for inaccessible hypotheses is stripped of the
dagger and shown in `font-lock-comment-face' instead."
  (let ((begin (point)))
    (apply #'insert text)
    (when lean4-highlight-inaccessible-names
      (let ((end (point-marker)))
        (goto-char begin)
        (while (re-search-forward "\\(\\sw+\\)✝\\([¹²³⁴-⁹⁰]*\\)" end t)
          (replace-match
           (propertize (concat (match-string-no-properties 1)
                               (match-string-no-properties 2))
                       'font-lock-face 'font-lock-comment-face)
           'fixedcase 'literal))
        (goto-char end)))))

(defun lean4--insert-goal-text (text delimiter)
  "Insert goal TEXT fontified as Lean, followed by DELIMITER."
  (lean4-info--insert-highlight-inaccessible-names
   (lean4-info--fontify-string text)
   delimiter))

(defun lean4-info--mk-message-section (value caption messages buffer)
  "Add a section with id VALUE, caption CAPTION and contents MESSAGES.
Each message is rendered as a button jumping into BUFFER at the
message's own line and column.  Nothing is inserted when MESSAGES is
empty."
  (when messages
    (magit-insert-section (magit-section value)
      (magit-insert-heading caption)
      (magit-insert-section-body
        (dolist (diagnostic messages)
          (let* ((range (plist-get (lean4-diagnostic-lsp-data diagnostic) :range))
                 (start (plist-get range :start))
                 (line (1+ (or (plist-get start :line) 0)))
                 (column (or (plist-get start :character) 0)))
            (insert-text-button
             (format "%d:%d:" line column)
             'action #'lean4-info--error-button-action
             'button-data (list buffer line column)
             'face 'magit-section-heading
             'help-echo "mouse-2: visit this file, line and column")
            (lean4-info--insert-highlight-inaccessible-names
             "\n" (lean4-diagnostic-message diagnostic) "\n")))))))

(defun lean4-info-buffer-redisplay ()
  "Re-render the Lean info buffer from the last goals and diagnostics.
Does nothing unless the info buffer is currently being displayed."
  (when (lean4-info-buffer-active lean4-info-buffer-name)
    (let* ((deactivate-mark)            ; keep transient mark
           (inhibit-read-only t)
           (buffer (current-buffer))
           (goals lean4-goals)
           (term-goal lean4-term-goal)
           (line (1- (line-number-at-pos nil 'absolute)))
           (diagnostics (sort (flymake-diagnostics)
                              (lambda (a b)
                                (< (lean4-diagnostic-full-end-line a)
                                   (lean4-diagnostic-full-end-line b))))))
      (pcase-let ((`(,above ,here ,below)
                   (lean4-info--split-diagnostics diagnostics line)))
        (with-current-buffer lean4-info-buffer-name
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (when goals
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body
                  (if (> (length goals) 0)
                      (seq-doseq (goal goals)
                        (magit-insert-section (magit-section)
                          (lean4--insert-goal-text goal "\n\n")))
                    (insert "goals accomplished\n\n")))))
            (when term-goal
              (magit-insert-section (magit-section 'term-goal)
                (magit-insert-heading "Expected type:")
                (magit-insert-section-body
                  (lean4--insert-goal-text term-goal "\n"))))
            (lean4-info--mk-message-section
             'errors-here "Messages here:" here buffer)
            (lean4-info--mk-message-section
             'errors-below "Messages below:" below buffer)
            (lean4-info--mk-message-section
             'errors-above "Messages above:" above buffer)))))))

;;;; Refresh

(defcustom lean4-info-debounce-delay 0.1
  "Seconds of quiet before the info buffer is re-rendered.
`magit-section' rendering is not cheap, and point moves in bursts."
  :group 'lean4-info
  :type 'number)

(defvar lean4-info--debounce-timer nil)

(defun lean4-info-buffer-redisplay-debounced ()
  "Schedule a redisplay of the info buffer, coalescing rapid requests."
  (when (timerp lean4-info--debounce-timer)
    (cancel-timer lean4-info--debounce-timer))
  (let ((buffer (current-buffer)))
    (setq lean4-info--debounce-timer
          (run-with-idle-timer
           lean4-info-debounce-delay nil
           (lambda ()
             (setq lean4-info--debounce-timer nil)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lean4-info-buffer-redisplay))))))))

(defvar-local lean4-info--generation 0
  "Counter used to discard replies that arrive out of order.
Goal requests are asynchronous and Lean does not answer them in the
order they were sent, so a reply for a position point has already left
would otherwise overwrite a newer one.")

(defun lean4-info--request (server method generation setter)
  "Ask SERVER for METHOD at point and pass the result to SETTER.
The reply is dropped unless GENERATION is still current.  Errors are
ignored: the server routinely rejects position requests for a region it
is still elaborating, and there is nothing useful to report."
  (let ((buffer (current-buffer)))
    (jsonrpc-async-request
     server method (eglot--TextDocumentPositionParams)
     :success-fn
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eq generation lean4-info--generation)
             (funcall setter result)
             (lean4-info-buffer-redisplay)))))
     :error-fn #'ignore
     :timeout-fn #'ignore)))

(defun lean4-info-buffer-refresh ()
  "Refresh the goals shown in the Lean info buffer.
Does nothing unless the info buffer is on display: the requests are not
free, and Lean is slow to answer them while a file is elaborating."
  (when (lean4-info-buffer-active lean4-info-buffer-name)
    (when-let* ((server (eglot-current-server))
                (generation (cl-incf lean4-info--generation)))
      (lean4-info--request
       server :$/lean/plainGoal generation
       (lambda (result) (setq lean4-goals (plist-get result :goals))))
      (lean4-info--request
       server :$/lean/plainTermGoal generation
       (lambda (result) (setq lean4-term-goal (plist-get result :goal)))))))

;;;###autoload
(defun lean4-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (lean4-toggle-info-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

(provide 'lean4-info)
;;; lean4-info.el ends here
