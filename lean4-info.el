;;; lean4-info.el --- Emacs mode for Lean theorem prover  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Gabriel Ebner. All rights reserved.
;; Copyright (C) 2026 Doug Torrance

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
;; Goals are fetched over Lean's interactive RPC, which returns them as
;; trees in which every subterm is labelled.  That is what lets point rest
;; on a subterm and get its type, or jump to where it is defined.  Hover is
;; offered through ElDoc and jumping through xref, so the user's own
;; frontends present both.  When the server is too old for RPC, or
;; `lean4-info-interactive' is nil, the display falls back to
;; `$/lean/plainGoal', which is plain text and supports neither.
;;
;; Messages come from `Lean.Widget.getInteractiveDiagnostics', so terms
;; inside an error are as hoverable as terms inside a goal, and traces fold.
;; Without RPC they fall back to the raw objects Eglot stashes on the Flymake
;; diagnostics, which are plain strings.
;;
;; Either way they are placed by Lean's non-standard `fullRange' rather than
;; by `range': Lean underlines only the first line of a multi-line error, but
;; the message should stay visible while point is anywhere inside the
;; declaration it is about.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'seq)
(require 'magit-section)
(require 'eldoc)
(require 'xref)

(require 'lean4-eglot)
(require 'lean4-diagnostics)
(require 'lean4-render)
(require 'lean4-rpc)
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
  "Goals at point: a rendered string, `accomplished', or nil for none.")

(defvar-local lean4-term-goal nil
  "Expected type at point, as a rendered string, or nil.")

(defvar-local lean4-info--handle nil
  "RPC handle the goals on display were fetched through.")

(defvar-local lean4-info--source-buffer nil
  "The Lean buffer the goals on display came from.")

(defvar lean4-info-paused nil
  "Non-nil while the goal display is paused.")

(defvar lean4-info--pin nil
  "Marker at the location the goal display is pinned to, or nil.")

(defvar-local lean4-info--diagnostics nil
  "Interactive diagnostics for this buffer, as raw LSP plists.
Nil until the first RPC refresh, and while running without RPC, in
which case the display falls back to what Flymake holds.")

(defvar lean4-info--trace-expansion (make-hash-table :test #'equal)
  "Trace nodes the reader has unfolded, keyed by path.
The value is the children to show, so a node whose children had to be
fetched stays open across the redisplay every cursor movement triggers.

Global rather than buffer-local: unfolding happens in the info buffer
but redisplay reads from the Lean buffer, and there is only ever one
info buffer.")

(defvar-local lean4-info--refs nil
  "Server-side references owned by the goals currently on display.
Released when the next set replaces them; Lean reference counts these
and will hold the memory until told otherwise.")

(defvar-keymap lean4-info-buffer-map
  :doc "Keymap for the *Lean Goal* buffer."
  ;; `M-.' comes free from the xref backend; RET is the convenience the
  ;; VS Code InfoView offers for the same thing.
  "RET" #'xref-find-definitions
  "TAB" #'lean4-info-toggle-fold
  "C-c C-t" #'lean4-info-goto-type-definition
  "C-c C-SPC" #'lean4-info-toggle-pause
  "C-c C-s" #'lean4-info-toggle-pin)

(defun lean4-info-toggle-fold ()
  "Fold or unfold whatever is at point.
A trace node if there is one, otherwise the enclosing section, so that
TAB does the expected thing everywhere in the buffer."
  (interactive)
  (if (get-text-property (point) 'lean4-trace-children)
      (lean4-info-toggle-trace)
    (call-interactively #'magit-section-toggle)))

(defun lean4-ensure-info-buffer (buffer)
  "Create BUFFER if it does not exist.
Also choose settings used for the *Lean Goal* buffer."
  (unless (get-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (buffer-disable-undo)
      (magit-section-mode)
      (set-syntax-table lean4-syntax-table)
      (use-local-map (make-composed-keymap lean4-info-buffer-map
                                           (current-local-map)))
      ;; Hover and jumping are offered through the standard hooks, so the
      ;; user's own ElDoc and xref frontends handle presentation.
      (add-hook 'eldoc-documentation-functions
                #'lean4-info-eldoc-function nil 'local)
      (add-hook 'xref-backend-functions
                #'lean4-info-xref-backend nil 'local)
      (add-hook 'post-command-hook
                #'lean4-info-highlight-subterm nil 'local)
      (eldoc-mode 1)
      (setq buffer-read-only t)
      (lean4-info--update-header))))

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

(defun lean4-info--range (diagnostic)
  "Return the extent of raw LSP DIAGNOSTIC, preferring Lean's `fullRange'."
  (or (plist-get diagnostic :fullRange) (plist-get diagnostic :range)))

(defun lean4-info--start-line (diagnostic)
  "Return the zero-based line raw LSP DIAGNOSTIC starts on."
  (or (thread-first diagnostic lean4-info--range
                    (plist-get :start) (plist-get :line))
      0))

(defun lean4-info--end-line (diagnostic)
  "Return the zero-based line raw LSP DIAGNOSTIC ends on."
  (or (thread-first diagnostic lean4-info--range
                    (plist-get :end) (plist-get :line))
      0))

(defun lean4-info--split-diagnostics (diagnostics line)
  "Partition raw LSP DIAGNOSTICS relative to zero-based LINE.
Returns a list (ABOVE HERE BELOW).  A diagnostic is \"here\" when LINE
falls within its full range, which is how a message about a multi-line
declaration stays visible while point moves through it."
  (let (above here below)
    (dolist (diagnostic diagnostics)
      (cond
       ((< (lean4-info--end-line diagnostic) line) (push diagnostic above))
       ((<= (lean4-info--start-line diagnostic) line) (push diagnostic here))
       (t (push diagnostic below))))
    (list (nreverse above) (nreverse here) (nreverse below))))

;;;; Rendering

(defun lean4-info--fontify-string (text)
  "Return TEXT fontified as Lean source, with inaccessible names dimmed.
Used for goals arriving as plain text.  Interactive goals are already
propertized by `lean4-render', which dims those names itself."
  (with-temp-buffer
    (delay-mode-hooks (lean4-info-mode))
    (insert text)
    (font-lock-ensure)
    (when lean4-highlight-inaccessible-names
      (goto-char (point-min))
      ;; Lean marks a hypothesis you may not name with a dagger.  Drop the
      ;; dagger and say the same thing with a face.
      (while (re-search-forward "\\(\\sw+\\)✝\\([¹²³⁴-⁹⁰]*\\)" nil t)
        (replace-match
         (propertize (concat (match-string-no-properties 1)
                             (match-string-no-properties 2))
                     'font-lock-face 'font-lock-comment-face)
         'fixedcase 'literal)))
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
          (let* ((start (plist-get (plist-get diagnostic :range) :start))
                 (line (1+ (or (plist-get start :line) 0)))
                 (column (or (plist-get start :character) 0))
                 (message (plist-get diagnostic :message)))
            (insert-text-button
             (format "%d:%d:" line column)
             'action #'lean4-info--error-button-action
             'button-data (list buffer line column)
             'face 'magit-section-heading
             'help-echo "mouse-2: visit this file, line and column")
            (insert "\n"
                    ;; Plain diagnostics carry a string; interactive ones
                    ;; carry a tree, whose terms and traces render live.
                    (if (stringp message)
                        message
                      (lean4-render-message message nil
                                            lean4-info--trace-expansion))
                    "\n")))))))

(defun lean4-info--add-visibility-indicators ()
  "Draw the fold indicators on the sections just inserted.

`magit-section' updates them only from `magit-section-show' and
`magit-section-hide', so a section that has never been toggled carries
no indicator: the buffer looks as though nothing folds until something
is folded, after which that one section gains a chevron and the rest
still do not."
  (when (and (fboundp 'magit-section-maybe-update-visibility-indicator)
             (bound-and-true-p magit-root-section))
    (letrec ((walk
              (lambda (section)
                (magit-section-maybe-update-visibility-indicator section)
                (dolist (child (oref section children))
                  (funcall walk child)))))
      (funcall walk magit-root-section))))

(defun lean4-info-buffer-redisplay (&optional force)
  "Re-render the Lean info buffer from the last goals and diagnostics.

Does nothing unless the info buffer is currently being displayed, unless
FORCE is non-nil.  Forcing is for commands invoked from inside the info
buffer itself, such as unfolding a trace: the usual check requires the
Lean buffer to be the selected one, which it is not in that case."
  (when (or force (lean4-info-buffer-active lean4-info-buffer-name))
    (let* ((deactivate-mark)            ; keep transient mark
           (inhibit-read-only t)
           (buffer (current-buffer))
           (goals lean4-goals)
           (term-goal lean4-term-goal)
           ;; Carried across so that ElDoc and xref, which run in the info
           ;; buffer, can make RPC calls about what is displayed there.
           (handle lean4-info--handle)
           (line (1- (line-number-at-pos nil 'absolute)))
           (diagnostics
            (sort (or (seq-remove #'lean4-diagnostics-silent-p
                                  lean4-info--diagnostics)
                      ;; No RPC: recover the raw objects Eglot stashed on
                      ;; the Flymake diagnostics.
                      (delq nil (mapcar #'lean4-diagnostic-lsp-data
                                        (flymake-diagnostics))))
                  (lambda (a b) (< (lean4-info--end-line a)
                                   (lean4-info--end-line b))))))
      (pcase-let ((`(,above ,here ,below)
                   (lean4-info--split-diagnostics diagnostics line)))
        (with-current-buffer lean4-info-buffer-name
          (setq lean4-info--handle handle
                lean4-info--source-buffer buffer)
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (when goals
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body
                  (if (eq goals 'accomplished)
                      (insert "goals accomplished\n\n")
                    (insert goals "\n\n")))))
            (when term-goal
              (magit-insert-section (magit-section 'term-goal)
                (magit-insert-heading "Expected type:")
                (magit-insert-section-body
                  (insert term-goal "\n"))))
            (lean4-info--mk-message-section
             'errors-here "Messages here:" here buffer)
            (lean4-info--mk-message-section
             'errors-below "Messages below:" below buffer)
            (lean4-info--mk-message-section
             'errors-above "Messages above:" above buffer))
          (lean4-info--add-visibility-indicators))))))

;;;; Refresh

(defcustom lean4-info-debounce-delay 0.1
  "Seconds of quiet before the info buffer is re-rendered.
`magit-section' rendering is not cheap, and point moves in bursts."
  :group 'lean4-info
  :type 'number)

(defvar lean4-info--debounce-timer nil)

(defun lean4-info-buffer-redisplay-debounced ()
  "Update the info buffer for point's new position, coalescing rapid moves.

Two things have to happen, and only one of them is cheap.  Re-rendering
places the messages for the line point is now on, and can be done at
once.  The goals belong to the position, though, and have to be fetched:
without that the display keeps showing whatever it was opened on, which
looks like a buffer that has stopped working."
  (when (timerp lean4-info--debounce-timer)
    (cancel-timer lean4-info--debounce-timer))
  (let ((buffer (current-buffer)))
    (setq lean4-info--debounce-timer
          ;; A plain timer, not an idle one.  Cancelling and rescheduling on
          ;; each call already coalesces a burst of movement, so idleness
          ;; buys nothing -- and idle timers depend on Emacs actually going
          ;; idle, which it does not do under --batch, making this
          ;; untestable and its behaviour dependent on what ran before.
          (run-at-time
           lean4-info-debounce-delay nil
           (lambda ()
             (setq lean4-info--debounce-timer nil)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lean4-info-buffer-redisplay)
                 (lean4-info-buffer-refresh))))))))

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

;;;; Interactive goals
;;
;; `$/lean/plainGoal' returns text.  `Lean.Widget.getInteractiveGoals'
;; returns the same goal as a tree in which every subterm is labelled, which
;; is what makes hovering and jumping possible.  Lean's own protocol notes
;; describe the plain requests as being there for editors that cannot do
;; this, so the interactive path is the one to prefer where it works.

(defcustom lean4-info-interactive t
  "Whether to fetch goals over Lean's interactive RPC.
When nil, or when the server does not offer RPC, the goal display falls
back to `$/lean/plainGoal', which yields plain text: no hovering a
subterm for its type and no jumping from one to its definition."
  :group 'lean4-info
  :type 'boolean)

(defun lean4-info--goals-value (result goals render)
  "Decide what the goal display should hold for RESULT.

Three outcomes have to stay distinct, because they mean different
things to the reader: nil when point is not inside a proof at all and
the section should be absent; `accomplished' when Lean returned a proof
state with nothing left to prove; and the text RENDER makes of GOALS
otherwise."
  (cond ((null result) nil)
        ((seq-empty-p goals) 'accomplished)
        (t (funcall render goals))))

(defun lean4-info--release-refs ()
  "Give back the references held by the goals being replaced."
  (when (and lean4-info--handle lean4-info--refs)
    (lean4-rpc-release (lean4-rpc-handle-session lean4-info--handle)
                       lean4-info--refs))
  (setq lean4-info--refs nil))

(defun lean4-info--adopt (rendered refs)
  "Take ownership of REFS, which belong to the goals RENDERED.
Returns RENDERED so this can wrap a render call."
  (setq lean4-info--refs (append refs lean4-info--refs))
  rendered)

(defun lean4-info--refresh-interactive (generation)
  "Fetch and render interactive goals; drop replies older than GENERATION."
  (let ((handle (lean4-rpc-open))
        (buffer (current-buffer)))
    (lean4-info--release-refs)
    (setq lean4-info--handle handle)
    (cl-flet ((receive (setter collect)
                (lambda (result)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq generation lean4-info--generation)
                        (lean4-info--adopt nil (funcall collect result))
                        (funcall setter result)
                        (lean4-info-buffer-redisplay)))))))
      (lean4-rpc-get-interactive-goals
       handle
       (receive (lambda (result)
                  (setq lean4-goals (lean4-info--goals-value
                                     result (plist-get result :goals)
                                     #'lean4-render-goals)))
                (lambda (result)
                  (seq-mapcat
                   (lambda (goal)
                     (lean4-render-collect-refs (plist-get goal :type)))
                   (plist-get result :goals) #'list))))
      (lean4-rpc-get-interactive-term-goal
       handle
       (receive (lambda (result)
                  (setq lean4-term-goal (lean4-render-term-goal result)))
                (lambda (result)
                  (lean4-render-collect-refs (plist-get result :type)))))
      ;; Interactive diagnostics carry the fields Lean never pushes:
      ;; `isSilent' and `leanTags', and so the report that a proof is
      ;; complete.  Fetched here rather than from a notification handler
      ;; because this is the one place already holding an RPC handle.
      (lean4-rpc-get-interactive-diagnostics
       handle
       (lambda (diagnostics)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (eq generation lean4-info--generation)
               (setq lean4-info--diagnostics (append diagnostics nil))
               (lean4-diagnostics-update-markers diagnostics)
               (lean4-info-buffer-redisplay)))))
       #'ignore))))

(defun lean4-info--refresh-plain (server generation)
  "Fetch goals as plain text from SERVER, dropping replies before GENERATION."
  (lean4-info--request
   server :$/lean/plainGoal generation
   (lambda (result)
     (setq lean4-goals
           (lean4-info--goals-value
            result (plist-get result :goals)
            (lambda (goals)
              (mapconcat #'lean4-info--fontify-string
                         (append goals nil) "\n\n"))))))
  (lean4-info--request
   server :$/lean/plainTermGoal generation
   (lambda (result)
     (setq lean4-term-goal
           (when-let* ((goal (plist-get result :goal)))
             (lean4-info--fontify-string goal))))))

(defun lean4-info--refresh-here ()
  "Fetch the goals at point into the current buffer's state."
  (when-let* ((server (eglot-current-server))
              (generation (cl-incf lean4-info--generation)))
    (if lean4-info-interactive
        ;; A server too old for RPC, or a file worker that has just
        ;; died, should degrade to plain goals rather than blank out.
        (condition-case nil
            (lean4-info--refresh-interactive generation)
          (error (lean4-info--refresh-plain server generation)))
      (lean4-info--refresh-plain server generation))))

(defun lean4-info-buffer-refresh ()
  "Refresh the goals shown in the Lean info buffer.

Does nothing while paused, and nothing unless the info buffer is on
display: the requests are not free, and Lean is slow to answer them
while a file is elaborating.  When pinned, the goals are re-fetched at
the pinned location rather than at point."
  (unless lean4-info-paused
    (if-let* ((pin lean4-info--pin))
        (let ((buffer (marker-buffer pin)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (save-excursion
                (goto-char pin)
                (lean4-info--refresh-here)))))
      (when (lean4-info-buffer-active lean4-info-buffer-name)
        (lean4-info--refresh-here)))))

;;;; Pinning and pausing
;;
;; Two different needs that look alike.  Pausing stops the display updating
;; at all, so a goal can be read while point wanders; pinning ties it to one
;; location but keeps it updating, which is what you want while editing the
;; tactic above the goal you are watching.  Both announce themselves in the
;; header line, because a goal buffer that has quietly stopped following
;; point is indistinguishable from a broken one.

(defface lean4-info-button
  '((t :inherit mode-line-buffer-id))
  "Face for the clickable controls in the goal display's header line."
  :group 'lean4-info)

(defun lean4-info--button (label help command)
  "Return LABEL as a header-line button running COMMAND, described by HELP."
  (propertize
   label
   'face 'lean4-info-button
   'mouse-face 'highlight
   'help-echo help
   'keymap (let ((map (make-sparse-keymap)))
             ;; `header-line' has to be in the event prefix: a header-line
             ;; click is not a plain mouse-1.
             (keymap-set map "<header-line> <mouse-1>" command)
             map)))

(defun lean4-info--pinned-description ()
  "Describe where the display is pinned, or nil if it is not."
  (when lean4-info--pin
    (let ((source (marker-buffer lean4-info--pin)))
      (if (buffer-live-p source)
          (format "%s:%d" (buffer-name source)
                  (with-current-buffer source
                    (line-number-at-pos lean4-info--pin)))
        "a closed buffer"))))

(defun lean4-info--update-header ()
  "Rebuild the goal display's header line.

Always present, because it carries the controls as well as the state:
VS Code puts pin and pause where you are already looking, and a control
that only appears once you have found the keybinding is not much of a
control."
  (when-let* ((buffer (get-buffer lean4-info-buffer-name)))
    (with-current-buffer buffer
      (setq header-line-format
            (list
             " "
             (lean4-info--button
              (if lean4-info--pin "[Unpin]" "[Pin]")
              (if lean4-info--pin
                  "mouse-1: follow point again"
                "mouse-1: keep showing this position")
              #'lean4-info-toggle-pin)
             " "
             (lean4-info--button
              (if lean4-info-paused "[Unpause]" "[Pause]")
              (if lean4-info-paused
                  "mouse-1: start updating again"
                "mouse-1: stop updating")
              #'lean4-info-toggle-pause)
             (cond
              (lean4-info-paused (propertize "  Paused" 'face 'warning))
              (lean4-info--pin
               (propertize (format "  Pinned to %s"
                                   (lean4-info--pinned-description))
                           'face 'warning))
              (t ""))))
      (force-mode-line-update))))

;;;###autoload
(defun lean4-info-toggle-pause ()
  "Pause or unpause the goal display.
While paused it keeps showing whatever it last showed, so a goal can be
read while point moves elsewhere.  The names follow Lean's own: VS Code
calls these commands pause and unpause."
  (interactive)
  (setq lean4-info-paused (not lean4-info-paused))
  (lean4-info--update-header)
  (unless lean4-info-paused
    (lean4-info-buffer-refresh))
  (message "Lean goal display %s"
           (if lean4-info-paused "paused" "unpaused")))

;;;###autoload
(defun lean4-info-toggle-pin ()
  "Pin the goal display to point, or unpin it.

Unlike pausing, a pinned display keeps updating: it follows the goal at
the pinned location as the file is edited, which is what makes it useful
for watching one goal while working on the tactic above it."
  (interactive)
  (if lean4-info--pin
      (progn
        (set-marker lean4-info--pin nil)
        (setq lean4-info--pin nil)
        (message "Lean goal display unpinned"))
    (unless (derived-mode-p 'lean4-mode)
      (user-error "Not in a Lean buffer"))
    ;; A marker rather than a position: the point of pinning is to watch a
    ;; location while editing around it, which moves it.
    (setq lean4-info--pin (copy-marker (point)))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (display-buffer lean4-info-buffer-name)
    (message "Lean goal display pinned to line %d"
             (line-number-at-pos)))
  (lean4-info--update-header)
  (lean4-info-buffer-refresh))

;;;; Subterms
;;
;; Everything below works off the two text properties `lean4-render' leaves
;; on the goal: `lean4-info' names the subterm to the server, and
;; `lean4-subexpr-pos' says where it sits in the term tree.
;;
;; Nothing here defines a user interface.  Hovering goes through ElDoc and
;; jumping goes through xref, so whatever the user already runs -- the echo
;; area or eldoc-box, the xref buffer or consult -- is what they get.

(defface lean4-info-subterm
  '((t :inherit highlight))
  "Face marking the extent of the subterm under point."
  :group 'lean4-info)

(defvar-local lean4-info--subterm-overlay nil)

(defun lean4-info-subterm-bounds (&optional position)
  "Return the bounds of the subterm at POSITION as a cons, or nil.

Grows outward while the neighbouring text belongs to a subterm of which
this one is an ancestor, which is what makes the highlight cover the
whole of `1 + 1' when point is on the `+' rather than just the operator."
  (let ((position (or position (point))))
    (when-let* ((path (get-text-property position 'lean4-subexpr-pos)))
      (let ((start position)
            (end position))
        (while (and (> start (point-min))
                    (lean4-render-subexpr-ancestor-p
                     path (get-text-property (1- start) 'lean4-subexpr-pos)))
          (setq start (1- start)))
        (while (and (< end (point-max))
                    (lean4-render-subexpr-ancestor-p
                     path (get-text-property end 'lean4-subexpr-pos)))
          (setq end (1+ end)))
        (cons start end)))))

(defun lean4-info-highlight-subterm ()
  "Highlight the subterm under point in the info buffer."
  (when lean4-info--subterm-overlay
    (delete-overlay lean4-info--subterm-overlay))
  (when-let* ((bounds (lean4-info-subterm-bounds)))
    (setq lean4-info--subterm-overlay
          (make-overlay (car bounds) (cdr bounds)))
    (overlay-put lean4-info--subterm-overlay 'face 'lean4-info-subterm)))

(defun lean4-info--live-handle ()
  "Return a usable RPC handle for the goals on display, or nil.

The handle captured when the goals were fetched is preferred, but a
server can exit between a goal being displayed and the reader hovering
over it, so fall back to opening a fresh one against the Lean buffer
these goals came from."
  (cond ((lean4-rpc-handle-live-p lean4-info--handle) lean4-info--handle)
        ((buffer-live-p lean4-info--source-buffer)
         (with-current-buffer lean4-info--source-buffer
           (when (eglot-current-server)
             (setq-local lean4-info--handle nil)
             (ignore-errors (lean4-rpc-open)))))))

(defun lean4-info-eldoc-function (callback &rest _)
  "Report the type of the subterm under point through CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((info (get-text-property (point) 'lean4-info))
              (handle (lean4-info--live-handle)))
    (lean4-rpc-info-to-interactive
     handle info
     (lambda (popup)
       (let ((type (lean4-render-tagged-text (plist-get popup :type)))
             (documentation (plist-get popup :doc)))
         (funcall callback
                  (concat type
                          (when (and documentation
                                     (not (string-empty-p documentation)))
                            (concat "\n\n" documentation)))))))
    ;; Tell ElDoc an answer is coming; it is asynchronous.
    t))

;;;; Traces

(defun lean4-info-toggle-trace ()
  "Fold or unfold the trace node at point.

Children that were not sent with the message are fetched on demand, the
way VS Code fetches them: a `simp' trace on a real proof can be enormous
and Lean does not send it until something asks."
  (interactive)
  (let ((children (get-text-property (point) 'lean4-trace-children))
        (path (get-text-property (point) 'lean4-trace-path))
        (open (get-text-property (point) 'lean4-trace-open)))
    ;; Presence is tested on the children, not the path: a trace at the root
    ;; of a message has the empty path, which is nil.
    (unless children
      (user-error "No trace at point"))
    (cond
     (open
      (remhash path lean4-info--trace-expansion)
      (lean4-info--redisplay-source))
     ((eq (car children) 'strict)
      (puthash path (cdr children) lean4-info--trace-expansion)
      (lean4-info--redisplay-source))
     (t
      (let ((handle (lean4-info--live-handle))
            (buffer (current-buffer)))
        (unless handle
          (user-error "No Lean server to expand this trace"))
        (lean4-rpc-lazy-trace-children
         handle (cdr children)
         (lambda (result)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (puthash path result lean4-info--trace-expansion)
               (lean4-info--redisplay-source))))
         (lambda (error)
           (message "Could not expand trace: %S" error))))))))

(defun lean4-info--redisplay-source ()
  "Re-render the info buffer from the Lean buffer that populated it."
  (when (buffer-live-p lean4-info--source-buffer)
    (let ((position (point)))
      (with-current-buffer lean4-info--source-buffer
        (lean4-info-buffer-redisplay 'force))
      ;; Redisplay rebuilds the buffer, so put point back where the
      ;; reader left it rather than at the top.
      (goto-char (min position (point-max))))))

;;;; xref

(defun lean4-info--xref-location (kind)
  "Return an `xref-item' for the subterm under point, of KIND.
KIND is \"definition\", \"declaration\" or \"type\"."
  (when-let* ((info (get-text-property (point) 'lean4-info))
              (handle (lean4-info--live-handle))
              (locations (lean4-rpc-call-sync
                          handle "Lean.Widget.getGoToLocation"
                          (list :kind kind :info info))))
    (mapcar
     (lambda (location)
       (let* ((uri (plist-get location :targetUri))
              (range (or (plist-get location :targetSelectionRange)
                         (plist-get location :targetRange)))
              (start (plist-get range :start))
              (file (lean4--uri-to-path uri)))
         (xref-make (or file uri)
                    (xref-make-file-location
                     file
                     (1+ (or (plist-get start :line) 0))
                     (or (plist-get start :character) 0)))))
     (append locations nil))))

(defun lean4-info-xref-backend ()
  "Return the xref backend for the Lean info buffer."
  (when (get-text-property (point) 'lean4-info) 'lean4-info))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql lean4-info)))
  "Return the subterm under point, as text for xref to echo."
  (when-let* ((bounds (lean4-info-subterm-bounds)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(cl-defmethod xref-backend-definitions ((_backend (eql lean4-info)) _identifier)
  "Return where the subterm under point is defined."
  (lean4-info--xref-location "definition"))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql lean4-info)))
  "Return nil: there is nothing to complete over in a goal display."
  nil)

(defun lean4-info-goto-type-definition ()
  "Jump to the definition of the type of the subterm under point."
  (interactive)
  (if-let* ((items (lean4-info--xref-location "type")))
      (xref-pop-to-location (car items))
    (user-error "No type definition for the subterm at point")))

(defcustom lean4-info-auto-open t
  "Whether to show the goal display when a Lean file is opened.

VS Code opens its InfoView automatically, and the proof state is the
main thing one wants to see while writing Lean, so this matches it.

The buffer is shown, not selected, and it goes through `display-buffer',
so `display-buffer-alist' governs where it lands.  To put it in a side
window on the right, for instance:

  (add-to-list \\='display-buffer-alist
               \\='(\"\\\\`\\\\*Lean Goal\\\\*\\\\\\='\"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)))"
  :group 'lean4-info
  :type 'boolean)

(defun lean4-info-open ()
  "Show the goal display without selecting it."
  (interactive)
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (display-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

(defun lean4-info--maybe-auto-open ()
  "Show the goal display for a new Lean buffer, if that is wanted.

Deferred rather than run from the mode body: at that point `find-file'
has not finished, so the window configuration is still in flux and the
Lean buffer itself may not be displayed yet.

A plain timer, not an idle one: idle timers need Emacs to actually go
idle, which never happens under \\=--batch, and that would make this
untestable."
  (when lean4-info-auto-open
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda ()
         (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
           (with-current-buffer buffer
             (lean4-info-open))))))))

;;;###autoload
(defun lean4-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (lean4-toggle-info-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

(provide 'lean4-info)
;;; lean4-info.el ends here
