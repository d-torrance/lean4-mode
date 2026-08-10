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

(cl-defstruct (lean4-info-pin (:constructor lean4-info--pin-create))
  "A position the goal display goes on showing, whatever point does."
  marker                                ; where, in which Lean buffer
  goals                                 ; rendered, `accomplished', or nil
  term-goal                             ; rendered expected type, or nil
  handle                                ; the RPC handle it was fetched through
  refs                                  ; server-side references it owns
  paused                                ; held as it is, if non-nil
  id)                                   ; tells its section from the others

(defvar lean4-info--pin-counter 0
  "Source of the number that tells one pin\='s section from another.")

(defvar lean4-info--pins nil
  "Pinned positions, in the order they were pinned.

Not buffer-local: a pin is a marker, which knows its own buffer, and
they stay on display while the reader moves to another file -- which is
the point of pinning one.")

(defvar lean4-info--pinned-at nil
  "Where the most recent pin was made, while point is still there.

Pinning happens at point, so the position pinned and the position being
followed are the same one until point moves; showing both would show it
twice.  Cleared as soon as point is elsewhere, after which returning to
a pinned position does show both, one pinned and one followed -- which
is what VS Code does.")

(defun lean4-info--pin-at (&optional position buffer)
  "Return the pin at POSITION in BUFFER, defaulting to point here."
  (let ((position (or position (point)))
        (buffer (or buffer (current-buffer))))
    (seq-find (lambda (pin)
                (let ((marker (lean4-info-pin-marker pin)))
                  (and (eq (marker-buffer marker) buffer)
                       (= (marker-position marker) position))))
              lean4-info--pins)))

(defun lean4-info--pin-release (pin)
  "Give back what PIN holds: its server-side references and its marker."
  (lean4-info--pin-give-back-refs pin)
  (set-marker (lean4-info-pin-marker pin) nil))

(defvar lean4-info--rendered nil
  "What the info buffer was last built from.
Compared before rebuilding: the buffer is erased and re-inserted from
scratch, which is visible as a flicker, and the server can republish
diagnostics repeatedly without anything the reader would see changing.")

(defvar-local lean4-info--diagnostics nil
  "Interactive diagnostics for this buffer, as raw LSP plists.
Nil until the first RPC refresh, and while running without RPC, in
which case the display falls back to what Flymake holds.")

(defvar lean4-info--trace-generation 0
  "Bumped whenever a trace is folded or unfolded.
`lean4-info--rendered' cannot compare the expansion table itself: `equal'
compares hash tables by identity, so unfolding a trace looked like
nothing had changed and the display was never rebuilt.")

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
  ;; `M-.' comes free from the xref backend; RET goes to whatever is at
  ;; point, which on a term is the same thing.
  "RET" #'lean4-info-return
  "TAB" #'lean4-info-toggle-fold
  "C-c C-t" #'lean4-info-goto-type-definition
  "C-c C-SPC" #'lean4-info-toggle-pause
  "C-c C-s" #'lean4-info-toggle-pin
  "C-c C-o" #'lean4-info-toggle-message-order
  "C-c C-a" #'lean4-info-toggle-all-messages-pause
  "C-c C-g" #'lean4-info-refresh-paused)

(defun lean4-info--pin-at-point ()
  "Return the pin whose section point is in, or nil."
  (get-text-property (point) 'lean4-info-pin))

(defun lean4-info-return ()
  "Act on whatever point is on.

On a control, what clicking it would do; on a message, go to the place
it was reported for; on a term, go to the definition of that term.  TAB
folds and RET acts on the thing at point, which is the division
`magit-section' uses.

`mouse-1' does the same, in the same places: `lean4-info--act-at' is
what both of them ask."
  (interactive)
  (unless (lean4-info--act-at (point))
    (user-error "Nothing at point to go to")))


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

(defun lean4-info--range (diagnostic)
  "Return the extent of raw LSP DIAGNOSTIC, preferring Lean's `fullRange'."
  (or (plist-get diagnostic :fullRange) (plist-get diagnostic :range)))

(defun lean4-info--start-line (diagnostic)
  "Return the zero-based line raw LSP DIAGNOSTIC starts on."
  (or (thread-first diagnostic lean4-info--range
                    (plist-get :start) (plist-get :line))
      0))

(defun lean4-info--start-column (diagnostic)
  "Return the zero-based column raw LSP DIAGNOSTIC starts at."
  (or (thread-first diagnostic lean4-info--range
                    (plist-get :start) (plist-get :character))
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

(defun lean4-info--align-right (left right)
  "Return LEFT and RIGHT on one line, with RIGHT against the window edge.

A stretch space does the aligning, so it holds however the window is
resized.  The width is measured in columns rather than characters
because the glyphs need not be single-width, and two columns of margin
are left rather than one: the right edge is where the fringe or a scroll
bar begins, and an emoji can be drawn wider than `string-width' accounts
for, so a control flush against it bleeds off the side."
  (if (string-empty-p right)
      left
    (concat left
            (propertize " " 'display
                        `(space :align-to
                                (- right ,(+ (string-width right) 2))))
            right)))

(defun lean4-info--goto-position (line column)
  "Put point on LINE at COLUMN in the current buffer.
LINE is counted from one and COLUMN from zero, as a diagnostic reports
them.  A column past the end of its line is clamped rather than
signalling: the file can have been edited since the message was made."
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char (min column (- (line-end-position) (point)))))

(defun lean4-info--error-button-action (data)
  "Jump to the source location a diagnostic button points at.
DATA is the button's `button-data', a list (BUFFER LINE COLUMN) with
LINE counted from one and COLUMN from zero."
  (pcase-let ((`(,buffer ,line ,column) data))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer)
      (lean4-info--goto-position line column))))

(defconst lean4-info--indent 2
  "Columns each level of the goal display is set in by.")

(defvar lean4-info--level 0
  "How far in the section being inserted sits.")

(defun lean4-info--prefix ()
  "Return the indentation for the section being inserted."
  (make-string (* lean4-info--indent lean4-info--level) ?\s))

(defmacro lean4-info--indented (&rest body)
  "Insert whatever BODY inserts one level further in."
  (declare (indent 0) (debug t))
  `(let ((lean4-info--level (1+ lean4-info--level)))
     ,@body))

(defun lean4-info--indent-string (string prefix)
  "Return STRING with PREFIX before each of its lines.

Built from substrings, which carry their text properties with them: the
goal text is propertized character by character, and that is what
`lean4-render', ElDoc and xref read back out of it."
  (if (string-empty-p prefix)
      string
    (let ((parts nil)
          (start 0))
      (while (string-match "\n" string start)
        (push (concat prefix (substring string start (match-end 0))) parts)
        (setq start (match-end 0)))
      (unless (= start (length string))
        (push (concat prefix (substring string start)) parts))
      (apply #'concat (nreverse parts)))))

(defun lean4-info--insert (&rest strings)
  "Insert STRINGS, indented to the level being inserted at."
  (insert (lean4-info--indent-string (apply #'concat strings)
                                     (lean4-info--prefix))))

(defun lean4-info--heading-text (string)
  "Return STRING as a heading at the level being inserted at."
  (concat (lean4-info--prefix) string))


(defmacro lean4-info--marking-pin (pin &rest body)
  "Evaluate BODY, marking everything it inserts as belonging to PIN.

Over the whole section, not merely its heading: a key pressed anywhere
inside a pinned section should act on that section, which is the only
way \\[lean4-info-toggle-pin] can mean anything in a display holding
several pins."
  (declare (indent 1) (debug (form body)))
  `(let ((start (point)))
     (prog1 (progn ,@body)
       (put-text-property start (point) 'lean4-info-pin ,pin))))

(defun lean4-info--marker-line (pin)
  "Return the zero-based line PIN sits on."
  (let* ((marker (lean4-info-pin-marker pin))
         (source (marker-buffer marker)))
    (if (buffer-live-p source)
        (with-current-buffer source
          (1- (line-number-at-pos marker 'absolute)))
      0)))

(defun lean4-info--insert-position (goals term-goal here buffer)
  "Insert what a position has to say: GOALS, TERM-GOAL and HERE.
BUFFER is the Lean buffer the messages belong to."
  ;; Say so, rather than leaving a bare heading: outside a proof there is
  ;; nothing to report, and a section that goes blank reads like one that
  ;; has stopped working.  VS Code words it this way.  This is about the
  ;; position, so the file's own messages do not count -- the notice and
  ;; the "All messages" section can and should appear together.
  (unless (or goals term-goal here)
    (lean4-info--insert (propertize "No info found.\n" 'face 'shadow)))
  (when goals
    (magit-insert-section (lean4-info-section 'goals)
      (magit-insert-heading (lean4-info--heading-text "Goals:"))
      (magit-insert-section-body
        (if (eq goals 'accomplished)
            (lean4-info--insert "goals accomplished\n\n")
          (lean4-info--insert goals "\n\n")))))
  (when term-goal
    (magit-insert-section (lean4-info-section 'term-goal)
      (magit-insert-heading (lean4-info--heading-text "Expected type:"))
      (magit-insert-section-body
        (lean4-info--insert term-goal "\n"))))
  (lean4-info--mk-message-section
   'messages (lean4-info--messages-caption "Messages" here) here buffer))

(defun lean4-info--insert-message (diagnostic buffer)
  "Insert DIAGNOSTIC as a section of its own, headed by where in BUFFER it is.

A section rather than plain text so that one long message -- a `simp'
trace, a type mismatch between two large terms -- can be folded away
without folding the rest, and so that it gets a chevron saying it can
be.  The heading names the file as well as the line and column: the
position alone read as a bare pair of numbers."
  (let* ((start (plist-get (plist-get diagnostic :range) :start))
         (line (1+ (or (plist-get start :line) 0)))
         (column (or (plist-get start :character) 0))
         (message (plist-get diagnostic :message))
         (place (format "%s:%d:%d" (buffer-name buffer) line column)))
    (magit-insert-section (lean4-info-section (list 'message place))
      (magit-insert-heading
        ;; The place is on the whole heading, not just the label: RET goes
        ;; there from anywhere on the line.  The label itself is plain
        ;; text, since clicking a heading folds it, so the one thing that
        ;; goes there by mouse is the one control that says so.
        (lean4-info--heading-text
         (propertize
         (lean4-info--align-right
          (propertize place 'face 'lean4-info-location)
          (lean4-info--goto-button buffer line column))
          'lean4-info-position (list buffer line column))))
      (magit-insert-section-body
        (lean4-info--insert
         ;; Plain diagnostics carry a string; interactive ones carry a
         ;; tree, whose terms and traces render live.
         (if (stringp message)
             message
           (lean4-render-message message nil lean4-info--trace-expansion))
         "\n")))))

(defun lean4-info--mk-message-section (value caption messages buffer)
  "Add a section with id VALUE, caption CAPTION and contents MESSAGES.
Each message becomes a foldable section of its own, headed by the place
in BUFFER it belongs to.  Nothing is inserted when MESSAGES is empty."
  (when messages
    (magit-insert-section (magit-section value)
      (magit-insert-heading (lean4-info--heading-text caption))
      (magit-insert-section-body
        (lean4-info--indented
          (dolist (diagnostic messages)
            (lean4-info--insert-message diagnostic buffer)))))))

(defmacro lean4-info--keeping-position (&rest body)
  "Run BODY, which rebuilds the current buffer, without moving the view.

Rebuilding erases the buffer and inserts it again, which leaves point
and every window\='s start at the end of the new text.  The buffer is
rebuilt whenever the goal changes, so without this a reader part-way
down a long goal would be thrown to the bottom, and clicking a control
-- which rebuilds to redraw the control itself -- would scroll the
display away from whatever prompted the click."
  (declare (indent 0) (debug t))
  `(let* ((windows (get-buffer-window-list (current-buffer) nil t))
          (starts (mapcar #'window-start windows))
          (spot (point)))
     (prog1 (progn ,@body)
       (goto-char (min spot (point-max)))
       (cl-mapc (lambda (window start)
                  ;; Start first, then point: setting the start can move
                  ;; point to keep it on screen, which is the opposite of
                  ;; what is wanted here.
                  (set-window-start window (min start (point-max)) t)
                  (set-window-point window (min spot (point-max))))
                windows starts))))

(defconst lean4-info--severity-badges
  '((1 ("⊗" "✖" "×") "E" error)
    (2 ("⚠" "△" "!") "W" warning)
    (3 ("ⓘ" "ℹ" "i") "I" shadow)
    (4 ("ⓗ" "☞" "?") "H" shadow))
  "How each LSP severity is counted in a message section\='s heading.
Each entry is (SEVERITY CANDIDATES FALLBACK FACE).  The candidates are
tried in turn against the frame, as the pin and pause controls are: a
badge that renders as a row of boxes is worse than a letter.")

(defun lean4-info--severity-badge (diagnostics)
  "Return a count of DIAGNOSTICS by severity, or nil if there are none.

Reads as VS Code\='s does -- \"1 ⊗ 5 ⓘ\" for one error and five notes --
so the shape of a file\='s output can be taken in without unfolding the
section.  Severities nobody has are left out rather than shown as zero."
  (let (parts)
    (pcase-dolist (`(,severity ,candidates ,fallback ,face)
                   lean4-info--severity-badges)
      (let ((count (seq-count
                    (lambda (diagnostic)
                      (= (lean4-diagnostics-severity diagnostic) severity))
                    diagnostics)))
        (when (> count 0)
          (push (concat (number-to-string count) " "
                        (propertize (lean4-info--glyph nil candidates fallback)
                                    'face face))
                parts))))
    (when parts (string-join (nreverse parts) "  "))))

(defvar lean4-info-all-messages-paused nil
  "Non-nil while the file's message list is held as it was.")

(defvar lean4-info--all-messages-frozen nil
  "The message list being held while paused.")

;;;###autoload
(defun lean4-info-toggle-all-messages-pause ()
  "Hold the file's message list as it is, or let it follow the file again.

Its own pause, separate from the one in the position heading: the goal
at point and the list of everything in the file are worth stopping
independently, and VS Code gives each its own control."
  (interactive)
  (setq lean4-info-all-messages-paused (not lean4-info-all-messages-paused))
  (lean4-info--redisplay-source)
  (message "All messages %s"
           (if lean4-info-all-messages-paused "paused" "unpaused")))

(defcustom lean4-info-message-order 'point
  "How the file's messages are ordered in the goal display.

`point' puts the ones nearest point first; `location' puts them in the
order they appear in the file.  Nearest first is the default because it
is VS Code's, whose manual has All Messages \"sorted by their proximity
to the text cursor by default\" -- under the name Emacs uses for the
same thing.  VS Code offers the same choice as Infoview: Message
Order."
  :group 'lean4-info
  :type '(choice (const :tag "By position in the file" location)
                 (const :tag "By nearness to point" point)))

(defcustom lean4-info-sort-icon nil
  "Control for the order the file's messages are listed in.
Nil means pick whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defun lean4-info-sort-glyph ()
  "Return the control for the order the messages are listed in."
  ;; One glyph for both orders, as in VS Code.  Neither order is the
  ;; action and neither is a state the reader has to be warned about, so
  ;; the face carries which is in force and the tooltip says what a click
  ;; would change it to.
  (lean4-info--glyph lean4-info-sort-icon '("⇅" "↕") "S"))

;;;###autoload
(defun lean4-info-toggle-message-order ()
  "Order the file's messages by position, or by nearness to point."
  (interactive)
  (setq lean4-info-message-order
        (if (eq lean4-info-message-order 'point) 'location 'point))
  (lean4-info--redisplay-source)
  (message "Messages ordered %s"
           (if (eq lean4-info-message-order 'point)
               "by nearness to point"
             "by position in the file")))

(defun lean4-info--sort-messages (diagnostics line)
  "Return DIAGNOSTICS in the order to show them, relative to LINE.

Ordered by where each message starts rather than where it ends, so that
two messages about the same declaration come out the way they are
written -- the completed-proof report before the trace it belongs with,
as VS Code has them."
  (let ((sorted
         (sort (copy-sequence diagnostics)
               (lambda (a b)
                 (let ((la (lean4-info--start-line a))
                       (lb (lean4-info--start-line b)))
                   (if (= la lb)
                       (< (lean4-info--start-column a)
                          (lean4-info--start-column b))
                     (< la lb)))))))
    (if (eq lean4-info-message-order 'point)
        (sort sorted (lambda (a b)
                       (< (abs (- (lean4-info--start-line a) line))
                          (abs (- (lean4-info--start-line b) line)))))
      sorted)))

(defun lean4-info--messages-caption (label diagnostics &optional controls)
  "Return LABEL as a caption, counting DIAGNOSTICS by severity after it.
CONTROLS, if given, are set hard right on the same line.

No trailing colon, unlike the other captions: the `magit-section'
package replaces
one with a count of the section\='s children, which here would follow
the badge with a second, coarser count of the same messages."
  (let ((caption (if-let* ((badge (lean4-info--severity-badge diagnostics)))
                     (format "%s (%s)" label badge)
                   label)))
    (if controls (lean4-info--align-right caption controls) caption)))

(defun lean4-info--add-visibility-indicators ()
  "Draw the fold indicators on the sections just inserted.

`magit-section' updates them only from `magit-section-show' and
`magit-section-hide', so a section that has never been toggled carries
none: the display looked as though nothing folded until something was
folded, after which that one section gained an indicator and the rest
still had none."
  (when (and (fboundp 'magit-section-maybe-update-visibility-indicator)
             (bound-and-true-p magit-root-section))
    (letrec ((walk (lambda (section)
                     (magit-section-maybe-update-visibility-indicator section)
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))))

(defun lean4-info--act-at (position)
  "Do whatever POSITION is on; return non-nil if there was anything.

The one place that says what a spot in the display means, so that RET
and `mouse-1' cannot come to disagree about it."
  (cond
   ((get-text-property position 'lean4-info-command)
    (lean4-info--run-control
     (get-text-property position 'lean4-info-command))
    t)
   ((get-text-property position 'lean4-info-position)
    (lean4-info--error-button-action
     (get-text-property position 'lean4-info-position))
    t)
   ;; Only where the server labelled a subterm.  Handing anything else to
   ;; xref sent it off to look for a tags table, which is neither here nor
   ;; anything the reader asked for.
   ((get-text-property position 'lean4-info)
    (save-excursion
      (goto-char position)
      (call-interactively #'xref-find-definitions))
    t)))

(defun lean4-info-mouse-1 (event)
  "Do whatever EVENT was clicked on, or move point there.

What RET would do in the same spot, which is what `mouse-1' does
throughout Emacs -- on a button or a link it acts, and anywhere else it
sets point.  Notably it does not fold: the goals are trees of subterms,
and clicking one is how the reader puts it under ElDoc."
  (interactive "e")
  (let ((posn (event-start event)))
    (with-selected-window (posn-window posn)
      (let ((position (posn-point posn)))
        (unless (lean4-info--act-at position)
          (goto-char position))))))

(defvar-keymap lean4-info-section-map
  :doc "Keymap over every section of the goal display.

Held by `lean4-info-section', whose sections `magit-section' then applies
it to -- composed with its own bindings rather than replacing them.
Putting a keymap on the text directly does not survive: `magit-section'
overwrites the property on a heading line whenever the section is
folded.

Only `mouse-1' is bound here.  Folding by mouse is left to whatever
`magit-section' offers -- a double click and a click on the fringe or
the margin from version 4, and nothing at all in 3.3.0 -- so that this
display folds exactly the way the reader\='s Magit does."
  "<mouse-1>" #'lean4-info-mouse-1)

(defclass lean4-info-section (magit-section)
  ((keymap :initform 'lean4-info-section-map))
  :documentation "A section of the goal display.
Carries the keymap for everything in it; see `lean4-info-section-map'.")

(defun lean4-info--diagnostics ()
  "Return the diagnostics to build the display from.

Silent ones are kept.  `isSilent' marks a message as being for the goal
display rather than for the editor -- the completed-proof report is one
-- so this is the one place they belong.  `lean4-diagnostics' keeps them
out of Flymake, which is where they would be noise."
  (or (copy-sequence lean4-info--diagnostics)
      ;; No RPC: recover the raw objects Eglot stashed on the Flymake
      ;; diagnostics.
      (delq nil (mapcar #'lean4-diagnostic-lsp-data (flymake-diagnostics)))))

(defun lean4-info--file-messages (diagnostics line)
  "Return the file's own list, drawn from DIAGNOSTICS and ordered against LINE.

Held as it was while paused.  Leaves out what Lean marked as being for
the goal display rather than the editor: the completed-proof report
belongs against the proof it is about, not in a list of everything in
the file, which is where VS Code puts it and does not."
  (if lean4-info-all-messages-paused
      lean4-info--all-messages-frozen
    (setq lean4-info--all-messages-frozen
          (lean4-info--sort-messages
           (seq-remove #'lean4-diagnostics-silent-p diagnostics)
           line))))

(defun lean4-info--render-key (goals term-goal location diagnostics all)
  "Return what the display would be built from, for comparing.

GOALS, TERM-GOAL and LOCATION describe the position being followed;
DIAGNOSTICS is every message in the file and ALL the file's own list.

The buffer is erased and rebuilt from scratch, which is visible as a
flicker, and the server can republish diagnostics repeatedly without
anything the reader would see changing."
  (list goals term-goal location diagnostics all
        lean4-info-paused
        lean4-info-message-order
        lean4-info-all-messages-paused
        lean4-info--trace-generation
        (lean4-info--following-point-p)
        (mapcar (lambda (pin)
                  (list (marker-position (lean4-info-pin-marker pin))
                        (lean4-info-pin-goals pin)
                        (lean4-info-pin-term-goal pin)
                        (lean4-info-pin-paused pin)))
                lean4-info--pins)))

(defun lean4-info--insert-pinned (pin sorted buffer)
  "Insert PIN's section, its messages taken from SORTED in BUFFER."
  (lean4-info--marking-pin pin
    (magit-insert-section (lean4-info-section
                           (list 'pinned (lean4-info-pin-id pin)))
      (magit-insert-heading
       (lean4-info--heading-text
        (lean4-info--heading
         (lean4-info--marker-location-string (lean4-info-pin-marker pin))
         (lean4-info--pin-controls pin)
         (if (lean4-info-pin-paused pin) "pinned and paused" "pinned"))))
      (lean4-info--indented
        (lean4-info--insert-position
         (lean4-info-pin-goals pin)
         (lean4-info-pin-term-goal pin)
         (cadr (lean4-info--split-diagnostics
                sorted (lean4-info--marker-line pin)))
         buffer)))))

(defun lean4-info--insert-followed (location goals term-goal here buffer)
  "Insert the section following point: LOCATION, GOALS, TERM-GOAL, HERE.
BUFFER is the Lean buffer the messages in HERE belong to."
  (magit-insert-section (lean4-info-section 'position)
    (magit-insert-heading
     (lean4-info--heading-text
      (lean4-info--heading location (lean4-info--controls)
                           (lean4-info--point-state))))
    (lean4-info--indented
      (lean4-info--insert-position goals term-goal here buffer))))

(defun lean4-info--all-messages-controls ()
  "Return the controls for the file's message list."
  (concat
   (lean4-info--button
    (lean4-info-sort-glyph)
    (if (eq lean4-info-message-order 'point)
        "mouse-1: order by position in the file"
      "mouse-1: order by nearness to point")
    #'lean4-info-toggle-message-order
    ;; Marked engaged when the order is not the default one, there being
    ;; no glyph to say which is in force.
    (eq lean4-info-message-order 'location))
   "  "
   (lean4-info--button
    (if lean4-info-all-messages-paused
        (lean4-info-resume-glyph)
      (lean4-info-pause-glyph))
    (if lean4-info-all-messages-paused
        "mouse-1: unpause the file's message list"
      "mouse-1: pause the file's message list")
    #'lean4-info-toggle-all-messages-pause
    lean4-info-all-messages-paused)))

(defun lean4-info--insert-display (location goals term-goal here sorted all
                                            buffer)
  "Insert the whole display: the pins, the followed position, the file.

LOCATION, GOALS, TERM-GOAL and HERE describe the position being
followed; SORTED is every message in BUFFER, ALL the file's own list."
  (magit-insert-section (lean4-info-section 'root)
    ;; Pinned positions first, above the one following point, as VS Code
    ;; stacks them.  Each keeps updating: a pin is a marker, so it
    ;; follows its declaration as the file is edited.  The root is left
    ;; headless, holding them all side by side -- `magit-section' refuses
    ;; to fold a root, and a heading that does not fold is worse than no
    ;; heading.
    (dolist (pin lean4-info--pins)
      (lean4-info--insert-pinned pin sorted buffer))
    (when (lean4-info--following-point-p)
      (lean4-info--insert-followed location goals term-goal here buffer))
    ;; One section for the file, as VS Code has it, rather than one above
    ;; point and one below: that split said where a message was relative
    ;; to point, which the line number in each entry already says.
    (lean4-info--mk-message-section
     'all-messages
     (lean4-info--messages-caption "All messages" all
                                   (lean4-info--all-messages-controls))
     all buffer)))

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
           ;; Computed here, in the Lean buffer: by the time a heading is
           ;; inserted the info buffer is current, and point there says
           ;; nothing about the position being reported on.
           (location (lean4-info--location-string))
           (line (1- (line-number-at-pos nil 'absolute)))
           (diagnostics (lean4-info--diagnostics))
           (all (lean4-info--file-messages diagnostics line))
           (sorted (lean4-info--sort-messages diagnostics line))
           (here (cadr (lean4-info--split-diagnostics sorted line)))
           (key (lean4-info--render-key goals term-goal location
                                        diagnostics all)))
      ;; Nothing to see: rebuilding would only make the display blink.
      (unless (and (equal key lean4-info--rendered)
                   (get-buffer lean4-info-buffer-name))
        (setq lean4-info--rendered key)
        (with-current-buffer lean4-info-buffer-name
          (setq lean4-info--handle handle
                lean4-info--source-buffer buffer)
          (lean4-info--keeping-position
            (erase-buffer)
            (lean4-info--insert-display location goals term-goal here
                                        sorted all buffer)
            (lean4-info--add-visibility-indicators)))))))

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
looks like a buffer that has stopped working.

Neither is done while pinned or paused, when point is not what the
display is following.  Rebuilding it anyway produced identical content
over and over, which reads as a flicker -- the pinned display appearing
to blink at the reader for as long as they kept typing."
  (unless lean4-info-paused
    (lean4-info--schedule-update)))

(defun lean4-info--schedule-update ()
  "Redisplay and refresh the info buffer shortly, coalescing repeats."
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

(defun lean4-info--fetch-goals (handle keep show-goals show-term)
  "Ask HANDLE for the goals and the expected type at its position.

KEEP is called with the server-side references each answer owns, so that
whoever asked can give them back when the next set replaces them.
SHOW-GOALS and SHOW-TERM are called with what to display.

Errors are ignored: the server routinely rejects a request for a region
it is still elaborating, and there is nothing useful to report."
  (lean4-rpc-get-interactive-goals
   handle
   (lambda (result)
     (funcall keep (seq-mapcat
                    (lambda (goal)
                      (lean4-render-collect-refs (plist-get goal :type)))
                    (plist-get result :goals) #'list))
     (funcall show-goals
              (lean4-info--goals-value result (plist-get result :goals)
                                       #'lean4-render-goals)))
   #'ignore)
  (lean4-rpc-get-interactive-term-goal
   handle
   (lambda (result)
     (funcall keep (lean4-render-collect-refs (plist-get result :type)))
     (funcall show-term (lean4-render-term-goal result)))
   #'ignore))

(defun lean4-info--refresh-interactive (generation)
  "Fetch and render interactive goals; drop replies older than GENERATION."
  (let ((handle (lean4-rpc-open))
        (buffer (current-buffer)))
    (lean4-info--release-refs)
    (setq lean4-info--handle handle)
    (cl-flet ((receive (act)
                ;; A reply for a position point has already left is not
                ;; worth showing, and would overwrite a newer one.
                (lambda (value)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq generation lean4-info--generation)
                        (funcall act value)
                        (lean4-info-buffer-redisplay)))))))
      (lean4-info--fetch-goals
       handle
       (receive (lambda (refs) (lean4-info--adopt nil refs)))
       (receive (lambda (goals) (setq lean4-goals goals)))
       (receive (lambda (term) (setq lean4-term-goal term))))
      ;; Interactive diagnostics carry the fields Lean never pushes:
      ;; `isSilent' and `leanTags', and so the report that a proof is
      ;; complete.  Fetched here rather than from a notification handler
      ;; because this is the one place already holding an RPC handle.
      (lean4-rpc-get-interactive-diagnostics
       handle
       (receive (lambda (diagnostics)
                  (setq lean4-info--diagnostics (append diagnostics nil))
                  (lean4-diagnostics-update-markers diagnostics)))
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
    ;; Each pin is fetched in its own buffer: a pin can be in a file the
    ;; reader has since left, and its goals are still worth having.
    (dolist (pin lean4-info--pins)
      (lean4-info--refresh-pin pin))
    (when (lean4-info-buffer-active lean4-info-buffer-name)
      (lean4-info--refresh-here))))

(defvar lean4-info--forced nil
  "Bound while a refresh was asked for rather than merely due.")

(defun lean4-info--refresh-pin (pin)
  "Fetch the goals at PIN into it.

Does nothing while PIN is paused, which is what pausing one is for,
unless the refresh was asked for outright.

Only the goals and the expected type: the messages a pin shows are
picked out of the file\='s diagnostics, which are fetched once however
many pins there are."
  (let* ((marker (lean4-info-pin-marker pin))
         (buffer (marker-buffer marker)))
    (when (and (buffer-live-p buffer)
               (or lean4-info--forced (not (lean4-info-pin-paused pin))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          (when (and lean4-info-interactive (eglot-current-server))
            (condition-case nil
                (lean4-info--refresh-pin-interactive pin)
              ;; A server too old for RPC, or a worker that has just died.
              ;; A pin with no goals still shows its messages.
              (error nil))))))))

(defun lean4-info--pin-give-back-refs (pin)
  "Give back the server-side references PIN is holding."
  (when-let* ((handle (lean4-info-pin-handle pin))
              (refs (lean4-info-pin-refs pin)))
    (lean4-rpc-release (lean4-rpc-handle-session handle) refs))
  (setf (lean4-info-pin-refs pin) nil))

(defun lean4-info--refresh-pin-interactive (pin)
  "Fetch PIN\='s goals over RPC, releasing what the last set held."
  (let ((handle (lean4-rpc-open)))
    (lean4-info--pin-give-back-refs pin)
    (setf (lean4-info-pin-handle pin) handle)
    (lean4-info--fetch-goals
     handle
     (lambda (refs)
       (setf (lean4-info-pin-refs pin)
             (append refs (lean4-info-pin-refs pin))))
     (lambda (goals)
       (setf (lean4-info-pin-goals pin) goals)
       (lean4-info--redisplay-source))
     (lambda (term)
       (setf (lean4-info-pin-term-goal pin) term)
       (lean4-info--redisplay-source)))))

(defface lean4-info-button
  '((t :inherit mode-line-buffer-id))
  "Face for the pin and pause controls in the goal display."
  :group 'lean4-info)

(defface lean4-info-button-active
  '((t :inherit warning :weight bold))
  "Face for a goal-display control whose state is engaged."
  :group 'lean4-info)

(defface lean4-info-location
  '((t :inherit magit-section-heading))
  "Face for the source position the goal display is reporting on."
  :group 'lean4-info)

;; Unicode with a fallback, the way `magit-section' picks its own
;; indicators.  Emacs runs in terminals and on machines with no emoji font,
;; so a glyph that is merely likely to work is not good enough on its own.
;;
;; Chosen per frame rather than once at load: `char-displayable-p' answers
;; for the frame it is asked in, and one Emacs can serve a graphical frame
;; and a terminal at the same time.  Deciding at load time would give
;; whichever frame happened to come first.

(defun lean4-info--displayable-p (string)
  "Return non-nil if every character of STRING can be displayed here."
  (seq-every-p #'char-displayable-p (string-to-list string)))

(defun lean4-info--glyph (configured candidates fallback)
  "Return CONFIGURED, or the first of CANDIDATES this frame can show.
FALLBACK is used when it can show none of them."
  (or configured
      (seq-find #'lean4-info--displayable-p candidates)
      fallback))

(defcustom lean4-info-pin-icon nil
  "Control shown in the goal display while it is following point.
Clicking it pins, so it shows a pin held ready.  Nil means pick
whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defcustom lean4-info-unpin-icon nil
  "Control shown in the goal display while it is pinned.
Clicking it unpins, so it shows the pin driven home, as pause and resume
swap their glyphs: the control says what the click does rather than what
the state is.  Nil means pick whichever candidate the frame can
display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defcustom lean4-info-pause-icon nil
  "Control shown in the goal display while it is updating.
Clicking it pauses, so it shows the pause symbol.  Nil means pick
whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defcustom lean4-info-resume-icon nil
  "Control shown in the goal display while it is paused.
Clicking it resumes, so it shows the play symbol -- the pair being the
one place a glyph can say what the click does rather than what the state
is.  Nil means pick whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defun lean4-info-pin-glyph ()
  "Return the pin control for this frame."
  ;; U+1F4CC first: the pin-shaped U+1F588 is missing from many fonts and
  ;; renders as a box.  U+2316, POSITION INDICATOR, is the terminal-safe
  ;; one -- it is in the same block as the media controls, which terminal
  ;; fonts that have any of this tend to cover.
  (lean4-info--glyph lean4-info-pin-icon '("📌" "🖈" "⌖") "P"))

(defun lean4-info-unpin-glyph ()
  "Return the unpin control for this frame.

U+1F4CD is the same pin seen head-on rather than from the side: pushed
in rather than held ready.  VS Code turns its pin from upright to lying
flat to say the same thing, but that is a codicon font.  Unicode\='s one
upright/slanted pin pair needs U+1F588, which too many fonts render as a
box for the difference to rest on.

The candidate has to differ from what `lean4-info-pin-glyph' chose --- a
pair that lands on one glyph says nothing --- so the frame deciding the
first one also decides this."
  (let ((pin (lean4-info-pin-glyph)))
    (or lean4-info-unpin-icon
        (seq-find (lambda (candidate)
                    (and (not (equal candidate pin))
                         (lean4-info--displayable-p candidate)))
                  '("📍" "🖈" "⌾"))
        "[P]")))

(defcustom lean4-info-goto-icon nil
  "Control that sends point to the position a heading names.
Nil means pick whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defcustom lean4-info-refresh-icon nil
  "Control that updates a paused display once.
Nil means pick whichever candidate the frame can display."
  :group 'lean4-info
  :type '(choice (const :tag "Choose to suit the frame" nil) string))

(defun lean4-info-refresh-glyph ()
  "Return the refresh control for this frame."
  (lean4-info--glyph lean4-info-refresh-icon '("⟳" "↻" "⭮") "R"))

(defun lean4-info-goto-glyph ()
  "Return the go-to-position control for this frame."
  ;; VS Code uses a codicon of a page with an arrow leaving it.  Unicode
  ;; has no such glyph, and a bare page says nothing about going there,
  ;; so take the arrow half: U+21AA is the one that reads as jumping
  ;; somewhere rather than as scrolling or as a return.
  (lean4-info--glyph lean4-info-goto-icon '("↪" "⤴" "→") "->"))

(defun lean4-info-pause-glyph ()
  "Return the pause control for this frame."
  (lean4-info--glyph lean4-info-pause-icon '("⏸" "‖") "||"))

(defun lean4-info-resume-glyph ()
  "Return the resume control for this frame."
  (lean4-info--glyph lean4-info-resume-icon '("⏵" "▶" "▸") ">"))

(defun lean4-info--run-control (command)
  "Run COMMAND as though it had been invoked from the Lean buffer.

A control is clicked in the info buffer, which is the wrong buffer for
these commands to run in.  Pinning needs the Lean buffer, because the
position it pins to is point there; run from the info buffer it only
raises an error.

Clicking also selects the info window and leaves point on the glyph,
which parks a blinking cursor on the control and takes the keyboard away
from the file being edited.  VS Code leaves focus in the editor, so put
it back.

The window is selected before COMMAND runs, not after.  A window keeps
its own point while it is not selected, and selecting it overwrites the
buffer\\='s point with that one -- so a command that moved point had its
move made and then silently undone."
  (let* ((source (and (buffer-live-p lean4-info--source-buffer)
                      lean4-info--source-buffer))
         (window (and source (get-buffer-window source t))))
    (if source
        (progn
          (if (window-live-p window)
              (select-window window)
            ;; Nowhere to put the focus back: show the buffer, so that a
            ;; control which moves point has somewhere to move it to.
            (pop-to-buffer source))
          (with-current-buffer source (call-interactively command)))
      (call-interactively command))))

(defun lean4-info--button (label help command &optional active)
  "Return LABEL as a clickable control running COMMAND, described by HELP.
ACTIVE marks the control as engaged, which shows in its face."
  (propertize label
              'face (if active 'lean4-info-button-active 'lean4-info-button)
              'mouse-face 'highlight
              'help-echo help
              ;; What the control does.  `lean4-info-mouse-1' reads this
              ;; at the click position: the binding belongs to the section
              ;; rather than to these few characters, because that is the
              ;; only kind `magit-section' leaves alone.
              'lean4-info-command command))

(defun lean4-info--goto-button (buffer line column &optional label)
  "Return a control sending point to LINE and COLUMN of BUFFER.

LABEL is what to show, defaulting to the go-to glyph.

Names its buffer rather than relying on the one the display is
following: a message belongs to the file it was reported for, whatever
the display has moved on to since."
  (lean4-info--button
   (or label (lean4-info-goto-glyph))
   "mouse-1: go to this position"
   (lambda ()
     (interactive)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (lean4-info--goto-position line column))))))

(defun lean4-info--marker-goto-button (marker)
  "Return a control sending point to MARKER."
  (let ((buffer (marker-buffer marker)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char marker)
            (lean4-info--goto-button buffer (line-number-at-pos)
                                     (current-column))))
      "")))

(defun lean4-info-toggle-pin-pause (pin)
  "Hold PIN\='s goals as they are, or let them follow the file again.

Each pin pauses on its own.  Pinning a position and freezing it are
different things -- a pinned goal goes on following its declaration
through an edit unless told otherwise -- and VS Code gives every pinned
section its own pause."
  (setf (lean4-info-pin-paused pin) (not (lean4-info-pin-paused pin)))
  (lean4-info--redisplay-source)
  (unless (lean4-info-pin-paused pin)
    (lean4-info--refresh-pin pin))
  (message "Pinned position %s"
           (if (lean4-info-pin-paused pin) "paused" "unpaused")))

(defun lean4-info-refresh-pin (pin)
  "Bring PIN up to date without unpausing it."
  (let ((lean4-info--forced t))
    (lean4-info--refresh-pin pin)))

(defun lean4-info--pin-controls (pin)
  "Return the controls for PIN\='s section.

The way back to it, a refresh while it is paused, its own pause, and
unpinning -- in the order the followed section puts the same ones."
  (concat
   (lean4-info--marker-goto-button (lean4-info-pin-marker pin))
   "  "
   (if (lean4-info-pin-paused pin)
       (concat (lean4-info--button
                (lean4-info-refresh-glyph)
                "mouse-1: bring this paused position up to date"
                (lambda () (interactive) (lean4-info-refresh-pin pin)))
               "  ")
     "")
   (lean4-info--button
    (if (lean4-info-pin-paused pin)
        (lean4-info-resume-glyph)
      (lean4-info-pause-glyph))
    (if (lean4-info-pin-paused pin)
        "mouse-1: unpause this pinned position"
      "mouse-1: pause this pinned position")
    (lambda () (interactive) (lean4-info-toggle-pin-pause pin))
    (lean4-info-pin-paused pin))
   "  "
   (lean4-info--button
    (lean4-info-unpin-glyph)
    "mouse-1: unpin this position"
    (lambda () (interactive) (lean4-info-unpin pin))
    t)))

(defun lean4-info--controls ()
  "Return the controls for the section following point."
  (concat
   ;; Only while paused: nothing else leaves the display out of date, so
   ;; anywhere else this would be a control with nothing to do.
   (if lean4-info-paused
       (concat (lean4-info--button
                (lean4-info-refresh-glyph)
                "mouse-1: bring the paused display up to date"
                #'lean4-info-refresh-paused)
               "  ")
     "")
   (lean4-info--button
    (lean4-info-pin-glyph)
    "mouse-1: pin this position, keeping it on display"
    #'lean4-info-toggle-pin)
   "  "
   (lean4-info--button
    (if lean4-info-paused
        (lean4-info-resume-glyph)
      (lean4-info-pause-glyph))
    (if lean4-info-paused
        "mouse-1: unpause, and start updating again"
      "mouse-1: pause updating")
    #'lean4-info-toggle-pause
    lean4-info-paused)))

(defun lean4-info--location-string ()
  "Return the position point is on, as a string."
  (format "%s:%d:%d" (buffer-name) (line-number-at-pos) (current-column)))

(defun lean4-info--marker-location-string (marker)
  "Return the position MARKER is on, as a string."
  (let ((buffer (marker-buffer marker)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char marker)
            (lean4-info--location-string)))
      "(gone)")))

(defun lean4-info--heading (location controls &optional state)
  "Return a section heading for LOCATION, with CONTROLS and STATE.

The controls sit hard right, as they do in VS Code.  A stretch space
does the aligning, so it holds however the window is resized, and the
width is measured in columns rather than characters because the glyphs
need not be single-width."
  (let ((location (propertize location 'face 'lean4-info-location))
        (state (if state
                   (propertize (concat "  " state) 'face 'warning)
                 "")))
    (lean4-info--align-right (concat location state) controls)))

(defun lean4-info--point-state ()
  "Return the word for what is holding the followed position back, or nil."
  (and lean4-info-paused "paused"))

;;;###autoload
(defun lean4-info-toggle-pause ()
  "Pause or unpause the goal display, or the pinned section point is in.

While paused it keeps showing whatever it last showed, so a goal can be
read while point moves elsewhere.  Each pinned section pauses on its
own, so inside one this means that one; anywhere else it means the
section following point.  The names follow Lean\='s own: VS Code calls
these commands pause and unpause."
  (interactive)
  (if-let* ((pin (lean4-info--pin-at-point)))
      (lean4-info-toggle-pin-pause pin)
    (setq lean4-info-paused (not lean4-info-paused))
    ;; Redraw whatever the state: pausing suppresses the refresh, so
    ;; without this the controls would keep showing the state they were
    ;; in until something else happened to redisplay them.
    (lean4-info--redisplay-source)
    (unless lean4-info-paused
      (lean4-info-buffer-refresh))
    (message "Lean goal display %s"
             (if lean4-info-paused "paused" "unpaused"))))

;;;###autoload
(defun lean4-info-refresh-paused ()
  "Bring a paused display up to date, without unpausing it.

Inside a pinned section this means that section; anywhere else it means
the one following point.  Pausing is for reading something while working
elsewhere, which does not always mean wanting it stale for good.  This
is VS Code\='s \"refresh paused state\"."
  (interactive)
  (if-let* ((pin (lean4-info--pin-at-point)))
      (lean4-info-refresh-pin pin)
    (lean4-info--redisplay-source)
    (lean4-info-buffer-refresh)))

(defun lean4-info-unpin (pin)
  "Remove PIN from the goal display."
  (setq lean4-info--pins (delq pin lean4-info--pins))
  (when (and lean4-info--pinned-at
             (eq (car lean4-info--pinned-at) (marker-buffer
                                              (lean4-info-pin-marker pin))))
    (setq lean4-info--pinned-at nil))
  (lean4-info--pin-release pin)
  (lean4-info--redisplay-source)
  (lean4-info-buffer-refresh))

;;;###autoload
(defun lean4-info-toggle-pin ()
  "Pin the goal display to point, or unpin the pin already there.

Each pin is a section of its own, kept above the one following point, so
several positions can be watched at once.  Unlike pausing, a pinned
section keeps updating: it follows the goal at the position it was
pinned to as the file is edited, which is what makes it useful for
watching one goal while working on the tactic above it.

Pinning happens at point, so pinning a position that is already pinned
would only make a second section saying the same thing; this unpins it
instead.  VS Code leaves its control inert there, which says less."
  (interactive)
  (cond
   ;; Inside a pinned section of the display: that is the pin meant.
   ((get-text-property (point) 'lean4-info-pin)
    (lean4-info-unpin (get-text-property (point) 'lean4-info-pin))
    (message "Unpinned"))
   ;; Elsewhere in the display: do what the Lean buffer would do, which
   ;; is what the control in the followed section does.
   ((and (not (derived-mode-p 'lean4-mode))
         (buffer-live-p lean4-info--source-buffer))
    (lean4-info--run-control #'lean4-info-toggle-pin))
   ((lean4-info--pin-at)
    (lean4-info-unpin (lean4-info--pin-at))
    (message "Unpinned line %d" (line-number-at-pos)))
   (t
    (unless (derived-mode-p 'lean4-mode)
      (user-error "Not in a Lean buffer"))
    ;; A marker rather than a position: the point of pinning is to watch a
    ;; location while editing around it, which moves it.
    (setq lean4-info--pins
          (append lean4-info--pins
                  (list (lean4-info--pin-create
                         :marker (copy-marker (point))
                         :id (cl-incf lean4-info--pin-counter)))))
    ;; What was just pinned is where point is, so the display would show
    ;; it twice until point moved.
    (setq lean4-info--pinned-at (cons (current-buffer) (point)))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (display-buffer lean4-info-buffer-name)
    (message "Pinned line %d" (line-number-at-pos))
    (lean4-info--redisplay-source)
    (lean4-info-buffer-refresh))))

;;;###autoload
(defun lean4-info-unpin-all ()
  "Remove every pin from the goal display."
  (interactive)
  (mapc #'lean4-info--pin-release lean4-info--pins)
  (setq lean4-info--pins nil
        lean4-info--pinned-at nil)
  (lean4-info--redisplay-source)
  (lean4-info-buffer-refresh))

(defun lean4-info--following-point-p ()
  "Return non-nil if the position following point should be shown.

Not while it is the position just pinned: pinning happens at point, so
the two are the same until point moves."
  (not (and lean4-info--pinned-at
            (eq (car lean4-info--pinned-at) (current-buffer))
            (= (cdr lean4-info--pinned-at) (point)))))

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
      (cl-incf lean4-info--trace-generation)
      (lean4-info--redisplay-source))
     ((eq (car children) 'strict)
      (puthash path (cdr children) lean4-info--trace-expansion)
      (cl-incf lean4-info--trace-generation)
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
               (cl-incf lean4-info--trace-generation)
               (lean4-info--redisplay-source))))
         (lambda (error)
           (message "Could not expand trace: %S" error))))))))

(defun lean4-info--redisplay-source ()
  "Re-render the info buffer from the Lean buffer that populated it.

The source buffer is recorded local to the *info* buffer, so it has to
be read from there: called from a Lean buffer -- which is where pinning
and pausing happen -- reading it directly finds nothing, and the display
silently fails to redraw."
  (when-let* ((info (get-buffer lean4-info-buffer-name))
              (source (buffer-local-value 'lean4-info--source-buffer info))
              ((buffer-live-p source)))
    (let ((position (and (eq (current-buffer) info) (point))))
      (with-current-buffer source
        (lean4-info-buffer-redisplay 'force))
      ;; Redisplay rebuilds the buffer, so put point back where the reader
      ;; left it rather than at the top.
      (when position
        (with-current-buffer info
          (goto-char (min position (point-max))))))))

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
