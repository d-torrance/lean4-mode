;;; lean4-diagnostics.el --- Lean's diagnostic extensions -*- lexical-binding: t -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Lean adds four fields to the LSP `Diagnostic'.  Three of them this
;; library acts on; the fourth is listed so that the next reader knows it
;; was considered.
;;
;;   fullRange       the whole construct a message is about, as opposed to
;;                   the span that gets underlined.  Sent unconditionally;
;;                   read in `lean4-info'
;;   leanTags        UnsolvedGoals and GoalsAccomplished, which drive the two
;;                   decorations VS Code shows in the gutter.  Sent
;;                   unconditionally
;;   isSilent        a diagnostic meant for the InfoView but not for the
;;                   editor.  Filtered out before Flymake sees it, or every
;;                   completed proof would be underlined.  Gated behind
;;                   `silentDiagnosticSupport', which is asked for in
;;                   `lean4-client-capabilities'
;;   isIncremental   diagnostics to append to what is already displayed
;;                   rather than replace it.  Gated behind
;;                   `incrementalDiagnosticSupport', which is deliberately
;;                   not asked for; see "Capabilities" below
;;
;; A server that predates any of these simply never sets it, and everything
;; here degrades to doing nothing.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'seq)

(require 'lean4-eglot)
(require 'lean4-settings)
(require 'lean4-util)

;;;; Tags

(defconst lean4-diagnostics-tag-unsolved-goals 1
  "Lean's tag for a diagnostic reporting goals left unproved.")

(defconst lean4-diagnostics-tag-goals-accomplished 2
  "Lean's tag for a declaration whose goals are all discharged.")

;;;; Reading a raw LSP diagnostic
;;
;; The plists Lean sends, whether they arrive over RPC or by notification.

(defun lean4-diagnostics-range (diagnostic)
  "Return the extent of raw LSP DIAGNOSTIC, preferring Lean's `fullRange'.
Lean reports two extents per diagnostic: `range', which is what gets
underlined, and `fullRange', which covers the whole construct the
message is about.  Servers older than the extension send only `range'."
  (or (plist-get diagnostic :fullRange) (plist-get diagnostic :range)))

(defun lean4-diagnostics-start-line (diagnostic)
  "Return the zero-based line raw LSP DIAGNOSTIC starts on."
  (or (thread-first diagnostic lean4-diagnostics-range
                    (plist-get :start) (plist-get :line))
      0))

(defun lean4-diagnostics-start-column (diagnostic)
  "Return the zero-based column raw LSP DIAGNOSTIC starts at."
  (or (thread-first diagnostic lean4-diagnostics-range
                    (plist-get :start) (plist-get :character))
      0))

(defun lean4-diagnostics-end-line (diagnostic)
  "Return the zero-based line raw LSP DIAGNOSTIC ends on."
  (or (thread-first diagnostic lean4-diagnostics-range
                    (plist-get :end) (plist-get :line))
      0))

;;;; Reading a Flymake diagnostic
;;
;; What Eglot hands Flymake, and what Lean put in it before Eglot did.
;; Each of these is its raw counterpart above, reached through the object
;; Eglot stashed.

(defun lean4-diagnostic-lsp-data (diagnostic)
  "Return the raw LSP object behind Flymake DIAGNOSTIC, or nil.
Eglot stashes the server's original `Diagnostic' there, which is the
only way to reach Lean's extensions to it."
  (alist-get 'eglot-lsp-diag (flymake-diagnostic-data diagnostic)))

(defun lean4-diagnostic-full-range (diagnostic)
  "Return the `fullRange' of Flymake DIAGNOSTIC, falling back to `range'."
  (lean4-diagnostics-range (lean4-diagnostic-lsp-data diagnostic)))

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

(defun lean4-diagnostics-tags (diagnostic)
  "Return the Lean tags on a raw LSP DIAGNOSTIC, as a list of integers."
  (append (plist-get diagnostic :leanTags) nil))

(defun lean4-diagnostics-silent-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC is meant only for the goal display."
  (eq (plist-get diagnostic :isSilent) t))

(defconst lean4-diagnostics-severities
  '((error . 1) (warning . 2) (note . 3) (hint . 4))
  "LSP diagnostic severities, most severe first.")

(defcustom lean4-flymake-minimum-severity 'warning
  "How severe a diagnostic must be to appear in the editor.

Lean says a good deal at note severity that is meant for the goal
display rather than for the margin: the report that a proof is complete,
trace output, and whatever a project's own tactics emit.  Flagging all
of it as a problem buries the errors.

Anything below this level is still shown in the goal display, which is
where it is worth reading.  Set to `note' to have Flymake show
everything, as VS Code's Problems view does."
  :group 'lean4
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Errors and warnings" warning)
                 (const :tag "Everything" note)
                 (const :tag "Everything, including hints" hint)))

(defun lean4-diagnostics-severity (diagnostic)
  "Return the LSP severity of raw DIAGNOSTIC, defaulting to error."
  (or (plist-get diagnostic :severity) 1))

(defun lean4-diagnostics-suppressed-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC should be kept out of the editor."
  (or (lean4-diagnostics-silent-p diagnostic)
      (> (lean4-diagnostics-severity diagnostic)
         (alist-get lean4-flymake-minimum-severity
                    lean4-diagnostics-severities 3))))

(defun lean4-diagnostics-goals-accomplished-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC reports a finished proof."
  (and (memq lean4-diagnostics-tag-goals-accomplished
             (lean4-diagnostics-tags diagnostic))
       t))

(defun lean4-diagnostics-unsolved-goals-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC reports goals left unproved.
Read from Lean\='s own tag rather than from the message text, which is
what makes this survive a rewording."
  (and (memq lean4-diagnostics-tag-unsolved-goals
             (lean4-diagnostics-tags diagnostic))
       t))

;;;; Capabilities

;; `incrementalDiagnosticSupport' is deliberately not asked for.  It is not
;; a hint: with it, Lean starts sending partial batches marked
;; `isIncremental' that the client is required to append to what it already
;; has.  Eglot replaces instead, so each batch discards the previous one and
;; diagnostics from earlier in the file vanish as elaboration proceeds --
;; verified against Lean 4.32.2, where advertising it lost the `sorry'
;; warning as soon as the type error further down arrived.  Honouring it
;; needs version-keyed accumulation ahead of Eglot's own handler; until that
;; exists, not asking is what keeps diagnostics correct.
;;
;; `silentDiagnosticSupport' *is* asked for, in `lean4-client-capabilities';
;; see there for why both asking and then fetching are needed.

;;;; Filtering

(defcustom lean4-show-goals-accomplished t
  "Whether to mark declarations whose goals are all discharged.
The counterpart of the double check-mark VS Code shows in the gutter."
  :group 'lean4
  :type 'boolean)

(defface lean4-goals-accomplished
  '((t :inherit success))
  "Face for the marker shown against a completed proof.

Inherited rather than coloured here, so that the marker is whatever the
theme in force calls success -- green in most of them, and legible on
that theme\\='s background, which a colour named here would not be.  VS
Code draws this decoration in the same blue as the unsolved-goals one;
Emacs has a face that means success and none that means blue, and
following the theme is worth more than matching the shade.  A theme
that gives success the colour it gives something else can be overruled
by customizing this face."
  :group 'lean4)

(defcustom lean4-show-unsolved-goals t
  "Whether to mark the line where an \"unsolved goals\" error ends.
The counterpart of VS Code\='s `lean4.showUnsolvedGoalsDecoration', which
defaults on as well.  It draws a tool at the end of the line, away from
the text, where the underline alone says only that something is wrong
there."
  :group 'lean4
  :type 'boolean)

(defface lean4-unsolved-goals
  '((t :inherit warning))
  "Face for the marker shown where goals are left unproved.

Inherited from `warning' -- the marker says there is work left at this
line, which is what that face is for, and it has to be told apart from
the code beside it.  It was `shadow' before, which is the grey a theme
gives comments: the one thing a marker sitting after the code must not
look like."
  :group 'lean4)

(defun lean4-diagnostics--unsolved-goals-marker ()
  "Return the marker to draw for goals left unproved.

VS Code uses a tool, and U+1F6E0 is that tool -- but it is missing from
emoji fonts that carry the rest of the block, Noto Color Emoji among
them, so the frame is asked before it is used.  U+2692 is the same idea
in a font a great many more machines have, and a frame that can draw
neither gets a word."
  (concat " " (lean4--glyph nil
                            '("\N{HAMMER AND WRENCH}" "\N{HAMMER AND PICK}")
                            "(goals)")))

(define-fringe-bitmap 'lean4-diagnostics-accomplished-bitmap
  [#b00000011
   #b00000110
   #b00001100
   #b11011000
   #b01110000
   #b00110000]
  nil nil 'center)

(defun lean4-diagnostics--accomplished-marker ()
  "Return the marker to draw against a finished proof.

In the fringe, beside the line, where Flymake draws its own indicators
and where VS Code puts this one: a marker in the text pushes the
declaration across and takes the indentation of everything under it
along, which is a poor reward for having proved something.

A frame with no fringe to draw in -- a terminal -- gets the check mark
itself, there being nowhere else to put it."
  (if (display-graphic-p)
      (propertize "✓" 'display
                  (list (or flymake-fringe-indicator-position 'left-fringe)
                        'lean4-diagnostics-accomplished-bitmap
                        'lean4-goals-accomplished))
    (propertize "✓ " 'face 'lean4-goals-accomplished)))

(defvar-local lean4-diagnostics--accomplished-overlays nil
  "Overlays marking what Lean has tagged: finished proofs and unproved goals.

Every one of these is empty -- it carries a `before-string' or an
`after-string' and covers no text -- so none of them may be given the
`evaporate' property.  Emacs deletes an empty overlay the moment that
property is set on it, which is what kept the completed-proof marker from
ever appearing.  They are cleared by hand instead, on every pass.")

(defun lean4-diagnostics--clear-accomplished ()
  "Remove the tag markers from the current buffer."
  (mapc #'delete-overlay lean4-diagnostics--accomplished-overlays)
  (setq lean4-diagnostics--accomplished-overlays nil))

(defun lean4-diagnostics--mark-unsolved (diagnostic)
  "Mark the end of the line DIAGNOSTIC\='s range ends on.
At the end of the line rather than at the range, as VS Code places it:
the underline already says where, and a marker inside the code would sit
in the middle of the term it is about."
  (when-let* ((region (ignore-errors
                        (lean4--range-region (plist-get diagnostic :range)))))
    (save-excursion
      (goto-char (cdr region))
      (let ((overlay (make-overlay (line-end-position) (line-end-position))))
        (overlay-put overlay 'after-string
                     (propertize (lean4-diagnostics--unsolved-goals-marker)
                                 'face 'lean4-unsolved-goals))
        (push overlay lean4-diagnostics--accomplished-overlays)))))

(defun lean4-diagnostics--mark-accomplished (diagnostics)
  "Mark the declarations DIAGNOSTICS report as having no goals left.
DIAGNOSTICS are raw LSP objects, including the silent ones: Lean reports
a finished proof precisely as a silent diagnostic, so this has to run
before they are filtered away."
  (lean4-diagnostics--clear-accomplished)
  (dolist (diagnostic (append diagnostics nil))
    (cond
     ((and lean4-show-goals-accomplished
           (lean4-diagnostics-goals-accomplished-p diagnostic))
      (when-let* ((region (ignore-errors
                            (lean4--range-region
                             (plist-get diagnostic :range))))
                  (overlay (make-overlay (car region) (car region))))
        (overlay-put overlay 'before-string
                     (lean4-diagnostics--accomplished-marker))
        (push overlay lean4-diagnostics--accomplished-overlays)))
     ((and lean4-show-unsolved-goals
           (lean4-diagnostics-unsolved-goals-p diagnostic))
      (lean4-diagnostics--mark-unsolved diagnostic)))))

(cl-defmethod eglot-handle-notification :around
  ((server lean4-eglot-lsp-server)
   (method (eql textDocument/publishDiagnostics))
   &rest params &key uri diagnostics &allow-other-keys)
  "Keep Lean's silent diagnostics out of the editor.

Asking for `silentDiagnosticSupport' means Lean sends them; it is then
the client's job not to show them as problems.  \"Goals accomplished!\"
is one, and without this it appears in Flymake as a note against every
completed proof -- a report that nothing is wrong, filed as something
wrong.

The tags are read before the filter runs, since a finished proof is
reported precisely as a silent diagnostic and that is what the
goals-accomplished marker is made of."
  (lean4-with-uri-buffers server uri
    (lean4-diagnostics--mark-accomplished diagnostics))
  (let ((visible (seq-remove #'lean4-diagnostics-suppressed-p
                             (append diagnostics nil))))
    (apply #'cl-call-next-method server method
           (plist-put (copy-sequence params) :diagnostics (vconcat visible)))))

(defalias 'lean4-diagnostics-update-markers #'lean4-diagnostics--mark-accomplished
  "Refresh the goals-accomplished markers from DIAGNOSTICS.

DIAGNOSTICS must be the *interactive* ones, from
`Lean.Widget.getInteractiveDiagnostics'.  That is not a detail: Lean
reports a finished proof as a silent diagnostic, and silent diagnostics
are precisely the ones it does not push over
`textDocument/publishDiagnostics'.  Verified against Lean 4.32.2, where
`isSilent' and `leanTags' appear on the interactive diagnostics and on
no pushed one.")

;;;; Navigation

(defun lean4-diagnostics-next-sorry (&optional count)
  "Move to the COUNTth next declaration that still uses `sorry'.
VS Code leaves this to its Problems view; here the natural home is the
same `next-error' machinery every other Emacs mode uses, so this is a
convenience on top of Flymake rather than a parallel system."
  (interactive "p")
  (let* ((count (or count 1))
         (sorries (seq-filter
                   (lambda (diagnostic)
                     (string-search "sorry" (or (flymake-diagnostic-text diagnostic)
                                                "")))
                   (flymake-diagnostics)))
         (later (seq-filter (lambda (diagnostic)
                              (> (flymake-diagnostic-beg diagnostic) (point)))
                            sorries)))
    (if-let* ((target (nth (1- count) (or later sorries))))
        (goto-char (flymake-diagnostic-beg target))
      (user-error "No declaration uses `sorry' in this buffer"))))

(provide 'lean4-diagnostics)
;;; lean4-diagnostics.el ends here
