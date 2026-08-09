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

;; Lean adds four fields to the LSP `Diagnostic', each gated behind a client
;; capability so that a client which does not understand them never sees
;; them.  This library asks for them and acts on them.
;;
;;   fullRange       the whole construct a message is about, as opposed to
;;                   the span that gets underlined.  Read in `lean4-info'
;;   isSilent        a diagnostic meant for the InfoView but not for the
;;                   editor.  Filtered out before Flymake sees it, or every
;;                   completed proof would be underlined
;;   leanTags        UnsolvedGoals and GoalsAccomplished, which drive the two
;;                   decorations VS Code shows in the gutter
;;   isIncremental   diagnostics to append to what is already displayed
;;                   rather than replace it, so a long file does not flicker
;;                   between partial results while it elaborates
;;
;; A server that predates any of these simply never sets it, and everything
;; here degrades to doing nothing.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'seq)

(require 'lean4-eglot)
(require 'lean4-settings)

;;;; Tags

(defconst lean4-diagnostics-tag-unsolved-goals 1
  "Lean's tag for a diagnostic reporting goals left unproved.")

(defconst lean4-diagnostics-tag-goals-accomplished 2
  "Lean's tag for a declaration whose goals are all discharged.")

(defun lean4-diagnostics-tags (diagnostic)
  "Return the Lean tags on a raw LSP DIAGNOSTIC, as a list of integers."
  (append (plist-get diagnostic :leanTags) nil))

(defun lean4-diagnostics-silent-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC is meant only for the goal display."
  (eq (plist-get diagnostic :isSilent) t))

(defun lean4-diagnostics-goals-accomplished-p (diagnostic)
  "Return non-nil if raw LSP DIAGNOSTIC reports a finished proof."
  (memq lean4-diagnostics-tag-goals-accomplished
        (lean4-diagnostics-tags diagnostic)))

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
;; `silentDiagnosticSupport' *is* asked for, and both halves of that matter.
;; Without the capability Lean withholds silent diagnostics entirely -- drop
;; it and the "Goals accomplished!" report vanishes.  But asking is not
;; enough either: silent diagnostics are never *pushed*.  They arrive only
;; through `Lean.Widget.getInteractiveDiagnostics'.  Both verified against
;; Lean 4.32.2 by removing each in turn.
(setq lean4-client-capabilities
      (append lean4-client-capabilities '(:silentDiagnosticSupport t)))

;;;; Filtering

(defcustom lean4-show-goals-accomplished t
  "Whether to mark declarations whose goals are all discharged.
The counterpart of the double check-mark VS Code shows in the gutter."
  :group 'lean4
  :type 'boolean)

(defface lean4-goals-accomplished
  '((t :inherit success))
  "Face for the marker shown against a completed proof."
  :group 'lean4)

(defvar-local lean4-diagnostics--accomplished-overlays nil
  "Overlays marking declarations with no goals left.")

(defun lean4-diagnostics--clear-accomplished ()
  "Remove the goals-accomplished markers from the current buffer."
  (mapc #'delete-overlay lean4-diagnostics--accomplished-overlays)
  (setq lean4-diagnostics--accomplished-overlays nil))

(defun lean4-diagnostics--mark-accomplished (diagnostics)
  "Mark the declarations DIAGNOSTICS report as having no goals left.
DIAGNOSTICS are raw LSP objects, including the silent ones: Lean reports
a finished proof precisely as a silent diagnostic, so this has to run
before they are filtered away."
  (lean4-diagnostics--clear-accomplished)
  (when lean4-show-goals-accomplished
    (dolist (diagnostic (append diagnostics nil))
      (when (lean4-diagnostics-goals-accomplished-p diagnostic)
        (when-let* ((region (ignore-errors
                              (lean4--range-region
                               (plist-get diagnostic :range))))
                    (overlay (make-overlay (car region) (car region))))
          (overlay-put overlay 'before-string
                       (propertize "✓ " 'face 'lean4-goals-accomplished))
          (overlay-put overlay 'evaporate t)
          (push overlay lean4-diagnostics--accomplished-overlays))))))

(defun lean4-diagnostics-update-markers (diagnostics)
  "Refresh the goals-accomplished markers from DIAGNOSTICS.

DIAGNOSTICS must be the *interactive* ones, from
`Lean.Widget.getInteractiveDiagnostics'.  That is not a detail: Lean
reports a finished proof as a silent diagnostic, and silent diagnostics
are precisely the ones it does not push over
`textDocument/publishDiagnostics'.  Verified against Lean 4.32.2, where
`isSilent' and `leanTags' appear on the interactive diagnostics and on
no pushed one."
  (lean4-diagnostics--mark-accomplished diagnostics))

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
