;;; lean4-diagnostics-test.el --- Tests for Lean's diagnostic extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The predicates are exercised here with synthetic diagnostics of the
;; documented shape.  That they fire on real data is covered end to end in
;; lean4-e2e-test.el, against the *interactive* diagnostics -- which is the
;; only place Lean puts `isSilent' and `leanTags'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-diagnostics)

(ert-deftest lean4-diagnostics-reads-tags ()
  "Lean's tags are read off a raw diagnostic."
  (should (equal (lean4-diagnostics-tags '(:leanTags [1 2])) '(1 2)))
  (should-not (lean4-diagnostics-tags '(:message "no tags"))))

(ert-deftest lean4-diagnostics-recognises-goals-accomplished ()
  "Tag 2 means the declaration's goals are all discharged."
  (should (lean4-diagnostics-goals-accomplished-p
           `(:leanTags [,lean4-diagnostics-tag-goals-accomplished])))
  (should-not (lean4-diagnostics-goals-accomplished-p
               `(:leanTags [,lean4-diagnostics-tag-unsolved-goals])))
  (should-not (lean4-diagnostics-goals-accomplished-p '(:message "plain"))))

(ert-deftest lean4-diagnostics-recognises-silent ()
  "A silent diagnostic is for the goal display, not for the editor."
  (should (lean4-diagnostics-silent-p '(:isSilent t)))
  (should-not (lean4-diagnostics-silent-p '(:isSilent :json-false)))
  (should-not (lean4-diagnostics-silent-p '(:message "plain"))))

(ert-deftest lean4-diagnostics-does-not-request-incremental ()
  "`incrementalDiagnosticSupport' is not advertised.

Regression test.  Asking for it makes Lean send partial batches the
client must *append*; Eglot replaces instead, so each batch discards the
last and diagnostics earlier in the file disappear as elaboration moves
on.  Observed against Lean 4.32.2: advertising it lost the `sorry'
warning the moment the type error below it arrived.  Do not add this
back without implementing version-keyed accumulation first."
  (should-not (plist-get lean4-client-capabilities
                         :incrementalDiagnosticSupport)))

(ert-deftest lean4-diagnostics-requests-silent-support ()
  "`silentDiagnosticSupport' is advertised.

Regression test.  Without it Lean withholds silent diagnostics
altogether and the \"Goals accomplished!\" report never arrives --
verified against 4.32.2 by removing it.  Asking is only half of it: they
are never pushed either, and reach us only through
`Lean.Widget.getInteractiveDiagnostics'."
  (should (plist-get lean4-client-capabilities :silentDiagnosticSupport)))

(ert-deftest lean4-diagnostics-severity-threshold-filters-the-editor ()
  "Diagnostics below the threshold are kept out of the editor.

Lean says a good deal at note severity that is meant for the goal
display: the completed-proof report, traces, and whatever a project's
own tactics emit.  Flagging all of it buries the errors."
  (let ((lean4-flymake-minimum-severity 'warning))
    (should-not (lean4-diagnostics-suppressed-p '(:severity 1)))
    (should-not (lean4-diagnostics-suppressed-p '(:severity 2)))
    (should (lean4-diagnostics-suppressed-p '(:severity 3)))
    (should (lean4-diagnostics-suppressed-p '(:severity 4))))
  ;; Everything, as VS Code's Problems view shows it.
  (let ((lean4-flymake-minimum-severity 'note))
    (should-not (lean4-diagnostics-suppressed-p '(:severity 3)))
    (should (lean4-diagnostics-suppressed-p '(:severity 4))))
  (let ((lean4-flymake-minimum-severity 'error))
    (should-not (lean4-diagnostics-suppressed-p '(:severity 1)))
    (should (lean4-diagnostics-suppressed-p '(:severity 2)))))

(ert-deftest lean4-diagnostics-severity-defaults-to-error ()
  "A diagnostic with no severity is treated as an error, not hidden."
  (should (= (lean4-diagnostics-severity '(:message "x")) 1))
  (let ((lean4-flymake-minimum-severity 'error))
    (should-not (lean4-diagnostics-suppressed-p '(:message "x")))))

(ert-deftest lean4-diagnostics-silent-is-suppressed-at-any-severity ()
  "A silent diagnostic is for the goal display whatever its severity."
  (let ((lean4-flymake-minimum-severity 'hint))
    (should (lean4-diagnostics-suppressed-p '(:isSilent t :severity 1)))))

(ert-deftest lean4-diagnostics-marker-needs-the-tag ()
  "The goals-accomplished marker reads the tag, and only the tag.
Recognising the report by its wording was tried and abandoned: too
fragile, and it addressed one message where the real problem was note
severity generally."
  (should (lean4-diagnostics-goals-accomplished-p
           `(:leanTags [,lean4-diagnostics-tag-goals-accomplished])))
  (should-not (lean4-diagnostics-goals-accomplished-p
               '(:message "Goals accomplished!"))))

;;;; The unsolved-goals marker

(ert-deftest lean4-diagnostics-recognises-unsolved-goals ()
  "Read from Lean's tag, so a rewording of the message cannot break it."
  (should (lean4-diagnostics-unsolved-goals-p
           `(:leanTags [,lean4-diagnostics-tag-unsolved-goals])))
  (should-not (lean4-diagnostics-unsolved-goals-p
               `(:leanTags [,lean4-diagnostics-tag-goals-accomplished])))
  (should-not (lean4-diagnostics-unsolved-goals-p '(:leanTags [])))
  ;; A message that merely says so is not enough; the tag is what counts.
  (should-not (lean4-diagnostics-unsolved-goals-p
               '(:message "unsolved goals"))))

(ert-deftest lean4-diagnostics-marks-the-end-of-the-line ()
  "The marker goes after the line the error ends on, not into the code.
The underline already says where; a marker inside the term would sit in
the middle of what it is about."
  (with-temp-buffer
    (insert "theorem t : True := by
  skip
next line
")
    (let ((lean4-show-unsolved-goals t)
          (lean4-show-goals-accomplished nil))
      (lean4-diagnostics--mark-accomplished
       ;; Range covering line 1 through line 2 (zero-based).
       `[(:leanTags [,lean4-diagnostics-tag-unsolved-goals]
          :range (:start (:line 0 :character 0)
                  :end (:line 1 :character 6)))])
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (equal (length overlays) 1))
        (let ((overlay (car overlays)))
          ;; End of the second line, which is where the range ends.
          (should (equal (overlay-start overlay)
                         (save-excursion (goto-char (point-min))
                                         (forward-line 1)
                                         (line-end-position))))
          (should (overlay-get overlay 'after-string)))))))

(ert-deftest lean4-diagnostics-unsolved-marker-can-be-turned-off ()
  "The option is VS Code's `showUnsolvedGoalsDecoration', default on."
  (with-temp-buffer
    (insert "theorem t : True := by
")
    (let ((lean4-show-unsolved-goals nil)
          (lean4-show-goals-accomplished nil))
      (lean4-diagnostics--mark-accomplished
       `[(:leanTags [,lean4-diagnostics-tag-unsolved-goals]
          :range (:start (:line 0 :character 0)
                  :end (:line 0 :character 5)))])
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest lean4-diagnostics-markers-are-replaced-not-accumulated ()
  "Each pass clears what the last one drew."
  (with-temp-buffer
    (insert "theorem t : True := by
")
    (let ((lean4-show-unsolved-goals t)
          (lean4-show-goals-accomplished nil)
          (diagnostics `[(:leanTags [,lean4-diagnostics-tag-unsolved-goals]
                          :range (:start (:line 0 :character 0)
                                  :end (:line 0 :character 5)))]))
      (lean4-diagnostics--mark-accomplished diagnostics)
      (lean4-diagnostics--mark-accomplished diagnostics)
      (should (equal (length (overlays-in (point-min) (point-max))) 1)))))

(ert-deftest lean4-diagnostics-unsolved-marker-has-a-fallback ()
  "A frame that cannot draw the tool gets something it can."
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
    (should (equal (lean4-diagnostics--unsolved-goals-marker) " (goals)")))
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
    (should (string-search "\N{HAMMER AND WRENCH}"
                           (lean4-diagnostics--unsolved-goals-marker)))))

(ert-deftest lean4-diagnostics-accomplished-marker-survives-being-made ()
  "The completed-proof marker must actually end up in the buffer.

Regression test.  Both markers are empty overlays carrying a string, and
they used to be given the `evaporate' property -- which Emacs honours by
deleting an empty overlay the instant it is set, so the marker was
destroyed before it could be drawn and `lean4-show-goals-accomplished'
did nothing at all."
  (with-temp-buffer
    (insert "theorem proved : True := trivial\n")
    (let ((lean4-show-goals-accomplished t)
          (lean4-show-unsolved-goals nil))
      (lean4-diagnostics--mark-accomplished
       `[(:leanTags [,lean4-diagnostics-tag-goals-accomplished]
          :range (:start (:line 0 :character 0)
                  :end (:line 0 :character 7)))])
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (equal (length overlays) 1))
        (should (string-search "✓" (overlay-get (car overlays) 'before-string))))
      ;; And it is still there, not merely recorded in the list.
      (should (seq-every-p #'overlay-buffer
                          lean4-diagnostics--accomplished-overlays)))))

(ert-deftest lean4-diagnostics-markers-are-never-evaporating ()
  "No marker may carry `evaporate': see the regression test above."
  (with-temp-buffer
    (insert "theorem t : True := by\n")
    (let ((lean4-show-goals-accomplished t)
          (lean4-show-unsolved-goals t))
      (lean4-diagnostics--mark-accomplished
       `[(:leanTags [,lean4-diagnostics-tag-goals-accomplished]
          :range (:start (:line 0 :character 0)
                  :end (:line 0 :character 7)))
         (:leanTags [,lean4-diagnostics-tag-unsolved-goals]
          :range (:start (:line 0 :character 0)
                  :end (:line 0 :character 20)))])
      (should (equal (length lean4-diagnostics--accomplished-overlays) 2))
      (dolist (overlay lean4-diagnostics--accomplished-overlays)
        (should (overlay-buffer overlay))
        (should-not (overlay-get overlay 'evaporate))))))

(provide 'lean4-diagnostics-test)
;;; lean4-diagnostics-test.el ends here
