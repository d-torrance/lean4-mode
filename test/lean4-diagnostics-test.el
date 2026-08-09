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

(provide 'lean4-diagnostics-test)
;;; lean4-diagnostics-test.el ends here
