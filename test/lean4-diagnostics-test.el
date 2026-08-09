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

(ert-deftest lean4-diagnostics-recognises-the-report-by-wording ()
  "The completed-proof report is recognised on servers that do not mark it.

Lean 4.26 sets neither `isSilent' nor `leanTags', even when the client
asks: the message is all there is.  Matching on wording is confined to
this one message, which is fixed rather than a template."
  (should (lean4-diagnostics-goals-accomplished-text-p
           '(:message "Goals accomplished!")))
  (should (lean4-diagnostics-goals-accomplished-text-p
           '(:message "goals accomplished")))
  ;; Not anything that merely mentions it.
  (should-not (lean4-diagnostics-goals-accomplished-text-p
               '(:message "Goals accomplished, but there is a warning")))
  (should-not (lean4-diagnostics-goals-accomplished-text-p
               '(:message "declaration uses `sorry`"))))

(ert-deftest lean4-diagnostics-suppresses-both-ways ()
  "A report is kept out of the editor whether marked or merely worded."
  ;; Marked, as a current server sends it.
  (should (lean4-diagnostics-suppressed-p '(:isSilent t :message "anything")))
  ;; Unmarked, as 4.26 sends it.
  (should (lean4-diagnostics-suppressed-p '(:message "Goals accomplished!")))
  ;; Real diagnostics are untouched either way.
  (should-not (lean4-diagnostics-suppressed-p
               '(:message "declaration uses `sorry`")))
  ;; And the fallback can be turned off.
  (let ((lean4-hide-goals-accomplished nil))
    (should-not (lean4-diagnostics-suppressed-p
                 '(:message "Goals accomplished!")))
    ;; The marked case still works, since that is the real mechanism.
    (should (lean4-diagnostics-suppressed-p '(:isSilent t)))))

(ert-deftest lean4-diagnostics-marker-works-without-tags ()
  "The goals-accomplished marker fires on an unmarked report too."
  (should (lean4-diagnostics-goals-accomplished-p
           '(:message "Goals accomplished!")))
  (should (lean4-diagnostics-goals-accomplished-p
           `(:leanTags [,lean4-diagnostics-tag-goals-accomplished]))))

(provide 'lean4-diagnostics-test)
;;; lean4-diagnostics-test.el ends here
