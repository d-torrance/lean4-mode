;;; lean4-diagnostics-test.el --- Tests for Lean's diagnostic extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Lean 4.32.2 does not set `leanTags' or `isSilent' on the diagnostics it
;; produces for this package's fixture, even though the `:lean' capability
;; block reaches it -- confirmed by the server echoing back the RPC wire
;; format negotiated in the same block.  So these fields are exercised here
;; with synthetic diagnostics of the documented shape rather than end to end.
;;
;; That is a deliberate limit on what is claimed: the handling is correct if
;; a server sends the fields, and does nothing if it does not.

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
  "`silentDiagnosticSupport' is advertised, since it is acted on."
  (should (plist-get lean4-client-capabilities :silentDiagnosticSupport)))

(provide 'lean4-diagnostics-test)
;;; lean4-diagnostics-test.el ends here
