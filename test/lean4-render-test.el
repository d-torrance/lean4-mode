;;; lean4-render-test.el --- Tests for the goal renderer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The renderer is pure, so all of this runs without a Lean toolchain or a
;; server.  The payloads below are real: they were captured from Lean 4.32.2
;; answering `Lean.Widget.getInteractiveGoals' over the fixture project.

;;; Code:

(require 'ert)
(require 'lean4-render)

;;;; Fixtures

(defconst lean4-render-test--one-plus-one
  ;; The `CodeWithInfos' for `1 + 1 = 2', captured verbatim.  Note how the
  ;; numerals nest inside the sum, which nests inside the equation.
  '(:tag [(:info (:__rpcref "0") :subexprPos "/")
          (:append
           [(:tag [(:info (:__rpcref "1") :subexprPos "/0/1")
                   (:append
                    [(:tag [(:info (:__rpcref "2") :subexprPos "/0/1/0/1")
                            (:text "1")])
                     (:text " + ")
                     (:tag [(:info (:__rpcref "3") :subexprPos "/0/1/1")
                            (:text "1")])])])
            (:text " = ")
            (:tag [(:info (:__rpcref "4") :subexprPos "/1")
                   (:text "2")])])])
  "Real `CodeWithInfos' for the proposition `1 + 1 = 2'.")

(defun lean4-render-test--info-at (string index)
  "Return the `lean4-info' property at INDEX of STRING."
  (get-text-property index 'lean4-info string))

(defun lean4-render-test--pos-at (string index)
  "Return the `lean4-subexpr-pos' property at INDEX of STRING."
  (get-text-property index 'lean4-subexpr-pos string))

;;;; TaggedText

(ert-deftest lean4-render-text-node ()
  "A bare text node renders as its string."
  (should (equal (lean4-render-tagged-text '(:text "hello")) "hello")))

(ert-deftest lean4-render-append-node ()
  "An append node concatenates its children in order."
  (should (equal (lean4-render-tagged-text
                  '(:append [(:text "a") (:text "b") (:text "c")]))
                 "abc")))

(ert-deftest lean4-render-unknown-node-is-empty ()
  "An unrecognised node renders as nothing rather than signalling.
Lean grows new payload shapes between releases; a goal display that
errors is worse than one that omits something."
  (should (equal (lean4-render-tagged-text '(:someFutureThing 42)) ""))
  (should (equal (lean4-render-tagged-text nil) "")))

(ert-deftest lean4-render-produces-the-expected-text ()
  "The captured payload renders to the proposition it stands for."
  (should (equal (substring-no-properties
                  (lean4-render-tagged-text lean4-render-test--one-plus-one))
                 "1 + 1 = 2")))

(ert-deftest lean4-render-innermost-tag-wins ()
  "Every character carries the innermost subterm covering it.

This is the property the whole interactive display rests on: hovering
the first `1' must report that numeral, not the sum it sits in, and not
the equation the sum sits in."
  (let ((rendered (lean4-render-tagged-text lean4-render-test--one-plus-one)))
    ;; "1 + 1 = 2"
    ;;  ^        index 0: the left numeral, deepest path
    (should (equal (lean4-render-test--pos-at rendered 0) "/0/1/0/1"))
    ;;   ^       index 1: the space, covered only by the sum
    (should (equal (lean4-render-test--pos-at rendered 1) "/0/1"))
    ;;     ^     index 4: the right numeral
    (should (equal (lean4-render-test--pos-at rendered 4) "/0/1/1"))
    ;;      ^    index 5: " = " belongs to the equation alone
    (should (equal (lean4-render-test--pos-at rendered 5) "/"))
    ;;         ^ index 8: the right-hand side
    (should (equal (lean4-render-test--pos-at rendered 8) "/1"))))

(ert-deftest lean4-render-every-character-is-covered ()
  "No character is left without a subterm."
  (let ((rendered (lean4-render-tagged-text lean4-render-test--one-plus-one)))
    (dotimes (index (length rendered))
      (should (lean4-render-test--pos-at rendered index))
      (should (lean4-render-test--info-at rendered index)))))

(ert-deftest lean4-render-info-matches-position ()
  "The reference recorded beside a position is that subterm's own."
  (let ((rendered (lean4-render-tagged-text lean4-render-test--one-plus-one)))
    (should (equal (lean4-render-test--info-at rendered 0) '(:__rpcref "2")))
    (should (equal (lean4-render-test--info-at rendered 4) '(:__rpcref "3")))
    (should (equal (lean4-render-test--info-at rendered 8) '(:__rpcref "4")))))

;;;; Diff status

(ert-deftest lean4-render-diff-status-is-faced ()
  "A subterm marked as changed by a tactic is highlighted."
  (let ((rendered (lean4-render-tagged-text
                   '(:tag [(:info (:__rpcref "0") :subexprPos "/"
                            :diffStatus "wasInserted")
                           (:text "new")]))))
    (should (memq 'lean4-goal-inserted
                  (ensure-list (get-text-property 0 'font-lock-face rendered))))))

(ert-deftest lean4-render-unknown-diff-status-is-ignored ()
  "A `diffStatus' we do not recognise leaves the text unfaced."
  (let ((rendered (lean4-render-tagged-text
                   '(:tag [(:info (:__rpcref "0") :subexprPos "/"
                            :diffStatus "somethingNew")
                           (:text "x")]))))
    (should-not (get-text-property 0 'font-lock-face rendered))))

;;;; Subterm paths

(ert-deftest lean4-render-subexpr-path-splits ()
  "A subterm position splits into its steps."
  (should (equal (lean4-render-subexpr-path "/0/1") '("0" "1")))
  (should (equal (lean4-render-subexpr-path "/") nil))
  (should (equal (lean4-render-subexpr-path nil) nil)))

(ert-deftest lean4-render-subexpr-ancestry ()
  "Containment is decided step by step, not by string prefix."
  (should (lean4-render-subexpr-ancestor-p "/0" "/0/1"))
  (should (lean4-render-subexpr-ancestor-p "/0/1" "/0/1"))
  (should-not (lean4-render-subexpr-ancestor-p "/0/1" "/0"))
  ;; The reason this is not `string-prefix-p': step 1 does not contain
  ;; step 10, but "/1" is a string prefix of "/10".
  (should-not (lean4-render-subexpr-ancestor-p "/1" "/10")))

;;;; Hypotheses and goals

(ert-deftest lean4-render-hypothesis-with-several-names ()
  "Names sharing a type are shown together, as Lean prints them."
  (let ((rendered (lean4-render-hypothesis
                   '(:names ["a" "b"] :type (:text "Nat")))))
    (should (equal (substring-no-properties rendered) "a b : Nat\n"))))

(ert-deftest lean4-render-hypothesis-with-a-value ()
  "A let-bound hypothesis shows its value."
  (let ((rendered (lean4-render-hypothesis
                   '(:names ["n"] :type (:text "Nat") :val (:text "42")))))
    (should (equal (substring-no-properties rendered) "n : Nat := 42\n"))))

(ert-deftest lean4-render-inaccessible-name-is-dimmed ()
  "A name Lean has made inaccessible is visually de-emphasised."
  (let ((rendered (lean4-render-hypothesis
                   '(:names ["a✝"] :type (:text "Nat")))))
    (should (eq (get-text-property 0 'font-lock-face rendered)
                'lean4-inaccessible-name))))

(ert-deftest lean4-render-goal-uses-the-servers-prefix ()
  "The goal is introduced by whatever prefix the server sent."
  (should (equal (substring-no-properties
                  (lean4-render-goal '(:goalPrefix "⊢ " :hyps []
                                       :type (:text "True"))))
                 "⊢ True")))

(ert-deftest lean4-render-goal-shows-its-case-label ()
  "A named goal is labelled with its case."
  (let ((rendered (lean4-render-goal
                   '(:userName "inl" :goalPrefix "⊢ " :hyps []
                     :type (:text "True")))))
    (should (string-prefix-p "case inl\n" (substring-no-properties rendered)))))

(ert-deftest lean4-render-goal-ignores-its-own-removed-flag ()
  "A goal's own `isRemoved' flag is not treated as a diff.

Lean sets `isRemoved' on every goal returned by an ordinary
`getInteractiveGoals' request -- verified against 4.32.2 -- so honouring
it would strike out every goal in the buffer.  Only per-subterm
`diffStatus' is a real diff signal."
  (let ((rendered (lean4-render-goal
                   '(:goalPrefix "⊢ " :hyps [] :isRemoved t
                     :type (:text "True")))))
    (should-not (text-property-not-all 0 (length rendered)
                                       'font-lock-face nil rendered))))

(ert-deftest lean4-render-goals-separates-and-reports-emptiness ()
  "Several goals are separated; no goals is nil, not an empty string."
  (should-not (lean4-render-goals []))
  (let ((rendered (lean4-render-goals
                   '[(:goalPrefix "⊢ " :hyps [] :type (:text "A"))
                     (:goalPrefix "⊢ " :hyps [] :type (:text "B"))])))
    (should (equal (substring-no-properties rendered) "⊢ A\n\n⊢ B"))))

;;;; Messages and traces

(defconst lean4-render-test--trace
  ;; Shape captured from Lean 4.32.2 answering getInteractiveDiagnostics on
  ;; a `set_option trace.Meta.synthInstance true' declaration.
  '(:tag [(:trace (:children (:lazy (:__rpcref "8"))
                   :cls "Meta.synthInstance"
                   :collapsed t
                   :indent 0
                   :msg (:text "Inhabited (Nat × Nat)")))
          (:text "")])
  "A real collapsed trace node with lazily fetchable children.")

(ert-deftest lean4-render-message-plain-text ()
  "A message with no embeds renders as its text."
  (should (equal (lean4-render-message '(:text "boom")) "boom"))
  (should (equal (lean4-render-message
                  '(:append [(:text "a") (:text "b")]))
                 "ab")))

(ert-deftest lean4-render-message-expr-embed-is-interactive ()
  "A term embedded in a message keeps its subterm tagging.
This is what makes a subterm of a type mismatch as hoverable as one
inside a goal."
  (let ((rendered (lean4-render-message
                   `(:tag [(:expr ,lean4-render-test--one-plus-one)
                           (:text "")]))))
    (should (equal (substring-no-properties rendered) "1 + 1 = 2"))
    (should (equal (get-text-property 0 'lean4-subexpr-pos rendered)
                   "/0/1/0/1"))))

(ert-deftest lean4-render-message-unknown-embed-falls-back ()
  "An embed we do not handle still shows the text that came with it.
Lean grows new `MsgEmbed' variants; showing nothing would be worse."
  (should (equal (lean4-render-message
                  '(:tag [(:someFutureEmbed 1) (:text "fallback")]))
                 "fallback")))

(ert-deftest lean4-render-trace-starts-collapsed ()
  "A trace the server marked collapsed renders as a header alone."
  (let ((rendered (lean4-render-message lean4-render-test--trace)))
    (should (string-search lean4-render-collapsed-marker rendered))
    (should (string-search "[Meta.synthInstance]" rendered))
    (should (string-search "Inhabited (Nat × Nat)" rendered))))

(ert-deftest lean4-render-trace-header-carries-what-unfolding-needs ()
  "The header records its path and how to obtain its children."
  (let* ((rendered (lean4-render-message lean4-render-test--trace))
         ;; The marker is indented past the leading whitespace.
         (index (string-search lean4-render-collapsed-marker rendered)))
    (should (equal (get-text-property index 'lean4-trace-children rendered)
                   '(lazy . (:__rpcref "8"))))
    (should-not (get-text-property index 'lean4-trace-open rendered))
    ;; A path is present, so an expansion table can be keyed on it.
    (should (listp (get-text-property index 'lean4-trace-path rendered)))))

(ert-deftest lean4-render-trace-expands-from-the-table ()
  "Children supplied in the expansion table are shown under the header."
  (let ((expanded (make-hash-table :test #'equal)))
    ;; The trace sits at the root of this message, so its path is ().
    (puthash '() (vector '(:text "a child")) expanded)
    (let ((rendered (lean4-render-message lean4-render-test--trace
                                          nil expanded)))
      (should (string-search lean4-render-expanded-marker rendered))
      (should (string-search "a child" rendered)))))

(ert-deftest lean4-render-trace-strict-children-open-by-default ()
  "A node whose children arrived inline, and which is not marked
collapsed, is shown open: that is how Lean asked for it to appear."
  (let ((rendered (lean4-render-message
                   '(:tag [(:trace (:children (:strict [(:text "inline")])
                                    :cls "Test" :collapsed :json-false
                                    :indent 0 :msg (:text "head")))
                           (:text "")]))))
    (should (string-search lean4-render-expanded-marker rendered))
    (should (string-search "inline" rendered))))

(ert-deftest lean4-render-trace-indents-by-depth ()
  "Nesting depth is rendered as indentation."
  (let ((rendered (lean4-render-message
                   '(:tag [(:trace (:children (:lazy (:__rpcref "1"))
                                    :cls "Test" :collapsed t
                                    :indent 2 :msg (:text "deep")))
                           (:text "")]))))
    (should (string-prefix-p "    " rendered))))

;;;; Reference collection

(ert-deftest lean4-render-collects-every-reference ()
  "Every subterm reference is collected, so it can be released."
  (let ((refs (lean4-render-collect-refs lean4-render-test--one-plus-one)))
    (should (= (length refs) 5))
    (should (member '(:__rpcref "0") refs))
    (should (member '(:__rpcref "4") refs))))

(provide 'lean4-render-test)
;;; lean4-render-test.el ends here
