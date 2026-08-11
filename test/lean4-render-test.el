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

;;;; How much of a goal to show

(defconst lean4-render-test--mixed-hyps
  ;; One of each kind the filters distinguish.  `isInstance' and `isType'
  ;; arrive as JSON booleans; `:json-false' is how a false one reaches Emacs.
  '[(:names ["α"] :type (:text "Type") :isType t)
    (:names ["inst"] :type (:text "Monoid α") :isInstance t)
    (:names ["n"] :type (:text "Nat") :val (:text "42"))
    (:names ["h" "h✝"] :type (:text "P") :isType :json-false)
    (:names ["k✝"] :type (:text "Q"))]
  "Hypotheses covering every distinction the display settings make.")

(defun lean4-render-test--goal (&rest settings)
  "Render a goal over `lean4-render-test--mixed-hyps' under SETTINGS."
  (substring-no-properties
   (lean4-render-goal `(:goalPrefix "⊢ " :hyps ,lean4-render-test--mixed-hyps
                        :type (:text "True"))
                      settings)))

(ert-deftest lean4-render-shows-everything-by-default ()
  "With no settings the whole goal is shown, as VS Code shows it."
  (should (equal (lean4-render-test--goal)
                 (concat "α : Type\n"
                         "inst : Monoid α\n"
                         "n : Nat := 42\n"
                         "h h✝ : P\n"
                         "k✝ : Q\n"
                         "⊢ True"))))

(ert-deftest lean4-render-hides-type-assumptions ()
  "Hiding type assumptions drops the whole bundle."
  (let ((rendered (lean4-render-test--goal :hide-type-assumptions t)))
    (should-not (string-search "α : Type" rendered))
    ;; A bundle whose `isType' is JSON false stays.
    (should (string-search "h h✝ : P" rendered))
    (should (string-search "inst : Monoid α" rendered))))

(ert-deftest lean4-render-hides-instance-assumptions ()
  "Hiding instances drops the whole bundle and nothing else."
  (let ((rendered (lean4-render-test--goal :hide-instance-assumptions t)))
    (should-not (string-search "inst" rendered))
    (should (string-search "α : Type" rendered))))

(ert-deftest lean4-render-hides-inaccessible-names ()
  "Inaccessible names go, and a bundle left nameless goes with them."
  (let ((rendered (lean4-render-test--goal :hide-inaccessible-assumptions t)))
    ;; One name of two: the bundle stays, with the dagger gone.
    (should (string-search "h : P" rendered))
    (should-not (string-search "h✝" rendered))
    ;; The only name: the bundle would render as " : Q", so it goes entirely.
    (should-not (string-search "Q" rendered))))

(ert-deftest lean4-render-hides-let-values ()
  "A let-binder keeps its type and loses its value."
  (let ((rendered (lean4-render-test--goal :hide-let-values t)))
    (should (string-search "n : Nat\n" rendered))
    (should-not (string-search "42" rendered))))

(ert-deftest lean4-render-hides-goal-names ()
  "The case label goes when goal names are hidden."
  (let ((goal '(:userName "inl" :goalPrefix "⊢ " :hyps []
                :type (:text "True"))))
    (should (string-prefix-p "case inl"
                             (substring-no-properties
                              (lean4-render-goal goal))))
    (should (equal (substring-no-properties
                    (lean4-render-goal goal '(:hide-goal-names t)))
                   "⊢ True"))))

(ert-deftest lean4-render-target-can-come-first ()
  "The target moves above the hypotheses, which reverse with it.
Reversing keeps the target next to the hypothesis it was next to."
  (should (equal (substring-no-properties
                  (lean4-render-goal
                   '(:goalPrefix "⊢ " :type (:text "True")
                     :hyps [(:names ["a"] :type (:text "A"))
                            (:names ["b"] :type (:text "B"))])
                   '(:target-first t)))
                 "⊢ True\nb : B\na : A")))

(ert-deftest lean4-render-target-first-without-hypotheses ()
  "A goal with nothing to reverse gains no stray blank line."
  (should (equal (substring-no-properties
                  (lean4-render-goal '(:goalPrefix "⊢ " :hyps []
                                       :type (:text "True"))
                                     '(:target-first t)))
                 "⊢ True")))

(ert-deftest lean4-render-emphasizes-only-the-first-goal ()
  "Goals after the first are faced down; the first is left alone."
  (let* ((goals '[(:goalPrefix "⊢ " :hyps [] :type (:text "A"))
                  (:goalPrefix "⊢ " :hyps [] :type (:text "B"))])
         (rendered (lean4-render-goals goals '(:emphasize-first-goal t)))
         (second (string-search "B" rendered)))
    (should-not (memq 'lean4-unemphasized-goal
                      (ensure-list (get-text-property 0 'font-lock-face
                                                      rendered))))
    (should (memq 'lean4-unemphasized-goal
                  (ensure-list (get-text-property second 'font-lock-face
                                                  rendered))))))

(ert-deftest lean4-render-emphasis-keeps-the-faces-underneath ()
  "The face is added beneath those a subterm already carries.
Prepending it would override the diff and name colours."
  (let* ((goals `[(:goalPrefix "⊢ " :hyps [] :type (:text "A"))
                  (:goalPrefix "⊢ " :hyps [(:names ["h✝"] :type (:text "P"))]
                   :type (:text "B"))])
         (rendered (lean4-render-goals goals '(:emphasize-first-goal t)))
         (faces (ensure-list
                 (get-text-property (string-search "h✝" rendered)
                                    'font-lock-face rendered))))
    (should (memq 'lean4-inaccessible-name faces))
    (should (memq 'lean4-unemphasized-goal faces))
    ;; The name's own face keeps priority, being first.
    (should (< (seq-position faces 'lean4-inaccessible-name)
               (seq-position faces 'lean4-unemphasized-goal)))))

(ert-deftest lean4-render-visible-hypotheses-accepts-a-vector ()
  "Lean sends an array; the filter returns a list either way."
  (should (equal (lean4-render-visible-hypotheses
                  '[(:names ["a"] :type (:text "A"))] nil)
                 '((:names ["a"] :type (:text "A"))))))

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

(ert-deftest lean4-render-trace-comes-out-as-a-part ()
  "A trace is handed back whole, for the caller to insert as it likes.

The goal display makes it a section of its own so that it folds the way
everything around it folds, which a string has nowhere to express."
  (let ((parts (lean4-render-message-parts lean4-render-test--trace)))
    (should (= (length parts) 1))
    (pcase-let ((`(trace ,node ,path) (car parts)))
      (should (equal (lean4-render-trace-children node)
                     '(lazy . (:__rpcref "8"))))
      ;; A path is present, so one trace can be told from another.
      (should (listp path)))))

(ert-deftest lean4-render-trace-header-is-class-then-message ()
  "The header line names the trace class and says what the node says."
  (let ((header (lean4-render-trace-header
                 (nth 1 (car (lean4-render-message-parts
                              lean4-render-test--trace))))))
    (should (string-search "[Meta.synthInstance]" header))
    (should (string-search "Inhabited (Nat × Nat)" header))))

(ert-deftest lean4-render-trace-paths-tell-siblings-apart ()
  "Two traces in one message get different paths.
That is what keeps `magit-section' from confusing their sections when
the display is rebuilt."
  (let* ((message `(:append [,lean4-render-test--trace
                             ,lean4-render-test--trace]))
         (parts (lean4-render-message-parts message))
         (paths (mapcar (lambda (part) (nth 2 part)) parts)))
    (should (= (length parts) 2))
    (should-not (equal (nth 0 paths) (nth 1 paths)))))

(ert-deftest lean4-render-message-flattens-a-trace-to-its-header ()
  "Rendered as a string, a trace is its header and nothing under it.
A string has nowhere to put children that fold."
  (let ((rendered (lean4-render-message lean4-render-test--trace)))
    (should (string-search "[Meta.synthInstance]" rendered))
    (should-not (string-search "\n" rendered))))

;;;; Reference collection

(ert-deftest lean4-render-collects-every-reference ()
  "Every subterm reference is collected, so it can be released."
  (let ((refs (lean4-render-collect-refs lean4-render-test--one-plus-one)))
    (should (= (length refs) 5))
    (should (member '(:__rpcref "0") refs))
    (should (member '(:__rpcref "4") refs))))

;;;; What a search found

(ert-deftest lean4-render-faces-a-search-match ()
  "`Lean.Widget.highlightMatches' marks what it found with a tag carrying
nothing -- the protocol writes a constructor with no fields as its bare
name -- around the words that matched.  They are faced as a match is faced
everywhere else in Emacs."
  (let ((rendered (lean4-render-tagged-text
                   '(:append [(:text "@inst")
                              (:tag ["highlighted" (:text "Inhabited")])
                              (:text "Prod")]))))
    (should (equal rendered "@instInhabitedProd"))
    ;; Prepended rather than set, so the property is a list of faces: a
    ;; match inside a subterm keeps whatever the subterm put there.
    (should (member 'lean4-render-match
                    (get-text-property (string-search "Inhabited" rendered)
                                       'font-lock-face rendered)))
    (should-not (get-text-property 0 'font-lock-face rendered))))

(ert-deftest lean4-render-faces-a-match-in-a-message ()
  "And in the words of a message, not only inside a term."
  (let ((parts (lean4-render-message-parts
                '(:append [(:text "found ")
                           (:tag ["highlighted" (:text "here")])]))))
    (should (equal (apply #'concat parts) "found here"))
    (should (member 'lean4-render-match
                    (get-text-property 0 'font-lock-face (nth 1 parts))))))

(provide 'lean4-render-test)
;;; lean4-render-test.el ends here
