;;; lean4-widget-test.el --- Tests for driving Lean's widgets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Reading an `Html' answer for the edit inside it, without running the
;; JavaScript that would have drawn it.  The trees here were recorded from a
;; real `Mathlib.Tactic.Conv.SelectionPanel.rpc' answering a real selection,
;; so what is asserted is the shape Lean actually sends rather than the shape
;; the source suggested it would.

;;; Code:

(require 'ert)
(require 'lean4-widget)

(defconst lean4-widget-test--offering
  '(:element
    ["details" [["open" t]]
     [(:element ["summary" [["className" "mv2 pointer"]] [(:text "Conv 🔍️")]])
      (:element
       ["div" [["className" "ml1"]]
        [(:component
          ["4075709796503415264" "default"
           (:edit (:edits [(:newText "conv =>\n    enter [2]\n    skip"
                            :range (:start (:line 3 :character 2)
                                    :end (:line 3 :character 7)))]
                   :textDocument (:uri "file:///Spike.lean" :version 0))
            :newSelection (:start (:line 5 :character 4)
                           :end (:line 5 :character 8)))
           [(:text "Generate conv")]])]])]])
  "What `conv?' answers when something is selected.
Recorded from a live server: the panel is an element tree, and the link
that would apply the edit is a component whose props carry it.")

(defconst lean4-widget-test--help
  '(:element
    ["details" [["open" t]]
     [(:element ["summary" [["className" "mv2 pointer"]] [(:text "Conv 🔍️")]])
      (:element ["div" [["className" "ml1"]]
                 [(:element ["span" [] [(:text "Use shift-click to select \
one sub-expression in the goal or local context that you want to zoom in \
on.")]])]])]])
  "What the same widget answers when nothing is selected.
Not an error and not an empty answer: a panel saying what to do.  There is
no edit in it, and offering the reader one would be inventing something.")

(ert-deftest lean4-widget-the-method-is-the-widget-id-and-rpc ()
  "`mk_rpc_widget% X.rpc' is how a widget with a server half is written,
so the method to call is the id `getWidgets' reports with `.rpc' after it.
This is Lean's own convention rather than anything the protocol
guarantees, which is why it is asserted where a change would be noticed."
  (should (equal (lean4-widget--rpc-method
                  '(:id "Mathlib.Tactic.Conv.SelectionPanel"))
                 "Mathlib.Tactic.Conv.SelectionPanel.rpc"))
  ;; A widget with no id is not one this can drive, and says so by
  ;; answering nil rather than by building a nonsense method name.
  (should-not (lean4-widget--rpc-method '(:javascriptHash "123"))))

(ert-deftest lean4-widget-an-offer-is-the-edit-and-what-the-link-said ()
  "The component carrying an `edit' is the link a reader would have
clicked; its children are what the link read as, which is what names it
when a widget offers more than one."
  (let ((offers (lean4-widget--offers lean4-widget-test--offering)))
    (should (= (length offers) 1))
    (should (equal (caar offers) "Generate conv"))
    (let ((edits (plist-get (cdar offers) :edits)))
      (should (= (length edits) 1))
      (should (equal (plist-get (elt edits 0) :newText)
                     "conv =>\n    enter [2]\n    skip")))))

(ert-deftest lean4-widget-help-text-is-not-an-offer ()
  "A panel that says what to do is not a panel offering to do it.
Reading an edit out of this one would mean inventing it, and applying an
invented edit to a proof is the worst thing this code could do."
  (should-not (lean4-widget--offers lean4-widget-test--help)))

(ert-deftest lean4-widget-nothing-is-read-from-nothing ()
  "The walk answers for the shapes a tree bottoms out in rather than
signalling, so an `Html' from a newer Lean degrades instead of erroring."
  (should-not (lean4-widget--offers nil))
  (should-not (lean4-widget--offers '(:text "just words")))
  (should-not (lean4-widget--offers "a bare string")))

(ert-deftest lean4-widget-arrays-are-sent-as-arrays ()
  "`goals' and `selectedLocations' are `Array' on the wire and are held
here as lists.

Regression test.  A list reaches `json-serialize' as an alist and goes out
as a JSON object; the server could not decode it, and answered with the
help text it shows when nothing is selected -- so the failure looked
exactly like a selection the widget did not care for, which is the one
thing it must not be confused with."
  (let* ((params (lean4-widget--params
                  '(:props (:replaceRange (:start (:line 3 :character 2)
                                           :end (:line 3 :character 7))))
                  '(:line 3 :character 2)
                  (list '(:mvarId "_uniq.59"))
                  (list '(:mvarId "_uniq.59" :loc (:target "/1")))))
         (json (json-serialize params)))
    (should (vectorp (plist-get params :goals)))
    (should (vectorp (plist-get params :selectedLocations)))
    (should (string-search "\"goals\":[{" json))
    (should (string-search "\"selectedLocations\":[{" json))))

(ert-deftest lean4-widget-the-replace-range-comes-from-the-widget ()
  "The widget says what its answer would replace -- the `conv?' token --
and that range is passed back to it rather than worked out here."
  (let ((params (lean4-widget--params
                 '(:props (:replaceRange (:start (:line 3 :character 2)
                                          :end (:line 3 :character 7))))
                 nil nil nil)))
    (should (equal (plist-get params :replaceRange)
                   '(:start (:line 3 :character 2)
                     :end (:line 3 :character 7))))))

(provide 'lean4-widget-test)
;;; lean4-widget-test.el ends here
