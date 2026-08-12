;;; lean4-widget.el --- Driving Lean's widgets without a browser -*- lexical-binding: t -*-

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

;; A user widget is a piece of JavaScript the server hands the editor, which
;; VS Code renders in a webview.  Emacs has no webview and is not getting
;; one, so widgets are not rendered here.
;;
;; The ones worth having do not need to be.  A widget that reads a selection
;; -- Mathlib's `conv?', `gcongr?', `calc?' -- is a thin shell over a
;; `@[server_rpc_method]' which does the work and answers with an `Html'
;; tree; the JavaScript renders that tree and, when a link in it is clicked,
;; applies an edit the tree already carried.  Both halves are JSON.  So the
;; work can be had by calling the method and reading the answer, which is
;; what this file does.
;;
;; The one convention relied on is Lean's own: `mk_rpc_widget%' pairs a
;; widget `X' with a method `X.rpc', and `Lean.Widget.getWidgets' reports
;; each instance's `id'.  So the method to call is the widget's id with
;; `.rpc' after it.  A widget not built that way answers nothing and is
;; skipped, which is the right outcome: this offers what it can drive and
;; stays quiet about the rest.

;;; Code:

(require 'seq)

(require 'lean4-rpc)

(declare-function eglot--apply-text-edits "eglot")

(defun lean4-widget--rpc-method (widget)
  "Return the RPC method WIDGET's panel is driven by.
`mk_rpc_widget% X.rpc' is how a widget with a server half is written, and
`X' is the id `Lean.Widget.getWidgets' reports, so the method is that id
with `.rpc' after it."
  (when-let* ((id (plist-get widget :id)))
    (concat id ".rpc")))

(defun lean4-widget--params (widget position goals selection)
  "Return the parameters WIDGET\\='s method wants.

`SelectInsertParams' -- the shape every selection-reading widget takes --
is the cursor POSITION, the tactic GOALS, the SELECTION, and the range in
the file the answer would replace, which the widget itself supplies.

GOALS are passed back exactly as they arrived.  They carry RPC references
to the elaborator\\='s own state, which are meaningful only to the session
that issued them; rebuilding them here would produce something the server
could not use."
  (list :pos position
        ;; Both of these are `Array' on the wire, and the display holds
        ;; them as lists.  A list reaches `json-serialize' as an alist and
        ;; goes out as an object, which the server reads as no goals and no
        ;; selection -- and answers, quite reasonably, with the help text
        ;; it shows when nothing is selected.
        :goals (vconcat goals)
        :selectedLocations (vconcat selection)
        :replaceRange (plist-get (plist-get widget :props) :replaceRange)))

(defun lean4-widget--offers (html)
  "Return what HTML offers as a list of (LABEL . EDIT).

An `Html' tree is `:text', `:element' or `:component'.  A component whose
props carry an `edit' is a link that would apply that edit when clicked;
its children say what the link reads as.  Everything else is presentation
-- a summary line, a div, the panel title -- and is walked through rather
than shown, there being no way to show it and nothing lost by not.

The edit is an LSP `TextDocumentEdit', which is what makes this worth
doing: Eglot already knows how to apply one."
  (let ((offers nil))
    (cond
     ((not (listp html)) nil)
     ((plist-member html :component)
      (let* ((component (plist-get html :component))
             (props (elt component 2))
             (children (elt component 3))
             (edit (plist-get props :edit)))
        (when edit
          (push (cons (or (lean4-widget--text-of children) "Apply") edit)
                offers))
        (setq offers (append offers (lean4-widget--offers-in children)))))
     ((plist-member html :element)
      (setq offers (lean4-widget--offers-in (elt (plist-get html :element) 2)))))
    offers))

(defun lean4-widget--offers-in (children)
  "Return the offers of every member of CHILDREN."
  (seq-mapcat #'lean4-widget--offers (append children nil)))

(defun lean4-widget--text (html)
  "Return the plain words of HTML, as a string."
  (cond
   ((or (null html) (not (listp html))) "")
   ((plist-member html :text) (or (plist-get html :text) ""))
   ((plist-member html :element)
    (lean4-widget--words (elt (plist-get html :element) 2)))
   ((plist-member html :component)
    (lean4-widget--words (elt (plist-get html :component) 3)))
   (t "")))

(defun lean4-widget--words (children)
  "Return the plain words of CHILDREN, a sequence of `Html', as a string."
  (mapconcat #'lean4-widget--text (append children nil) ""))

(defun lean4-widget--text-of (children)
  "Return what CHILDREN read as, or nil if they read as nothing.
What a link says -- \"Generate conv\" and the like -- which is what names
it when there is more than one to choose between.  CHILDREN arrive as a
vector, so this is not `lean4-widget--text' of one node."
  (let ((text (string-trim (lean4-widget--words children))))
    (unless (string-empty-p text) text)))

(defun lean4-widget-apply-edit (edit)
  "Apply EDIT, an LSP `TextDocumentEdit', to the buffer it names.
The version is deliberately not checked: the document has not changed
since the goals were fetched -- editing it is what would have thrown the
selection away -- and Lean reports version 0 for a file it has only
read."
  (when-let* ((edits (plist-get edit :edits)))
    (eglot--apply-text-edits (append edits nil))))

(provide 'lean4-widget)
;;; lean4-widget.el ends here
