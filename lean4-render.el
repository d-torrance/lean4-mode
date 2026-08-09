;;; lean4-render.el --- Render Lean's interactive goal trees -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jan Mas Rovira
;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: MIT

;; Derived from neo-lean-mode <https://github.com/janmasrovira/neo-lean-mode>,
;; file neo-lean-render.el, by Jan Mas Rovira, which is MIT licensed; see
;; LICENSES/MIT.txt.

;;; Commentary:

;; Turns the trees Lean returns over RPC into propertized strings.
;;
;; This library is pure: it takes plists and returns strings, touches no
;; buffer, and knows nothing about Eglot, jsonrpc or the session layer.  That
;; is what makes the interesting part -- which character carries which
;; subterm -- testable without a Lean toolchain.
;;
;; A `CodeWithInfos' is a `TaggedText SubexprInfo': a tree of
;;
;;   (:text STRING)
;;   (:append [TAGGED-TEXT ...])
;;   (:tag [SUBEXPR-INFO TAGGED-TEXT])
;;
;; where each tag names the subterm the enclosed text stands for.  Rendering
;; flattens the tree to a string and records, on every character, the
;; innermost subterm covering it.  Two text properties carry that:
;;
;;   `lean4-info'        a reference the server understands, used to ask for
;;                       the subterm's type and docstring, or where it is
;;                       defined
;;   `lean4-subexpr-pos' Lean's own path for the subterm, like "/0/1"
;;
;; The paths are what make containment cheap: "/0/1/0" is inside "/0/1"
;; exactly when one is a path-prefix of the other, so highlighting the
;; subterm under point is string work rather than another round trip.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Faces

(defgroup lean4-render nil
  "Faces for Lean's interactive goal display."
  :group 'lean4)

(defface lean4-goal-case
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for a goal's case label."
  :group 'lean4-render)

(defface lean4-hypothesis-name
  '((t :inherit font-lock-variable-name-face))
  "Face for the names of hypotheses."
  :group 'lean4-render)

(defface lean4-inaccessible-name
  '((t :inherit font-lock-comment-face))
  "Face for hypothesis names Lean has made inaccessible."
  :group 'lean4-render)

(defface lean4-goal-inserted
  '((t :inherit diff-added))
  "Face for a subterm a tactic introduced."
  :group 'lean4-render)

(defface lean4-goal-removed
  '((t :inherit diff-removed))
  "Face for a subterm a tactic will remove."
  :group 'lean4-render)

(defface lean4-goal-changed
  '((t :inherit diff-changed))
  "Face for a subterm a tactic rewrote."
  :group 'lean4-render)

(defconst lean4-render--diff-faces
  '(("wasInserted" . lean4-goal-inserted)
    ("willInsert"  . lean4-goal-inserted)
    ("wasDeleted"  . lean4-goal-removed)
    ("willDelete"  . lean4-goal-removed)
    ("wasChanged"  . lean4-goal-changed)
    ("willChange"  . lean4-goal-changed))
  "Lean's `diffStatus' values and the face each calls for.")

;;;; Tagged text

(defun lean4-render--apply-info (string info position diff)
  "Attach INFO, POSITION and DIFF to the parts of STRING that lack them.

Rendering is innermost-first, so by the time an enclosing tag gets to
decorate its text the nested tags have already claimed the characters
they cover.  Filling only the gaps is what leaves every character
carrying the *most specific* subterm containing it, which is what makes
hovering a numeral inside a sum report the numeral and not the sum."
  (let ((length (length string))
        (start 0))
    (while (< start length)
      (let ((end (or (next-single-property-change start 'lean4-info string)
                     length)))
        (unless (get-text-property start 'lean4-info string)
          (when info
            (put-text-property start end 'lean4-info info string))
          (when position
            (put-text-property start end 'lean4-subexpr-pos position string))
          (when-let* ((face (cdr (assoc diff lean4-render--diff-faces))))
            (font-lock-prepend-text-property start end 'font-lock-face
                                             face string)))
        (setq start end)))
    string))

(defun lean4-render-tagged-text (tagged-text)
  "Render TAGGED-TEXT, a `TaggedText', as a propertized string.
Returns the empty string for anything unrecognised, so that a payload
from a newer Lean than we know about degrades rather than errors."
  (cond
   ((null tagged-text) "")
   ((stringp tagged-text) tagged-text)
   ((plist-member tagged-text :text)
    (or (plist-get tagged-text :text) ""))
   ((plist-member tagged-text :append)
    (mapconcat #'lean4-render-tagged-text
               (append (plist-get tagged-text :append) nil)
               ""))
   ((plist-member tagged-text :tag)
    (let* ((tag (plist-get tagged-text :tag))
           (subexpr (elt tag 0))
           (inner (elt tag 1))
           ;; Render the inside first; see `lean4-render--apply-info'.
           (string (copy-sequence (lean4-render-tagged-text inner))))
      (lean4-render--apply-info string
                                (plist-get subexpr :info)
                                (plist-get subexpr :subexprPos)
                                (plist-get subexpr :diffStatus))))
   (t "")))

;;;; Subterm paths

(defun lean4-render-subexpr-path (position)
  "Split a Lean subterm POSITION such as \"/0/1\" into a list of steps."
  (when (stringp position)
    (seq-remove #'string-empty-p (split-string position "/"))))

(defun lean4-render-subexpr-ancestor-p (ancestor descendant)
  "Return non-nil if subterm ANCESTOR contains DESCENDANT.
Both are Lean subterm positions.  Compares step by step rather than as
strings, so that \"/1\" is not treated as containing \"/10\"."
  (let ((ancestor (lean4-render-subexpr-path ancestor))
        (descendant (lean4-render-subexpr-path descendant)))
    (and ancestor descendant
         (<= (length ancestor) (length descendant))
         (equal ancestor (seq-take descendant (length ancestor))))))

;;;; Hypotheses and goals

(defconst lean4-render--inaccessible-suffix "✝"
  "Character Lean appends to a hypothesis name you may not refer to.")

(defun lean4-render--name (name)
  "Render a hypothesis NAME, dimming it if it is inaccessible."
  (propertize name 'font-lock-face
              (if (string-search lean4-render--inaccessible-suffix name)
                  'lean4-inaccessible-name
                'lean4-hypothesis-name)))

(defun lean4-render-hypothesis (hypothesis)
  "Render HYPOTHESIS, an `InteractiveHypothesisBundle', as one line.
Names sharing a type are reported together and are shown together, the
way Lean itself prints them."
  (let* ((names (append (plist-get hypothesis :names) nil))
         (type (lean4-render-tagged-text (plist-get hypothesis :type)))
         (value (plist-get hypothesis :val)))
    (concat (mapconcat #'lean4-render--name names " ")
            " : " type
            (when value
              (concat " := " (lean4-render-tagged-text value)))
            "\n")))

(defun lean4-render-goal (goal)
  "Render GOAL, an `InteractiveGoal', as a block of text.

Deliberately ignores GOAL's own `isInserted'/`isRemoved' flags: the
server sets `isRemoved' on every goal it returns from an ordinary
`getInteractiveGoals' request, so honouring them would strike out
perfectly good goals.  Only per-subterm `diffStatus', which
`lean4-render-tagged-text' applies, is a real diff signal."
  (let ((case-label (plist-get goal :userName))
        (prefix (or (plist-get goal :goalPrefix) "⊢ "))
        (hypotheses (append (plist-get goal :hyps) nil))
        (type (lean4-render-tagged-text (plist-get goal :type))))
    (concat
     (when case-label
       (concat (propertize (format "case %s" case-label)
                           'font-lock-face 'lean4-goal-case)
               "\n"))
     (mapconcat #'lean4-render-hypothesis hypotheses "")
     prefix type)))

(defun lean4-render-goals (goals)
  "Render GOALS, a sequence of `InteractiveGoal', separated by blank lines.
Returns nil when there are none, which the caller should report as the
proof being finished rather than as an absence of information."
  (let ((goals (append goals nil)))
    (when goals
      (mapconcat #'lean4-render-goal goals "\n\n"))))

(defun lean4-render-term-goal (term-goal)
  "Render TERM-GOAL, an `InteractiveTermGoal'."
  (when term-goal
    (lean4-render-goal term-goal)))

;;;; Collecting references

(defun lean4-render-collect-refs (tagged-text)
  "Return every subterm reference appearing in TAGGED-TEXT.
The caller owns these: Lean reference counts what it hands out and
expects each one back through `$/lean/rpc/release'."
  (let (refs)
    (letrec ((walk
              (lambda (node)
                (cond
                 ((null node))
                 ((plist-member node :append)
                  (seq-doseq (child (plist-get node :append))
                    (funcall walk child)))
                 ((plist-member node :tag)
                  (let ((tag (plist-get node :tag)))
                    (when-let* ((info (plist-get (elt tag 0) :info)))
                      (push info refs))
                    (funcall walk (elt tag 1))))))))
      (funcall walk tagged-text))
    (nreverse refs)))

(provide 'lean4-render)
;;; lean4-render.el ends here
