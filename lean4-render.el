;;; lean4-render.el --- Render Lean's interactive goal trees -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jan Mas Rovira
;; Copyright (C) 2026 Doug Torrance

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

(defface lean4-unemphasized-goal
  '((t :inherit shadow :height 0.9))
  "Face for the goals after the first, when the first is emphasized.
VS Code draws these at seventy percent opacity and a smaller font size."
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

(defface lean4-render-match
  '((t :inherit match))
  "Face for the text a trace search found.
`match' is what Emacs uses for a search hit everywhere else -- isearch's
lazy highlight, `occur', `grep' -- so a hit in a trace looks like a hit."
  :group 'lean4-info)

(defconst lean4-render--highlight-tag "highlighted"
  "How `Lean.Widget.highlightMatches' marks the text a search found.
A tag whose embed carries nothing, which the protocol encodes as the bare
name of the constructor rather than as an object with a field: a
`TaggedText' whose tag is this string, wrapped around the words that
matched.  It turns up inside a term as readily as in the words around one,
the search looking into both.")

(defun lean4-render--highlight-p (embed)
  "Return non-nil if EMBED is a search match rather than a subterm."
  (equal embed lean4-render--highlight-tag))

(defun lean4-render--highlight (string)
  "Return STRING faced as a search match."
  (font-lock-prepend-text-property 0 (length string) 'font-lock-face
                                   'lean4-render-match string)
  string)

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
      (if (lean4-render--highlight-p subexpr)
          (lean4-render--highlight string)
        (lean4-render--apply-info string
                                  (plist-get subexpr :info)
                                  (plist-get subexpr :subexprPos)
                                  (plist-get subexpr :diffStatus)))))
   (t "")))

;;;; Messages
;;
;; A diagnostic's message is a `TaggedText MsgEmbed', a different tree from a
;; goal's `CodeWithInfos'.  Its tags are not subterms but embedded content:
;;
;;   (:expr  CODE-WITH-INFOS)   a term, with all the subterm tagging that
;;                              implies -- so a term inside a type mismatch
;;                              is as hoverable as one inside a goal
;;   (:goal  INTERACTIVE-GOAL)  a whole proof state
;;   (:trace TRACE)             a foldable trace node
;;   (:widget ...)              a user widget; not rendered natively
;;
;; None of these appear unless the client sent `hasWidgets' at
;; initialization; without it Lean flattens the message to text first.
;;
;; Trace nodes carry their children either inline or as a reference to be
;; fetched on demand.  This library cannot fetch, so it renders what it was
;; given and records, on the header text, everything needed to fetch the
;; rest: see `lean4-trace-children' and `lean4-trace-path'.

(defface lean4-trace-class
  '((t :inherit font-lock-type-face))
  "Face for the class name of a trace node, such as `Meta.synthInstance'."
  :group 'lean4-render)

;; Deliberately not the chevrons `magit-section' uses for its own headings
;; -- on a terminal `magit-section-visibility-indicator' is ("▸" . "▾"), and
;; a trace that folds is a different thing from a section that folds.
(defun lean4-render-trace-children (trace)
  "Return the children of TRACE as (KIND . VALUE).
KIND is `strict' when the children came with the message and `lazy' when
they have to be fetched; VALUE is the child array or the reference."
  (let ((children (plist-get trace :children)))
    (cond ((plist-member children :strict)
           (cons 'strict (plist-get children :strict)))
          ((plist-member children :lazy)
           (cons 'lazy (plist-get children :lazy))))))

(defun lean4-render-trace-header (trace)
  "Return the line that stands for TRACE: its class, then its own message.

Only the line.  What hangs under it is inserted by whoever is building
the display, as a section of its own, so that a trace folds the way
everything else in that display folds."
  (let ((class (plist-get trace :cls)))
    (concat (when class
              (concat (propertize (format "[%s]" class)
                                  'font-lock-face 'lean4-trace-class)
                      " "))
            (lean4-render-message (plist-get trace :msg)))))


(defun lean4-render-message-parts (message &optional path)
  "Return MESSAGE, a `TaggedText MsgEmbed', as a list of parts.

A part is either a propertized string or (trace NODE PATH), which the
caller inserts however it likes -- as a section of its own, in the goal
display, so that a trace folds the way everything around it folds.
PATH identifies the node within the enclosing message, and tells one
trace from another when the display is rebuilt."
  (let ((path (or path '())))
    (cond
     ((null message) nil)
     ((stringp message) (list message))
     ((plist-member message :text) (list (or (plist-get message :text) "")))
     ((plist-member message :append)
      (let ((index -1))
        (mapcan (lambda (child)
                  (setq index (1+ index))
                  (lean4-render-message-parts child (cons index path)))
                (append (plist-get message :append) nil))))
     ((plist-member message :tag)
      (let* ((tag (plist-get message :tag))
             (embed (elt tag 0))
             (inner (elt tag 1)))
        (cond
         ;; First: this embed is a bare string, and `plist-member' signals
         ;; on one rather than answering no.
         ((lean4-render--highlight-p embed)
          (mapcar (lambda (part)
                    (if (stringp part)
                        (lean4-render--highlight (copy-sequence part))
                      part))
                  (lean4-render-message-parts inner (cons 0 path))))
         ;; A term: hand it to the goal renderer, so its subterms carry the
         ;; same properties they would inside a goal.
         ((plist-member embed :expr)
          (list (lean4-render-tagged-text (plist-get embed :expr))))
         ((plist-member embed :goal)
          (list (lean4-render-goal (plist-get embed :goal))))
         ((plist-member embed :trace)
          (list (list 'trace (plist-get embed :trace) path)))
         (t (lean4-render-message-parts inner (cons 0 path))))))
     (t nil))))

(defun lean4-render-message (message &optional path)
  "Render MESSAGE, a `TaggedText MsgEmbed', as a propertized string.

Traces come out as their header line alone, since a string has nowhere
to put what hangs under one; `lean4-render-message-parts' is the form to
use where that matters.  PATH identifies MESSAGE within an enclosing
message."
  (mapconcat (lambda (part)
               (if (stringp part)
                   part
                 (lean4-render-trace-header (nth 1 part))))
             (lean4-render-message-parts message path)
             ""))


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

(defun lean4-render--inaccessible-p (name)
  "Return non-nil if NAME is one Lean has made inaccessible."
  (string-search lean4-render--inaccessible-suffix name))

(defun lean4-render--name (name)
  "Render a hypothesis NAME, dimming it if it is inaccessible."
  (propertize name 'font-lock-face
              (if (lean4-render--inaccessible-p name)
                  'lean4-inaccessible-name
                'lean4-hypothesis-name)))

;; The SETTINGS every function below takes is a plist saying how much of a
;; goal to show.  Every key is false by default, so nil means what VS Code
;; shows out of the box; `lean4-info--goal-settings' is what fills it in from
;; the options the reader sets.
;;
;;   `:hide-type-assumptions'          leave out hypotheses that are types
;;   `:hide-instance-assumptions'      leave out typeclass instances
;;   `:hide-inaccessible-assumptions'  leave out names Lean has hidden
;;   `:hide-let-values'                show a let-binder's type but not its value
;;   `:hide-goal-names'                leave out the "case foo" label
;;   `:target-first'                   the target above the hypotheses
;;   `:emphasize-first-goal'           draw the goals after the first smaller

(defun lean4-render--flag (object key)
  "Return non-nil if OBJECT's KEY is JSON true.
Lean sends these as optional booleans, and a JSON `false' does not reach
Emacs as nil, so an explicit comparison is needed."
  (eq (plist-get object key) t))

(defun lean4-render--hypothesis-names (hypothesis settings)
  "Return the names of HYPOTHESIS that SETTINGS leave visible."
  (let ((names (append (plist-get hypothesis :names) nil)))
    (if (plist-get settings :hide-inaccessible-assumptions)
        (seq-remove #'lean4-render--inaccessible-p names)
      names)))

(defun lean4-render-visible-hypotheses (hypotheses settings)
  "Return the members of HYPOTHESES that SETTINGS leave visible.

A bundle whose names have all been filtered away goes too: it would
otherwise render as a nameless \" : T\".  This is the order VS Code
applies the filters in, and it matters -- an instance is dropped whole,
whereas an inaccessible name is dropped from a bundle that may have
other names in it."
  (seq-filter
   (lambda (hypothesis)
     (and (not (and (plist-get settings :hide-instance-assumptions)
                    (lean4-render--flag hypothesis :isInstance)))
          (not (and (plist-get settings :hide-type-assumptions)
                    (lean4-render--flag hypothesis :isType)))
          (lean4-render--hypothesis-names hypothesis settings)))
   (append hypotheses nil)))

(defun lean4-render--locate (string kind &optional fvar-id)
  "Return STRING with the goal-location context of KIND on every character.

A `GoalsLocation' names a place inside a goal, and Lean\\='s
`SubExpr.GoalLocation' distinguishes four: a hypothesis itself, a
subexpression of its type, one of its value, and one of the target.
`lean4-render-tagged-text' has already put the subexpression on each
character as `lean4-subexpr-pos'; this says which of the four it is a
subexpression *of*, which the position alone does not.

KIND is `hyp', `hypType', `hypValue' or `target', spelt as Lean spells
its constructors, since that is what goes on the wire.  FVAR-ID names the
hypothesis for the three that have one.  The goal\\='s own `mvarId' is put
on by `lean4-render-goal', which is the only place that knows it."
  (let ((string (copy-sequence string)))
    (put-text-property 0 (length string) 'lean4-goal-part kind string)
    (when fvar-id
      (put-text-property 0 (length string) 'lean4-fvar-id fvar-id string))
    string))

(defun lean4-render-hypothesis (hypothesis &optional settings)
  "Render HYPOTHESIS, an `InteractiveHypothesisBundle', as one line.
Names sharing a type are reported together and are shown together, the
way Lean itself prints them.  SETTINGS is as described above.

Each part is marked with the goal location it stands for, so that a
reader can select a subexpression of the type without selecting the name
beside it.  A bundle names several hypotheses at once and carries an
`fvarIds' for each; the type is one expression shared between them, and
Lean\\='s own InfoView attributes it to the first, there being no way to
say \"the type of all of these\"."
  (let* ((names (lean4-render--hypothesis-names hypothesis settings))
         (fvar-id (elt (append (plist-get hypothesis :fvarIds) nil) 0))
         (type (lean4-render--locate
                (lean4-render-tagged-text (plist-get hypothesis :type))
                "hypType" fvar-id))
         (value (unless (plist-get settings :hide-let-values)
                  (plist-get hypothesis :val))))
    (concat (lean4-render--locate
             (mapconcat #'lean4-render--name names " ") "hyp" fvar-id)
            " : " type
            (when value
              (concat " := "
                      (lean4-render--locate
                       (lean4-render-tagged-text value)
                       "hypValue" fvar-id)))
            "\n")))

(defun lean4-render-goal (goal &optional settings)
  "Render GOAL, an `InteractiveGoal', as a block of text.
SETTINGS is as described above.

Deliberately ignores GOAL's own `isInserted'/`isRemoved' flags: the
server sets `isRemoved' on every goal it returns from an ordinary
`getInteractiveGoals' request, so honouring them would strike out
perfectly good goals.  Only per-subterm `diffStatus', which
`lean4-render-tagged-text' applies, is a real diff signal."
  (let* ((case-label (unless (plist-get settings :hide-goal-names)
                       (plist-get goal :userName)))
         (prefix (or (plist-get goal :goalPrefix) "⊢ "))
         (hypotheses (lean4-render-visible-hypotheses
                      (plist-get goal :hyps) settings))
         (type (lean4-render--locate
                (lean4-render-tagged-text (plist-get goal :type)) "target"))
         (target (concat prefix type))
         ;; VS Code reverses the hypotheses as well as moving the target
         ;; above them, so that the two lines which were adjacent -- the
         ;; last hypothesis and the target -- stay adjacent.
         (hypotheses (if (plist-get settings :target-first)
                         (reverse hypotheses)
                       hypotheses))
         (body (mapconcat (lambda (hypothesis)
                            (lean4-render-hypothesis hypothesis settings))
                          hypotheses ""))
         (rendered
          (concat
           (when case-label
             (concat (propertize (format "case %s" case-label)
                                 'font-lock-face 'lean4-goal-case)
                     "\n"))
           (if (plist-get settings :target-first)
               ;; Each hypothesis ends in a newline and the target does
               ;; not, so swapping them means moving one newline as well --
               ;; and there is none to move when every hypothesis has been
               ;; filtered away.
               (if (string-empty-p body)
                   target
                 (concat target "\n" (string-remove-suffix "\n" body)))
             (concat body target)))))
    ;; The goal this text belongs to, on all of it.  A `GoalsLocation' is
    ;; an `mvarId' and a place within that goal, and this is the only
    ;; place that knows which goal is being rendered: the parts below
    ;; have said which *kind* of place they are, but not of what.
    (when-let* ((mvar-id (plist-get goal :mvarId)))
      (put-text-property 0 (length rendered) 'lean4-mvar-id mvar-id rendered))
    rendered))

(defun lean4-render-goals (goals &optional settings)
  "Render GOALS, a sequence of `InteractiveGoal', separated by blank lines.
SETTINGS is as described above.

Returns nil when there are none, which the caller should report as the
proof being finished rather than as an absence of information."
  (let ((goals (append goals nil)))
    (when goals
      (let ((rendered (seq-map-indexed
                       (lambda (goal index)
                         (let ((text (lean4-render-goal goal settings)))
                           (when (and (> index 0)
                                      (plist-get settings :emphasize-first-goal))
                             ;; Appended rather than prepended, so the faces
                             ;; the subterms already carry keep their colours
                             ;; and only the size and dimming come from here.
                             (font-lock-append-text-property
                              0 (length text) 'font-lock-face
                              'lean4-unemphasized-goal text))
                           text))
                       goals)))
        (string-join rendered "\n\n")))))

(defun lean4-render-term-goal (term-goal &optional settings)
  "Render TERM-GOAL, an `InteractiveTermGoal'.
SETTINGS is as described above."
  (when term-goal
    (lean4-render-goal term-goal settings)))

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
