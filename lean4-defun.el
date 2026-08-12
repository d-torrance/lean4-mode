;;; lean4-defun.el --- Declaration navigation for Lean 4 -*- lexical-binding: t -*-

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

;; What `C-M-a', `C-M-e', `mark-defun' and `narrow-to-defun' should do in a
;; Lean buffer, through the three variables Emacs provides for saying so.
;;
;; VS Code has no counterpart to these commands: its structural navigation is
;; the outline, the breadcrumbs bar and go-to-symbol, all of which come from
;; the server's `textDocument/documentSymbol' -- and all of which Emacs
;; already has, since Eglot feeds that same response to Imenu.  What it has no
;; answer for is that `C-M-a' is bound in every Emacs buffer and, with nothing
;; set here, falls back to a recipe meant for Lisp -- an open paren in column
;; zero -- which Lean code never matches, so it runs to the top of the file.
;;
;; The server could answer this exactly, through those same document symbols.
;; It is not asked, for the reason `lean4-indent-line' guesses rather than
;; asks: motion has to work in a buffer opened a moment ago, with no server
;; running, below the last edit, and in a file that does not compile.  That is
;; precisely when one navigates by structure.  Nothing here talks to Lean.
;;
;; A declaration here is a named one -- `theorem', `def', `instance' and their
;; kind -- and not `namespace' or `section', which enclose declarations rather
;; than being them: a `C-M-a' that stopped at `namespace Foo' would leave the
;; theorem the reader was standing in.  Those are structure for an outline to
;; show, not defuns.
;;
;; Lean terminates nothing, so a declaration runs until the next one begins at
;; the same indentation or less, exactly as a Python function does.  And it
;; begins not at its keyword but at whatever is attached above it -- its
;; attributes and its doc comment -- since a `mark-defun' that took the
;; theorem and left its docstring behind would be a bug.  A blank line
;; detaches: a `/- ... -/' with a gap under it comments on the file, not on
;; what follows.  An ordinary `--' comment never attaches, having no way to
;; say whether it belongs to the line below or to the one above.
;;
;; Declarations inside a `mutual' block are each treated as a defun of their
;; own.  Lean writes them flush with the `mutual', so nothing short of
;; counting `mutual' and `end' would tell them from top-level ones, and
;; reaching one of them is worth more than reporting the block as a unit.

;;; Code:

(require 'rx)
(require 'lean4-indent)

(defconst lean4-defun--keywords
  '("theorem" "lemma" "def" "abbrev" "instance" "example" "structure" "class"
    "inductive" "opaque" "axiom" "macro" "macro_rules" "syntax" "elab"
    "elab_rules" "notation" "declare_syntax_cat" "mutual")
  "Keywords which introduce a declaration.
`namespace' and `section' are deliberately absent: they enclose
declarations rather than being ones.")

(defconst lean4-defun--modifiers
  '("private" "protected" "noncomputable" "unsafe" "partial" "nonrec"
    "scoped" "local" "public" "meta")
  "Words which may stand between an attribute list and a declaration keyword.
`public' and `meta' are the module system\\='s; the rest are older.")

(defconst lean4-defun--prefix-keywords '("set_option" "open")
  "Commands which can stand before a declaration, joined to it by `in'.
Exactly these two.  `Lean.Parser.Command.«in»' is a trailing parser over
`withOpen' and `withSetOption', so an `open' and a `set_option' are what
may wrap a command -- and Lean folds the pair as one, its
`textDocument/foldingRange' beginning the region at the `set_option'
rather than at the declaration under it.")

;; Matched with `looking-at' at a line's indentation, never at its beginning,
;; so this must not be anchored with `bol': that assertion holds only in
;; column zero and would quietly stop matching on any indented line.
;; `lean4-indent.el' carries the same warning for the same reason.

(defconst lean4-defun--declaration-regexp
  (rx-to-string
   `(seq (? (seq (or ,@lean4-defun--prefix-keywords) symbol-end
                 (+ nonl) symbol-start "in" symbol-end (+ space)))
         (? "@[" (* nonl) "]" (* space))
         (* (seq (or ,@lean4-defun--modifiers) (+ space)))
         (group (or ,@lean4-defun--keywords))
         symbol-end
         (* space)
         (? (group (or (seq "«" (* (not (any "»"))) "»")
                       (+ (not (any " \t\n:(){}[]⦃⦄«»")))))))
   t)
  "A line beginning a declaration, with its keyword and name.
Group 1 is the keyword and group 2 the name, which is absent from an
`example' and from an anonymous `instance'.  The attribute list is
matched greedily and the match backtracks, so that the `]' meant in
`@[simp] theorem foo : p [x] := h' is the first one and not the last.")

(defconst lean4-defun--prefix-regexp
  (rx-to-string
   `(seq (or ,@lean4-defun--prefix-keywords) symbol-end
         (+ nonl) symbol-start "in" symbol-end (* space) eol)
   t)
  "A line which prefixes the declaration below it and holds nothing else.
The `in' is matched at a symbol boundary, so a declaration named
`in_range' on the same line is not mistaken for the connective.")

(defconst lean4-defun--attribute-regexp
  (rx "@[")
  "A line carrying attributes for the declaration below it.")

(defun lean4-defun--at-start-p ()
  "Return non-nil if the current line begins a declaration."
  (save-excursion
    (back-to-indentation)
    (and (not (lean4-indent--in-string-or-comment-p))
         (looking-at-p lean4-defun--declaration-regexp))))

(defun lean4-defun--at-boundary-p ()
  "Return non-nil if the current line begins something new.
That is a declaration, a top-level command such as `open' or
`variable', or the `end' of a namespace: anything, in short, which the
declaration above it does not reach past."
  (save-excursion
    (back-to-indentation)
    (and (not (eolp))
         (not (lean4-indent--in-string-or-comment-p))
         (or (looking-at-p lean4-defun--declaration-regexp)
             (looking-at-p lean4-indent--toplevel-regexp)
             (looking-at-p lean4-indent--block-end-regexp)))))

(defun lean4-defun--at-attribute-p ()
  "Return non-nil if the current line holds attributes and nothing else."
  (save-excursion
    (back-to-indentation)
    (and (not (lean4-indent--in-string-or-comment-p))
         (looking-at-p lean4-defun--attribute-regexp)
         (not (looking-at-p lean4-defun--declaration-regexp)))))

(defun lean4-defun--at-prefix-p ()
  "Return non-nil if this line prefixes the declaration below it.
That is a `set_option ... in' or an `open ... in' with the declaration on
the next line; one with the declaration on the same line is a declaration
start in its own right, `lean4-defun--declaration-regexp' allowing the
prefix."
  (save-excursion
    (back-to-indentation)
    (and (not (lean4-indent--in-string-or-comment-p))
         (looking-at-p lean4-defun--prefix-regexp))))

(defun lean4-defun--blank-line-p ()
  "Return non-nil if the current line is empty or all whitespace."
  (save-excursion
    (back-to-indentation)
    (eolp)))

(defun lean4-defun--comment-line-start ()
  "Return where the block comment covering this whole line begins, or nil.
A `--' comment answers nil: only the `/- -/' kind, which is what a
docstring is, counts as attached to what follows it."
  (save-excursion
    (back-to-indentation)
    (let ((state (syntax-ppss)))
      (cond ((nth 4 state) (nth 8 state))
            ((looking-at-p "/-") (point))))))

(defun lean4-defun--preamble-start ()
  "Return where the declaration on the current line begins.
That is the start of its attributes and of the doc comment they in turn
hang from, whichever reaches furthest back.  A blank line, or anything
that is neither, stops the walk."
  (save-excursion
    (forward-line 0)
    (let ((start (point))
          (done nil))
      (while (not done)
        (if (bobp)
            (setq done t)
          (forward-line -1)
          (let ((comment (and (not (lean4-defun--blank-line-p))
                              (lean4-defun--comment-line-start))))
            (cond
             ((lean4-defun--blank-line-p) (setq done t))
             ((lean4-defun--at-attribute-p) (setq start (point)))
             ;; `set_option ... in' and `open ... in' belong to what they
             ;; wrap: Lean folds the pair as one command, and a
             ;; `mark-defun' which took the theorem and left the option
             ;; behind would leave neither half meaning what it did.
             ((lean4-defun--at-prefix-p) (setq start (point)))
             (comment (goto-char comment)
                      (forward-line 0)
                      (setq start (point)))
             (t (setq done t))))))
      start)))

(defun lean4-defun--before-blank-lines (position)
  "Return POSITION moved back over the blank lines directly above it."
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (while (and (not (bobp))
                (save-excursion (forward-line -1) (lean4-defun--blank-line-p)))
      (forward-line -1))
    (point)))

(defun lean4-defun--limit ()
  "Return where the declaration beginning on the current line ends.
That is the start of the line after its last non-blank one, so that the
blank lines between two declarations belong to neither."
  (save-excursion
    (back-to-indentation)
    (let ((column (current-indentation))
          (last (line-end-position))
          (limit nil))
      (while (and (null limit) (zerop (forward-line 1)))
        (cond ((lean4-defun--blank-line-p))
              ;; A declaration ends this one from whatever column it is
              ;; written in: Lean's commands are not layout-sensitive --
              ;; only the terms and tactics inside them are -- and none of
              ;; these keywords can appear within a term, so a misindented
              ;; `def' is still a `def' and not part of what precedes it.
              ;; The other boundaries do depend on the column, `deriving'
              ;; above all, that being a top-level command and also the
              ;; last line of an indented `structure' body.
              ((or (lean4-defun--at-start-p)
                   (and (<= (current-indentation) column)
                        (lean4-defun--at-boundary-p)))
               (setq limit (lean4-defun--before-blank-lines
                            (lean4-defun--preamble-start))))
              (t (setq last (line-end-position)))))
      (or limit
          (progn (goto-char last)
                 (if (eobp) (point) (1+ (point))))))))

(defun lean4-defun--enclosing-preamble-start ()
  "Return the start of the declaration whose preamble covers point, or nil.
Point can only be inside a preamble while it is on a comment line or an
attribute line, and asking that first is what keeps this cheap: the
declaration a doc comment documents is below it, not above.

The test is on the line rather than on point, because `end-of-defun'
calls this with point already moved to the end of one -- past the `-/'
and so, as far as the syntax is concerned, out of the comment again."
  (when (or (lean4-defun--comment-line-start)
            (lean4-defun--at-attribute-p))
    (let ((origin (point))
          (found nil))
      (save-excursion
        (while (and (null found) (zerop (forward-line 1)))
          (cond ((lean4-defun--at-start-p) (setq found (point)))
                ((or (lean4-defun--blank-line-p)
                     (lean4-defun--comment-line-start)
                     (lean4-defun--at-attribute-p)))
                (t (setq found 'none))))
        (when (integerp found)
          (let ((start (lean4-defun--preamble-start)))
            (and (<= start origin) start)))))))

(defun lean4-defun--previous-start ()
  "Move to where the declaration before point begins.
Return non-nil if there was one, leaving point alone if there was not."
  (let* ((origin (point))
         (enclosing (lean4-defun--enclosing-preamble-start))
         (target (and enclosing (< enclosing origin) enclosing)))
    (unless target
      (save-excursion
        (forward-line 0)
        (while (and (null target)
                    (progn
                      (when (lean4-defun--at-start-p)
                        (let ((start (lean4-defun--preamble-start)))
                          (when (< start origin) (setq target start))))
                      (and (null target) (not (bobp)))))
          (forward-line -1))))
    (when target (goto-char target) t)))

(defun lean4-defun--next-start ()
  "Move to where the declaration after point begins.
Return non-nil if there was one, leaving point alone if there was not."
  (let ((origin (point))
        (target nil))
    (save-excursion
      (forward-line 0)
      (while (and (null target)
                  (progn
                    (when (lean4-defun--at-start-p)
                      (let ((start (lean4-defun--preamble-start)))
                        (when (> start origin) (setq target start))))
                    (and (null target) (zerop (forward-line 1)))))))
    (when target (goto-char target) t)))

(defun lean4-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a declaration.
With ARG, do it that many times; a negative ARG moves forward instead.
Return non-nil if a declaration was found, leaving point where it
begins.

This is the `beginning-of-defun-function' of `lean4-mode'."
  (let* ((arg (or arg 1))
         (step (if (< arg 0) #'lean4-defun--next-start
                 #'lean4-defun--previous-start))
         (found t))
    (dotimes (_ (abs arg))
      (when found (setq found (funcall step))))
    found))

(defun lean4-end-of-defun ()
  "Move forward to the end of the declaration point begins.
`end-of-defun' has already put point where that declaration starts,
which for a documented one is its doc comment rather than its keyword.

This is the `end-of-defun-function' of `lean4-mode'."
  (goto-char
   (save-excursion
     ;; Measure from the keyword line: the preamble above it may be indented
     ;; differently, and it is the keyword's own column that says how far the
     ;; declaration reaches.
     (while (and (not (lean4-defun--at-start-p)) (zerop (forward-line 1))))
     (lean4-defun--limit))))

(defun lean4-defun--current-start ()
  "Return where the declaration point is inside begins, or nil.
Unlike `lean4-defun--previous-start' this counts the declaration point
stands at the very beginning of, and refuses one point has already
moved past the end of."
  (save-excursion
    (let ((origin (point)))
      (goto-char (line-end-position))
      (when (lean4-defun--previous-start)
        (let ((start (point)))
          (while (and (not (lean4-defun--at-start-p)) (zerop (forward-line 1))))
          (and (< origin (lean4-defun--limit)) start))))))

(defun lean4-current-defun-name ()
  "Return the name of the declaration point is inside, or nil.
An unnamed one -- an `example', or an anonymous `instance' -- answers
with its keyword, that being what there is to say about it.

This is the `add-log-current-defun-function' of `lean4-mode'."
  (when-let* ((start (lean4-defun--current-start)))
    (save-excursion
      (goto-char start)
      (while (and (not (lean4-defun--at-start-p)) (zerop (forward-line 1))))
      (back-to-indentation)
      (when (looking-at lean4-defun--declaration-regexp)
        (let ((keyword (match-string-no-properties 1))
              (name (match-string-no-properties 2)))
          ;; `class inductive Foo' reads its second keyword as a name.
          (when (and name (member name lean4-defun--keywords))
            (goto-char (match-beginning 2))
            (when (looking-at lean4-defun--declaration-regexp)
              (setq keyword (match-string-no-properties 1)
                    name (match-string-no-properties 2))))
          (or name keyword))))))

(provide 'lean4-defun)
;;; lean4-defun.el ends here
