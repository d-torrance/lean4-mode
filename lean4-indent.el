;;; lean4-indent.el --- Indentation for Lean 4 -*- lexical-binding: t -*-

;; Copyright (C) 2026 Lean4-Mode contributors

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

;; Indentation for Lean 4, as a guess that can be overridden.
;;
;; Three approaches were considered and two rejected.
;;
;; The language server cannot help: Lean advertises no formatting provider of
;; any kind -- not document, range or on-type -- because Lean has no official
;; formatter.  There is nothing to ask for.
;;
;; SMIE does not fit.  It wants a BNF over a closed token set with assigned
;; precedences, and Lean's syntax is extensible by construction: `syntax',
;; `notation' and `macro_rules' let a library add operators at parse time.
;; Indentation is decided by layout -- `by', `do', `match', `fun', `·' -- and
;; not by delimiters.  haskell-mode abandoned grammar-based indentation for
;; the same reasons.
;;
;; So this follows `python-mode', the closest well-tested analogue in Emacs:
;; another layout-sensitive language with no closing delimiters, where
;; indentation is a heuristic best guess and repeated TAB cycles through the
;; other plausible columns.  For calibration, VS Code's own Lean support has
;; no indentation rules whatsoever beyond bracket matching and a single rule
;; for `·' bullets, so a guess with an escape hatch is already ahead.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defcustom lean4-indent-offset 2
  "Number of columns to indent a nested block in Lean."
  :group 'lean4
  :type 'integer)

(defconst lean4-indent--block-openers
  '("by" "do" "then" "else" "where" "with" "from" "=>" ":=" "<|>")
  "Tokens which, ending a line, open an indented block on the next.")

;; These are matched with `looking-at-p' at a line's indentation, never at
;; its beginning, so none of them may be anchored with `bol': that assertion
;; only holds in column zero, which would silently disable every rule below
;; on any indented line.

(defconst lean4-indent--bullet-regexp
  (rx (or "·" ".") (or space eol))
  "A focus bullet, which indents what follows it.
This is the one indentation rule VS Code's Lean support defines.")

(defconst lean4-indent--bar-regexp
  (rx "|" (or space eol))
  "A match alternative, which lines up with its siblings.")

(defconst lean4-indent--toplevel-regexp
  (rx (? (or "private" "protected" "partial" "unsafe" "noncomputable"
             "@[" "scoped" "local")
         (* nonl) (* space))
      (or "theorem" "lemma" "def" "abbrev" "instance" "structure" "class"
          "inductive" "example" "namespace" "section" "open" "import"
          "variable" "universe" "set_option" "attribute" "macro"
          "macro_rules" "syntax" "notation" "mutual" "deriving" "#check"
          "#eval" "#print")
      symbol-end)
  "A line beginning a new top-level declaration.

Recognising these is what stops indentation running away: without it a
declaration following a deeply nested proof inherits that depth, and
every declaration after it drifts further right.")

(defconst lean4-indent--block-end-regexp
  (rx "end" symbol-end)
  "A line closing a `namespace' or `section'.")

(defconst lean4-indent--block-begin-regexp
  (rx (or "namespace" "section") symbol-end)
  "A line opening a `namespace' or `section'.")

(defun lean4-indent--line-text (&optional position)
  "Return the text of the line containing POSITION, without indentation.
Comments and strings are not stripped; callers that care check syntax."
  (save-excursion
    (when position (goto-char position))
    (buffer-substring-no-properties
     (progn (back-to-indentation) (point))
     (line-end-position))))

(defun lean4-indent--in-string-or-comment-p (&optional position)
  "Return non-nil if POSITION, or point, is inside a string or comment."
  (let ((state (syntax-ppss position)))
    (or (nth 3 state) (nth 4 state))))

(defun lean4-indent--previous-code-line ()
  "Move to the previous line that is not blank or wholly a comment.
Returns non-nil on success, leaving point at its indentation."
  (let ((found nil))
    (while (and (not found) (zerop (forward-line -1)))
      (back-to-indentation)
      (unless (or (eolp) (lean4-indent--in-string-or-comment-p))
        (setq found t)))
    found))

(defun lean4-indent--opens-block-p (text)
  "Return non-nil if the line TEXT ends with something opening a block."
  (let ((trimmed (string-trim-right
                  ;; A trailing line comment does not affect layout.
                  (replace-regexp-in-string "--.*\\'" "" text))))
    (seq-some (lambda (opener)
                (string-match-p (concat (regexp-quote opener) "\\'") trimmed))
              lean4-indent--block-openers)))

(defun lean4-indent--enclosing-bracket ()
  "Return the position of the innermost unclosed bracket, or nil."
  (let ((state (syntax-ppss)))
    (unless (or (nth 3 state) (nth 4 state))
      (car (last (nth 9 state))))))

(defun lean4-indent--matching-block-column ()
  "Return the indentation of the `namespace' or `section' this `end' closes.
Scans backwards keeping a depth count, so nested namespaces line up with
their own opener rather than at the left margin."
  (save-excursion
    (let ((depth 1) (column 0))
      (while (and (> depth 0) (lean4-indent--previous-code-line))
        (cond ((looking-at-p lean4-indent--block-end-regexp) (cl-incf depth))
              ((looking-at-p lean4-indent--block-begin-regexp)
               (cl-decf depth)
               (when (zerop depth) (setq column (current-indentation))))))
      column)))

(defun lean4-indent--bar-column ()
  "Return the column a match alternative should line up in.
Siblings first: a run of `|' lines is a list, and lists line up.  Failing
that, the line that introduced the match, since Lean's convention is to
write the alternatives flush with it rather than indented from it."
  (save-excursion
    (let ((column nil))
      (while (and (null column) (lean4-indent--previous-code-line))
        (cond
         ((looking-at-p lean4-indent--bar-regexp)
          (setq column (current-indentation)))
         ((lean4-indent--opens-block-p (lean4-indent--line-text))
          (setq column (current-indentation)))
         ((looking-at-p lean4-indent--toplevel-regexp)
          (setq column (+ (current-indentation) lean4-indent-offset)))))
      (or column 0))))

(defun lean4-indent-calculate ()
  "Return the column this line should probably be indented to."
  (save-excursion
    (back-to-indentation)
    (let ((bracket (lean4-indent--enclosing-bracket)))
      (cond
       ;; A new declaration starts over, whatever depth the proof above it
       ;; reached.  Without this, indentation runs away down the file.
       ((and (null bracket) (looking-at-p lean4-indent--toplevel-regexp)
             (not (looking-at-p lean4-indent--block-end-regexp)))
        ;; Level with the enclosing `namespace' or `section', not indented
        ;; from it: Lean's convention, and Mathlib's, is that a namespace
        ;; does not indent its contents.
        (save-excursion
          (let ((depth 0) (column 0))
            (while (and (zerop depth) (lean4-indent--previous-code-line))
              (cond ((looking-at-p lean4-indent--block-end-regexp)
                     (cl-decf depth))
                    ((looking-at-p lean4-indent--block-begin-regexp)
                     (setq depth 1 column (current-indentation)))))
            column)))
       ;; `end' lines up with the `namespace' or `section' it closes.
       ((and (null bracket) (looking-at-p lean4-indent--block-end-regexp))
        (lean4-indent--matching-block-column))
       ;; A closing bracket lines up with the line that opened it, not
       ;; with the contents it is closing.
       ((and bracket (looking-at-p (rx (or ")" "]" "}" "⟩" "⟫" "⦄"))))
        (save-excursion (goto-char bracket) (current-indentation)))
       ;; Otherwise, inside brackets: align after the opener, or one level
       ;; in from it when the opener ends its line.
       (bracket
        (save-excursion
          (goto-char bracket)
          (forward-char)
          (if (looking-at-p (rx (* space) eol))
              (+ (progn (goto-char bracket) (current-indentation))
                 lean4-indent-offset)
            (current-column))))
       ;; Match alternatives line up with each other.
       ((looking-at-p lean4-indent--bar-regexp) (lean4-indent--bar-column))
       (t
        (let ((previous
               (save-excursion
                 (when (lean4-indent--previous-code-line)
                   (list (current-indentation)
                         (lean4-indent--line-text)
                         (looking-at-p lean4-indent--bar-regexp))))))
          (pcase previous
            ('nil 0)
            (`(,column ,text ,bar)
             (cond
              ;; A `|' alternative opens the arm's body.
              (bar (+ column lean4-indent-offset))
              ((lean4-indent--opens-block-p text) (+ column lean4-indent-offset))
              ((string-match-p (concat "\\`" lean4-indent--bullet-regexp) text)
               (+ column lean4-indent-offset))
              (t column))))))))))

(defun lean4-indent--candidates ()
  "Return the plausible indentation columns for this line, ascending.

The computed guess, every column some earlier line is indented to, and
zero.  Repeated TAB steps through these, which is what makes a wrong
guess cost one keystroke rather than a manual fix."
  (let ((candidates (list 0 (lean4-indent-calculate))))
    (save-excursion
      (back-to-indentation)
      (let ((limit 0))
        (while (and (< limit 40) (lean4-indent--previous-code-line))
          (setq limit (1+ limit))
          (push (current-indentation) candidates)
          (when-let* ((text (lean4-indent--line-text)))
            (when (lean4-indent--opens-block-p text)
              (push (+ (current-indentation) lean4-indent-offset)
                    candidates))))))
    (seq-sort #'< (seq-uniq candidates))))

(defun lean4-indent--to (column)
  "Indent the current line to COLUMN, keeping point sensibly placed."
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))

(defun lean4-indent-line-function ()
  "Indent the current line to the computed guess.

This is what `indent-line-function' is set to, so it never cycles:
`indent-region', `newline-and-indent' and every other caller of
`indent-according-to-mode' go through here, and cycling there would make
bulk indentation depend on what ran before it -- which is exactly how
`indent-region' came to be non-idempotent."
  (lean4-indent--to (lean4-indent-calculate)))

(defun lean4-indent-line ()
  "Indent the current line, cycling on repeated invocation.

The first press indents to the computed guess.  Pressing again steps to
the next plausible column, wrapping around, in the manner of
`python-indent-line'.  Lean's layout cannot always be inferred, so the
cycle is the escape hatch rather than an afterthought.

Only for interactive use; see `lean4-indent-line-function'."
  (interactive)
  (if (not (memq last-command '(indent-for-tab-command lean4-indent-line
                                lean4-tab-indent)))
      (lean4-indent--to (lean4-indent-calculate))
    (let ((current (current-indentation))
          (candidates (lean4-indent--candidates)))
      ;; Step to the next candidate above the current column, or wrap.
      (lean4-indent--to
       (or (seq-find (lambda (column) (> column current)) candidates)
           (car candidates))))))

(provide 'lean4-indent)
;;; lean4-indent.el ends here
