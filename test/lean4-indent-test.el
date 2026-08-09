;;; lean4-indent-test.el --- Tests for Lean indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Indentation is a guess, so these pin the guesses that matter and the
;; cycling that exists for when a guess is wrong.  No server involved.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defmacro lean4-indent-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT.
Point starts at the end.  A `|' at the start of a line in TEXT is not
special; write real indentation."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-auto-start-server nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     ,@body))

(defun lean4-indent-test--column-of-last-line (text)
  "Return the column `lean4-indent-calculate' wants for a new line after TEXT."
  (lean4-indent-test--with-lean text
    (insert "\n")
    (lean4-indent-calculate)))

(ert-deftest lean4-indent-after-by-opens-a-block ()
  "A tactic block opened with `by' indents."
  (should (= (lean4-indent-test--column-of-last-line
              "theorem t : True := by")
             lean4-indent-offset)))

(ert-deftest lean4-indent-after-do-opens-a-block ()
  "A `do' block indents."
  (should (= (lean4-indent-test--column-of-last-line
              "def main : IO Unit := do")
             lean4-indent-offset)))

(ert-deftest lean4-indent-after-arrow-opens-a-block ()
  "A line ending in `=>' indents, as after a `fun' or a match arm."
  (should (= (lean4-indent-test--column-of-last-line
              "  | some x =>")
             (+ 2 lean4-indent-offset))))

(ert-deftest lean4-indent-after-where-opens-a-block ()
  "A `where' clause indents."
  (should (= (lean4-indent-test--column-of-last-line "def f : Nat where")
             lean4-indent-offset)))

(ert-deftest lean4-indent-after-bullet-opens-a-block ()
  "A focus bullet indents what follows it.
This is the one indentation rule VS Code's Lean support defines."
  (should (= (lean4-indent-test--column-of-last-line
              "theorem t : True := by\n  constructor\n  · trivial")
             (+ 2 lean4-indent-offset))))

(ert-deftest lean4-indent-continues-the-previous-line ()
  "An ordinary line keeps the indentation of the one above."
  (should (= (lean4-indent-test--column-of-last-line
              "theorem t : True := by\n  constructor\n  trivial")
             2)))

(ert-deftest lean4-indent-trailing-comment-does-not-open-a-block ()
  "A comment after a block opener is ignored; the block still opens."
  (should (= (lean4-indent-test--column-of-last-line
              "theorem t : True := by -- go")
             lean4-indent-offset))
  ;; And a comment alone does not open one.
  (should (= (lean4-indent-test--column-of-last-line
              "  trivial -- done")
             2)))

(ert-deftest lean4-indent-aligns-inside-brackets ()
  "A continuation inside an unclosed bracket lines up after the opener."
  (should (= (lean4-indent-test--column-of-last-line "def xs := [1, 2,")
             (1+ (string-search "[" "def xs := [1, 2,")))))

(ert-deftest lean4-indent-indents-under-a-hanging-bracket ()
  "A bracket that ends its line indents its contents one level."
  (should (= (lean4-indent-test--column-of-last-line "def xs := [")
             lean4-indent-offset)))

(ert-deftest lean4-indent-dedents-a-closing-line ()
  "A line that closes a block sits one level out."
  (lean4-indent-test--with-lean
      "namespace N\n  def f := 1\nend"
    (goto-char (point-max))
    (back-to-indentation)
    (should (= (lean4-indent-calculate) 0))))

(ert-deftest lean4-indent-skips-blank-and-comment-lines ()
  "Blank lines and whole-line comments do not determine indentation."
  (should (= (lean4-indent-test--column-of-last-line
              "theorem t : True := by\n  constructor\n\n  -- a note")
             2)))

(ert-deftest lean4-indent-first-line-is-column-zero ()
  "With nothing above it, a line indents to the left margin."
  (should (= (lean4-indent-test--column-of-last-line "") 0)))

;;;; Whole-buffer behaviour
;;
;; These are the cases unit-testing one line at a time missed: indenting a
;; region compounds each guess on the last, so a rule that is slightly wrong
;; in isolation is badly wrong over a file.

(defun lean4-indent-test--reindent (text)
  "Return TEXT after `indent-region' in a `lean4-mode' buffer."
  (lean4-indent-test--with-lean text
    (indent-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest lean4-indent-region-does-not-run-away ()
  "A declaration after a nested proof returns to the left margin.

Regression test.  Without the top-level rule each declaration inherited
the depth of the proof above it, so indentation marched rightwards down
the file and `indent-region' on a real file was destructive."
  (should (equal (lean4-indent-test--reindent
                  "theorem t : True := by\nconstructor\n· trivial\n\ndef f : Nat := 1\n")
                 "theorem t : True := by\n  constructor\n  · trivial\n\ndef f : Nat := 1\n")))

(ert-deftest lean4-indent-region-formats-a-match ()
  "Match alternatives line up with each other and with `with'.
Lean writes the arms flush with the `induction', not indented from it."
  (should (equal (lean4-indent-test--reindent
                  "theorem t : True := by\ninduction n with\n| zero =>\nsimp\n| succ n ih =>\nomega\n")
                 (concat "theorem t : True := by\n"
                         "  induction n with\n"
                         "  | zero =>\n"
                         "    simp\n"
                         "  | succ n ih =>\n"
                         "    omega\n"))))

(ert-deftest lean4-indent-region-closes-brackets-at-the-opener ()
  "A closing bracket lines up with the line that opened it."
  (should (equal (lean4-indent-test--reindent
                  "def xs : List Nat := [\n1,\n2\n]\n")
                 "def xs : List Nat := [\n  1,\n  2\n]\n")))

(ert-deftest lean4-indent-namespace-does-not-indent-its-contents ()
  "A namespace does not indent what it contains.
That is Lean's convention and Mathlib's; indenting fights the reader."
  (should (equal (lean4-indent-test--reindent
                  "namespace N\n\ndef f : Nat := 1\n\nend N\n")
                 "namespace N\n\ndef f : Nat := 1\n\nend N\n")))

(ert-deftest lean4-indent-nested-namespace-end-matches-its-opener ()
  "`end' lines up with the `namespace' it closes, however nested."
  (should (equal (lean4-indent-test--reindent
                  "namespace A\nnamespace B\ndef f : Nat := 1\nend B\nend A\n")
                 "namespace A\nnamespace B\ndef f : Nat := 1\nend B\nend A\n")))

(ert-deftest lean4-indent-region-is-idempotent ()
  "Indenting already-indented code changes nothing."
  (let ((text (concat "theorem t : True := by\n"
                      "  induction n with\n"
                      "  | zero =>\n"
                      "    simp\n"
                      "  | succ n ih =>\n"
                      "    omega\n"
                      "\n"
                      "def xs : List Nat := [\n"
                      "  1,\n"
                      "  2\n"
                      "]\n")))
    (should (equal (lean4-indent-test--reindent text) text))))

;;;; Cycling

(ert-deftest lean4-indent-candidates-include-guess-and-zero ()
  "The cycle offers the guess, the columns above, and the left margin."
  (lean4-indent-test--with-lean
      "theorem t : True := by\n  constructor\n    foo\n"
    (goto-char (point-max))
    (let ((candidates (lean4-indent--candidates)))
      (should (memq 0 candidates))
      (should (memq 2 candidates))
      (should (memq 4 candidates))
      (should (equal candidates (sort (copy-sequence candidates) #'<))))))

(ert-deftest lean4-indent-line-cycles-on-repeat ()
  "Pressing TAB again moves to the next plausible column.
A wrong guess should cost one keystroke, not a manual fix."
  (lean4-indent-test--with-lean
      "theorem t : True := by\n  constructor\n    foo\n"
    (goto-char (point-max))
    (let ((last-command nil))
      (lean4-indent-line))
    (let ((first (current-indentation)))
      (let ((last-command 'lean4-indent-line))
        (lean4-indent-line))
      (should-not (= first (current-indentation))))))

(ert-deftest lean4-indent-cycle-wraps-around ()
  "Cycling past the last candidate returns to the first."
  (lean4-indent-test--with-lean
      "theorem t : True := by\n  constructor\n"
    (goto-char (point-max))
    (let ((candidates (lean4-indent--candidates))
          (seen '()))
      (let ((last-command nil)) (lean4-indent-line))
      (push (current-indentation) seen)
      (dotimes (_ (1+ (length candidates)))
        (let ((last-command 'lean4-indent-line)) (lean4-indent-line))
        (push (current-indentation) seen))
      ;; Every candidate is reachable, and it comes back around.
      (should (seq-set-equal-p (seq-uniq seen) candidates)))))

(provide 'lean4-indent-test)
;;; lean4-indent-test.el ends here
