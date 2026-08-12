;;; lean4-outline-test.el --- Tests for folding Lean by its structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; What outline is told about a Lean file, and what folds as a result.
;;
;; The levels asserted here are the nesting Lean's own
;; `textDocument/foldingRange' reports for the same text -- a namespace
;; holding a section holding declarations, each region ending at the `end'
;; that closes it.  lean4-e2e-test.el checks that against the server; these
;; check it without one.

;;; Code:

(require 'ert)
(require 'outline)
(require 'lean4-mode)

(defconst lean4-outline-test--nested "\
/-
A block comment
over three lines.
-/
namespace Outer

section Inner

variable (n : Nat)

theorem multi : True := by
  constructor

def withWhere : Nat := helper 0
  where helper k := k

structure Point where
  x : Nat
  y : Nat

end Inner

end Outer
"
  "The text whose folding ranges were captured from Lean 4.32.2.
The server answers with five regions: `namespace Outer' to `end Outer',
`section Inner' to `end Inner', and the three declarations that span more
than one line.")

(defmacro lean4-outline-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-auto-start-server nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defun lean4-outline-test--goto (line)
  "Move to the line of the fixture beginning with LINE."
  (goto-char (point-min))
  (should (re-search-forward (concat "^" (regexp-quote line)) nil t))
  (forward-line 0))

(defun lean4-outline-test--level (line)
  "Return the outline level of the fixture line beginning with LINE."
  (lean4-outline-test--goto line)
  (funcall outline-level))

(defun lean4-outline-test--invisible-p (line)
  "Return non-nil if the fixture line beginning with LINE is folded away."
  (save-excursion
    (lean4-outline-test--goto line)
    ;; The line's own first character, not the newline before it: that
    ;; newline ends whatever region precedes the line and is hidden with it,
    ;; so it answers about the wrong line.
    (back-to-indentation)
    (outline-invisible-p (point))))

;;;; Levels, which are what decides where a fold ends

(ert-deftest lean4-outline-level-of-a-namespace ()
  "A namespace is the outermost thing there is."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (should (= (lean4-outline-test--level "namespace Outer") 1))))

(ert-deftest lean4-outline-level-of-a-section-inside-it ()
  "A section inside one is a level deeper."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (should (= (lean4-outline-test--level "section Inner") 2))))

(ert-deftest lean4-outline-level-of-a-declaration-inside-both ()
  "And a declaration inside both is deeper again."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (should (= (lean4-outline-test--level "theorem multi") 3))
    (should (= (lean4-outline-test--level "def withWhere") 3))
    (should (= (lean4-outline-test--level "structure Point") 3))))

(ert-deftest lean4-outline-level-of-an-end ()
  "An `end' takes the level of what it closes, which is what stops a fold
there rather than at the next block."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (should (= (lean4-outline-test--level "end Inner") 2))
    (should (= (lean4-outline-test--level "end Outer") 1))))

(ert-deftest lean4-outline-level-of-a-declaration-at-the-top ()
  "A declaration in no namespace at all is itself outermost."
  (lean4-outline-test--with-lean "theorem t : True := trivial\n"
    (should (= (lean4-outline-test--level "theorem t") 1))))

(ert-deftest lean4-outline-level-after-a-closed-block ()
  "A closed block encloses nothing: what follows it is back at the top."
  (lean4-outline-test--with-lean
      "namespace Foo\ntheorem a : True := trivial\nend Foo\ntheorem b : True := trivial\n"
    (should (= (lean4-outline-test--level "theorem a") 2))
    (should (= (lean4-outline-test--level "theorem b") 1))))

(ert-deftest lean4-outline-level-counts-mutual ()
  "`mutual' is closed by an `end' like the others, so it counts like them."
  (lean4-outline-test--with-lean
      "mutual\ndef a : Nat := 0\nend\ntheorem b : True := trivial\n"
    (should (= (lean4-outline-test--level "mutual") 1))
    (should (= (lean4-outline-test--level "def a") 2))
    (should (= (lean4-outline-test--level "end") 1))
    (should (= (lean4-outline-test--level "theorem b") 1))))

;;;; What counts as a heading

(ert-deftest lean4-outline-comments-are-not-headings ()
  "A declaration inside a block comment is text."
  (lean4-outline-test--with-lean
      "/-\ntheorem fake : True := trivial\n-/\ntheorem real : True := trivial\n"
    (lean4-outline-test--goto "theorem fake")
    (should-not (lean4-outline-search nil nil nil t))
    (lean4-outline-test--goto "theorem real")
    (should (lean4-outline-search nil nil nil t))))

(ert-deftest lean4-outline-the-comment-above-a-declaration-is-not-a-heading ()
  "Nor is a docstring, which belongs to the declaration under it."
  (lean4-outline-test--with-lean "/-- Doc. -/\ntheorem t : True := trivial\n"
    (lean4-outline-test--goto "/-- Doc. -/")
    (should-not (lean4-outline-search nil nil nil t))))

;;;; Searching

(ert-deftest lean4-outline-search-forward-finds-the-next ()
  "Forward from within a heading's line reaches the one after it."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (lean4-outline-test--goto "namespace Outer")
    (end-of-line)
    (should (lean4-outline-search nil t))
    (should (equal (buffer-substring-no-properties
                    (point) (line-end-position))
                   "section Inner"))))

(ert-deftest lean4-outline-search-forward-from-a-line-start-matches-it ()
  "From the very beginning of a heading's line, that heading is the answer.
`re-search-forward' matches at point, and `outline-next-preface' leans on
it: it steps over the newline before a heading and asks for a heading
there, hiding everything up to what comes back.  Answering with the
heading after this one has it hide this one."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (lean4-outline-test--goto "namespace Outer")
    (should (lean4-outline-search nil t))
    (should (equal (buffer-substring-no-properties
                    (point) (line-end-position))
                   "namespace Outer"))))

(ert-deftest lean4-outline-search-backward-finds-the-previous ()
  "And backward reaches the one before it."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (lean4-outline-test--goto "def withWhere")
    (should (lean4-outline-search nil t t))
    (should (equal (buffer-substring-no-properties
                    (point) (line-end-position))
                   "theorem multi : True := by"))))

(ert-deftest lean4-outline-search-answers-nil-at-the-end ()
  "Past the last heading it answers nil rather than signalling, outline
calling it in a loop which a signal would end."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (goto-char (point-max))
    (should-not (lean4-outline-search nil t))))

(ert-deftest lean4-outline-search-sets-the-match-data ()
  "Outline highlights what the match covers, which is the heading's line."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (goto-char (point-min))
    (should (lean4-outline-search nil t))
    (should (equal (match-string 0) "namespace Outer"))))

(ert-deftest lean4-outline-search-honours-a-bound ()
  "A bound stops the search, as it stops the searches this stands in for."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (lean4-outline-test--goto "namespace Outer")
    (end-of-line)
    (let ((before-the-section (save-excursion
                                (lean4-outline-test--goto "section Inner")
                                (1- (point)))))
      (should-not (lean4-outline-search before-the-section t)))))

;;;; Folding, which is the point of all of it

(ert-deftest lean4-outline-folding-a-section-stops-at-its-end ()
  "The reason `end' is a heading: without it this fold would swallow
`end Inner', `end Outer' and anything after them."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (outline-minor-mode 1)
    (lean4-outline-test--goto "section Inner")
    (outline-hide-subtree)
    (should (lean4-outline-test--invisible-p "theorem multi"))
    (should-not (lean4-outline-test--invisible-p "end Inner"))
    (should-not (lean4-outline-test--invisible-p "end Outer"))))

(ert-deftest lean4-outline-folding-a-declaration-hides-its-body ()
  "A declaration folds to its own first line."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (outline-minor-mode 1)
    (lean4-outline-test--goto "theorem multi")
    (outline-hide-subtree)
    (should (lean4-outline-test--invisible-p "  constructor"))
    (should-not (lean4-outline-test--invisible-p "def withWhere"))))

(ert-deftest lean4-outline-hiding-the-body-leaves-the-headings ()
  "`outline-hide-body' leaves every heading and hides the rest, which is
VS Code's \"Fold All\"."
  (lean4-outline-test--with-lean lean4-outline-test--nested
    (outline-minor-mode 1)
    (outline-hide-body)
    (should-not (lean4-outline-test--invisible-p "namespace Outer"))
    (should-not (lean4-outline-test--invisible-p "theorem multi"))
    (should (lean4-outline-test--invisible-p "  constructor"))
    (should (lean4-outline-test--invisible-p "  x : Nat"))))

;;;; A command with a prefix folds as one

(defconst lean4-outline-test--prefixed "\
set_option maxHeartbeats 400000 in
theorem a : True := by
  trivial

open Nat in
theorem b : True := by
  trivial

theorem c : True := by
  trivial
"
  "Text whose folding ranges were captured from Lean 4.32.2.
It answers with three regions, each beginning at the line the command
begins on -- the `set_option', the `open', and `theorem c' -- rather than
at the declaration a prefix wraps.")

(ert-deftest lean4-outline-a-prefix-is-the-heading ()
  "The command begins at the prefix, so that is where the fold begins."
  (lean4-outline-test--with-lean lean4-outline-test--prefixed
    (lean4-outline-test--goto "set_option maxHeartbeats 400000 in")
    (should (lean4-outline-search nil nil nil t))
    (lean4-outline-test--goto "open Nat in")
    (should (lean4-outline-search nil nil nil t))))

(ert-deftest lean4-outline-a-prefixed-declaration-is-not-another-heading ()
  "Two headings would fold the prefix line alone, which hides nothing."
  (lean4-outline-test--with-lean lean4-outline-test--prefixed
    (lean4-outline-test--goto "theorem a : True := by")
    (should-not (lean4-outline-search nil nil nil t))
    ;; The one with no prefix above it is a heading as ever.
    (lean4-outline-test--goto "theorem c : True := by")
    (should (lean4-outline-search nil nil nil t))))

(ert-deftest lean4-outline-folding-a-prefixed-command-hides-all-of-it ()
  "Including the declaration the prefix wraps."
  (lean4-outline-test--with-lean lean4-outline-test--prefixed
    (outline-minor-mode 1)
    (lean4-outline-test--goto "set_option maxHeartbeats 400000 in")
    (outline-hide-subtree)
    (should (lean4-outline-test--invisible-p "theorem a : True := by"))
    (should (lean4-outline-test--invisible-p "  trivial"))
    (should-not (lean4-outline-test--invisible-p "open Nat in"))))

(provide 'lean4-outline-test)
;;; lean4-outline-test.el ends here
