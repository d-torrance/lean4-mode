;;; lean4-defun-test.el --- Tests for Lean declaration navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Where `C-M-a' and `C-M-e' land, and what `which-function-mode' is told.
;; Layout decides all of it, so these pin the cases layout gets wrong if the
;; rules are stated carelessly: a declaration reached past its docstring, one
;; ended early by an indented `where', one ended late by a `namespace'.
;; No server involved.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defmacro lean4-defun-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT.
Point starts at the beginning.  A `|' in TEXT marks where point should
be put instead, and is removed."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-auto-start-server nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     (goto-char (point-min))
     (if (search-forward "|" nil t)
         (delete-char -1)
       (goto-char (point-min)))
     ,@body))

(defun lean4-defun-test--line ()
  "Return the text of the line point is on."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun lean4-defun-test--before-point ()
  "Return the text from the beginning of the buffer up to point.
What `end-of-defun' leaves behind it is the declaration it ended, so
this says which one that was; the line it leaves point on is the blank
one below, as in any other mode."
  (buffer-substring-no-properties (point-min) (point)))

;;;; Beginning of defun

(ert-deftest lean4-defun-beginning-from-inside-a-proof ()
  "`C-M-a' from within a proof reaches the declaration it belongs to."
  (lean4-defun-test--with-lean
      "theorem a : True := by\n  trivial\n\ntheorem b : True := by\n  tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "theorem b : True := by"))))

(ert-deftest lean4-defun-beginning-repeats ()
  "A prefix argument walks back that many declarations."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\ntheorem b : True := trivial\ntheorem c : True := trivial|\n"
    (beginning-of-defun 3)
    (should (equal (lean4-defun-test--line) "theorem a : True := trivial"))))

(ert-deftest lean4-defun-beginning-at-a-start-moves-to-the-previous ()
  "From the very start of a declaration, `C-M-a' goes to the one before."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n|theorem b : True := trivial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "theorem a : True := trivial"))))

(ert-deftest lean4-defun-beginning-includes-the-docstring ()
  "A declaration begins at its doc comment, not at its keyword."
  (lean4-defun-test--with-lean
      "/-- Doc. -/\ntheorem a : True := tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "/-- Doc. -/"))))

(ert-deftest lean4-defun-beginning-includes-a-multiline-docstring ()
  "A doc comment running over several lines is taken whole."
  (lean4-defun-test--with-lean
      "/-- One.\n\nTwo. -/\ntheorem a : True := tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "/-- One."))))

(ert-deftest lean4-defun-beginning-includes-attributes ()
  "Attributes on their own line belong to the declaration below them."
  (lean4-defun-test--with-lean
      "@[simp]\ntheorem a : True := tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "@[simp]"))))

(ert-deftest lean4-defun-beginning-includes-docstring-above-attributes ()
  "Doc comment, then attributes, then keyword: all one declaration."
  (lean4-defun-test--with-lean
      "/-- Doc. -/\n@[simp]\ntheorem a : True := tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "/-- Doc. -/"))))

(ert-deftest lean4-defun-beginning-stops-at-a-blank-line ()
  "A comment with a gap under it belongs to the file, not to what follows."
  (lean4-defun-test--with-lean
      "/- A note about the file. -/\n\ntheorem a : True := tri|vial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "theorem a : True := trivial"))))

(ert-deftest lean4-defun-beginning-from-inside-the-docstring ()
  "Point inside a doc comment belongs to the declaration under it."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n\n/-- D|oc. -/\ntheorem b : True := trivial\n"
    (beginning-of-defun)
    (should (equal (lean4-defun-test--line) "/-- Doc. -/"))))

(ert-deftest lean4-defun-beginning-ignores-a-commented-declaration ()
  "A `theorem' inside a block comment is text, not a declaration.
Two hops: the first reaches the start of `b', the second must pass the
whole comment rather than stopping at the declaration inside it."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n\n/-\ntheorem fake : True := trivial\n-/\n\ntheorem b : True := tri|vial\n"
    (beginning-of-defun 2)
    (should (equal (lean4-defun-test--line) "theorem a : True := trivial"))))

(ert-deftest lean4-defun-beginning-skips-namespace ()
  "`namespace' encloses declarations rather than being one."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n\nnamespace Foo\n\ntheorem b : True := by\n  tri|vial\n"
    (beginning-of-defun 2)
    (should (equal (lean4-defun-test--line) "theorem a : True := trivial"))))

(ert-deftest lean4-defun-beginning-finds-modified-declarations ()
  "Modifiers before the keyword do not hide the declaration."
  (lean4-defun-test--with-lean
      "private noncomputable def f : Nat := 0\ntheorem a : True := tri|vial\n"
    (beginning-of-defun 2)
    (should (equal (lean4-defun-test--line)
                   "private noncomputable def f : Nat := 0"))))

(ert-deftest lean4-defun-beginning-returns-nil-when-there-is-none ()
  "Above the first declaration there is nothing to find, and point stays."
  (lean4-defun-test--with-lean "import Foo\n|\ntheorem a : True := trivial\n"
    (let ((origin (point)))
      (should-not (beginning-of-defun))
      (should (= (point) origin)))))

(ert-deftest lean4-defun-beginning-backward-with-negative-argument ()
  "A negative argument moves forward instead."
  (lean4-defun-test--with-lean
      "theorem a| : True := trivial\ntheorem b : True := trivial\n"
    (beginning-of-defun -1)
    (should (equal (lean4-defun-test--line) "theorem b : True := trivial"))))

;;;; End of defun

(ert-deftest lean4-defun-end-stops-before-the-next-declaration ()
  "A declaration ends where the next one begins."
  (lean4-defun-test--with-lean
      "theorem a : True := by\n  tri|vial\n\ntheorem b : True := trivial\n"
    (end-of-defun)
    (should (equal (lean4-defun-test--before-point)
                   "theorem a : True := by\n  trivial\n"))))

(ert-deftest lean4-defun-end-keeps-the-next-docstring-out ()
  "The next declaration's doc comment belongs to it, not to this one."
  (lean4-defun-test--with-lean
      "theorem a| : True := trivial\n\n/-- Doc. -/\ntheorem b : True := trivial\n"
    (end-of-defun)
    (should (equal (lean4-defun-test--before-point)
                   "theorem a : True := trivial\n"))))

(ert-deftest lean4-defun-end-reaches-past-indented-lines ()
  "Everything indented under the declaration is part of it."
  (lean4-defun-test--with-lean
      "def f| : Nat := g 0\n  where\n    g n := n\n\ntheorem a : True := trivial\n"
    (end-of-defun)
    (should (equal (lean4-defun-test--before-point)
                   "def f : Nat := g 0\n  where\n    g n := n\n"))))

(ert-deftest lean4-defun-end-stops-at-a-command ()
  "A top-level command ends the declaration above it."
  (lean4-defun-test--with-lean
      "theorem a| : True := trivial\n\n#check Nat\n"
    (end-of-defun)
    (should (equal (lean4-defun-test--before-point)
                   "theorem a : True := trivial\n"))))

(ert-deftest lean4-defun-end-stops-at-end-of-namespace ()
  "The `end' closing a namespace is not part of the declaration above it."
  (lean4-defun-test--with-lean
      "namespace Foo\n\ntheorem a| : True := trivial\n\nend Foo\n"
    (end-of-defun)
    (should (equal (lean4-defun-test--before-point)
                   "namespace Foo\n\ntheorem a : True := trivial\n"))))

(ert-deftest lean4-defun-end-of-the-last-declaration ()
  "The last declaration in a file ends at the end of the file."
  (lean4-defun-test--with-lean "theorem a| : True := trivial\n"
    (end-of-defun)
    (should (= (point) (point-max)))))

;;;; Narrowing, which is the two of them together

(ert-deftest lean4-defun-narrow-takes-the-whole-declaration ()
  "`narrow-to-defun' keeps the docstring and drops the neighbours."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n\n/-- Doc. -/\ntheorem b : True := by\n  tri|vial\n\ntheorem c : True := trivial\n"
    (narrow-to-defun)
    (should (equal (buffer-string)
                   "/-- Doc. -/\ntheorem b : True := by\n  trivial\n"))))

;;;; Names, for `which-function-mode' and `add-log'

(ert-deftest lean4-defun-name-of-a-theorem ()
  "The name is the token after the keyword."
  (lean4-defun-test--with-lean "theorem foo_bar : True := tri|vial\n"
    (should (equal (lean4-current-defun-name) "foo_bar"))))

(ert-deftest lean4-defun-name-is-dotted-in-full ()
  "A dotted name is one name."
  (lean4-defun-test--with-lean "theorem Nat.foo : True := tri|vial\n"
    (should (equal (lean4-current-defun-name) "Nat.foo"))))

(ert-deftest lean4-defun-name-past-modifiers-and-attributes ()
  "Attributes and modifiers are stepped over."
  (lean4-defun-test--with-lean
      "@[simp] private theorem foo : True := tri|vial\n"
    (should (equal (lean4-current-defun-name) "foo"))))

(ert-deftest lean4-defun-name-of-an-escaped-identifier ()
  "A name in guillemets keeps them, spaces and all."
  (lean4-defun-test--with-lean "theorem «my thm» : True := tri|vial\n"
    (should (equal (lean4-current-defun-name) "«my thm»"))))

(ert-deftest lean4-defun-name-of-an-example-is-its-keyword ()
  "An `example' has no name to report."
  (lean4-defun-test--with-lean "example : True := tri|vial\n"
    (should (equal (lean4-current-defun-name) "example"))))

(ert-deftest lean4-defun-name-of-an-anonymous-instance ()
  "Nor has an instance which was not given one."
  (lean4-defun-test--with-lean "instance : Inhabited Nat where\n  default := |0\n"
    (should (equal (lean4-current-defun-name) "instance"))))

(ert-deftest lean4-defun-name-of-a-class-inductive ()
  "`class inductive' is two keywords and then the name."
  (lean4-defun-test--with-lean "class inductive Foo (α : Type) whe|re\n"
    (should (equal (lean4-current-defun-name) "Foo"))))

(ert-deftest lean4-defun-name-from-the-docstring ()
  "Point in the doc comment is already inside the declaration."
  (lean4-defun-test--with-lean "/-- D|oc. -/\ntheorem foo : True := trivial\n"
    (should (equal (lean4-current-defun-name) "foo"))))

(ert-deftest lean4-defun-name-is-nil-between-declarations ()
  "Between two declarations there is no declaration to name."
  (lean4-defun-test--with-lean
      "theorem a : True := trivial\n\n|\nnamespace Foo\n"
    (should-not (lean4-current-defun-name))))

(ert-deftest lean4-defun-name-is-nil-before-the-first ()
  "Nor in the header above them all."
  (lean4-defun-test--with-lean "import Fo|o\n\ntheorem a : True := trivial\n"
    (should-not (lean4-current-defun-name))))

(provide 'lean4-defun-test)
;;; lean4-defun-test.el ends here
