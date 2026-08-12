;;; lean4-util-test.el --- Tests for the shared utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for what the libraries share.  The frame is stood in for here:
;; what a real one can draw is a property of the machine the tests run on.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-util)

(ert-deftest lean4-util-glyph-wants-a-font-and-not-a-fontset ()
  "A character no font can draw is not displayable, whatever the fontset says.

Regression test.  `char-displayable-p' answers for whole blocks of
Unicode rather than for the character asked about, so on a graphical
frame it said yes to the tool U+1F6E0 on a machine whose emoji font
carries everything around it and not that one: the unsolved-goals
marker came out as a box with 1F6E0 printed inside it."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'char-displayable-p) (lambda (_) t))
            ((symbol-function 'internal-char-font)
             (lambda (_position character)
               (and (/= character ?\N{HAMMER AND WRENCH}) '(font . 0)))))
    (should-not (lean4--char-displayable-p ?\N{HAMMER AND WRENCH}))
    (should (lean4--char-displayable-p ?\N{HAMMER AND PICK}))
    ;; So the next candidate is what gets drawn.
    (should (equal (lean4--glyph nil
                                 '("\N{HAMMER AND WRENCH}"
                                   "\N{HAMMER AND PICK}")
                                 "(goals)")
                   "\N{HAMMER AND PICK}"))))

(ert-deftest lean4-util-glyph-asks-a-terminal-about-its-coding-system ()
  "A terminal has no font to ask about, and is not asked for one."
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'internal-char-font)
             (lambda (&rest _) (error "A terminal has no fonts"))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (lean4--char-displayable-p ?\N{HAMMER AND PICK})))
    (cl-letf (((symbol-function 'char-displayable-p) #'ignore))
      (should-not (lean4--char-displayable-p ?\N{HAMMER AND PICK}))
      (should (equal (lean4--glyph nil '("⇅" "↕") "S") "S")))))

(ert-deftest lean4-util-configured-glyph-wins ()
  "What the user asked for is used whatever the frame can draw."
  (cl-letf (((symbol-function 'lean4--displayable-p) (lambda (&rest _) t)))
    (should (equal (lean4--glyph "!" '("⇅") "S") "!"))))

;;;; A dependency's own sources

(ert-deftest lean4-util-dependency-files-are-recognised ()
  "Anything under a `.lake' or `.elan' directory belongs to something else,
at any depth: a package vendored under `.lake/packages' keeps its sources
several levels down."
  (should (lean4--dependency-file-p "/home/x/p/.lake/packages/mathlib/Mathlib/Order/Basic.lean"))
  (should (lean4--dependency-file-p "/home/x/.elan/toolchains/v4.33.0/src/lean/Init/Prelude.lean"))
  (should (lean4--dependency-file-p "/home/x/p/.lake/build/ir/Foo.lean")))

(ert-deftest lean4-util-a-project-s-own-files-are-not ()
  "The files one is writing are not, however the directories are named."
  (should-not (lean4--dependency-file-p "/home/x/p/Foo/Bar.lean"))
  (should-not (lean4--dependency-file-p "/home/x/lakeish/Foo.lean"))
  (should-not (lean4--dependency-file-p "/home/x/elan-notes/Foo.lean"))
  (should-not (lean4--dependency-file-p nil)))

(provide 'lean4-util-test)
;;; lean4-util-test.el ends here
