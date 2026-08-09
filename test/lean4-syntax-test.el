;;; lean4-syntax-test.el --- Tests for Lean font-lock and syntax  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Font-lock and syntax-table tests.  These exercise `lean4-mode' itself
;; rather than the language server, so they need no Lean toolchain.
;;
;; The nested block comment case is the one worth keeping: Lean's `/- -/'
;; comments nest, which needs an explicit syntax-table property, and it is a
;; recurring source of regressions.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defmacro lean4-syntax-test--with-buffer (text &rest body)
  "Evaluate BODY in a fontified `lean4-mode' buffer containing TEXT.
Point starts at `point-min'.  Any Eglot/LSP startup is suppressed: these
tests are about font-lock only."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun lean4-syntax-test--face-at (needle)
  "Return the face at the first character of NEEDLE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (should (search-forward needle nil t))
    (get-text-property (match-beginning 0) 'face)))

(defun lean4-syntax-test--face-includes-p (face expected)
  "Return non-nil if FACE is EXPECTED or a list containing it."
  (if (listp face) (memq expected face) (eq face expected)))

(ert-deftest lean4-syntax-line-comment ()
  "Text after `--' is a comment."
  (lean4-syntax-test--with-buffer "def f := 1 -- this is commentary\n"
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "this is commentary")
             'font-lock-comment-face))))

(ert-deftest lean4-syntax-block-comment ()
  "Text between `/-' and `-/' is a comment."
  (lean4-syntax-test--with-buffer "/- outer commentary -/\ndef f := 1\n"
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "outer commentary")
             'font-lock-comment-face))))

(ert-deftest lean4-syntax-block-comments-nest ()
  "Lean block comments nest, so the outer one runs to the outer `-/'.
If nesting were not handled, the first `-/' would close the comment and
the trailing text would be fontified as code."
  (lean4-syntax-test--with-buffer
      "/- outer /- inner -/ still commentary -/\ndef visible := 1\n"
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "still commentary")
             'font-lock-comment-face))
    ;; And the comment really does end: code after it is not commentary.
    (should-not (lean4-syntax-test--face-includes-p
                 (lean4-syntax-test--face-at "visible")
                 'font-lock-comment-face))))

(ert-deftest lean4-syntax-keywords ()
  "Declaration keywords get the keyword face."
  (dolist (keyword '("def" "theorem" "instance" "structure"))
    (lean4-syntax-test--with-buffer (concat keyword " f := 1\n")
      (should (lean4-syntax-test--face-includes-p
               (lean4-syntax-test--face-at keyword)
               'font-lock-keyword-face)))))

(ert-deftest lean4-syntax-sorry-is-a-warning ()
  "`sorry' is highlighted as a warning.
It admits a goal without proving it, so it must be visually loud."
  (lean4-syntax-test--with-buffer "theorem t : True := sorry\n"
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "sorry")
             'font-lock-warning-face))))

(ert-deftest lean4-syntax-string-literal ()
  "String literals get the string face."
  (lean4-syntax-test--with-buffer "def s := \"a string\"\n"
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "a string")
             'font-lock-string-face))))

(ert-deftest lean4-syntax-unicode-is-word-constituent ()
  "Characters Lean identifiers are built from have word syntax.
Note this is about the syntax table only.  Word *motion* additionally
honours script boundaries, so `forward-word' still stops between the
Greek and the subscript below -- that is Emacs behaviour, not something
`lean4-syntax-table' controls."
  (lean4-syntax-test--with-buffer ""
    (dolist (char '(?α ?β ?γ ?Γ ?₁ ?¹ ?ℕ ?𝒜))
      (should (eq (char-syntax char) ?w)))))

(ert-deftest lean4-syntax-unicode-identifier-is-one-symbol ()
  "A Lean identifier mixing scripts is a single symbol.
This is what matters in practice: point anywhere inside `αβ₁' and
commands that act on the symbol at point see all of it."
  (lean4-syntax-test--with-buffer "def αβ₁ := 1\n"
    (goto-char (point-min))
    (should (search-forward "αβ₁" nil t))
    (goto-char (1+ (match-beginning 0)))
    (should (equal (thing-at-point 'symbol) "αβ₁"))
    (should (equal (bounds-of-thing-at-point 'symbol)
                   (cons (match-beginning 0) (match-end 0))))))

(ert-deftest lean4-syntax-guillemet-identifier-body-is-not-comment ()
  "The body of a guillemet-escaped identifier is not fontified as comment.
`lean4-syntax-table' gives `«' and `»' comment syntax so that arbitrary
text between them -- Lean allows spaces and operators there -- cannot
derail the parser.  A font-lock rule then paints the body back to the
default face and leaves only the delimiters looking like comment.  This
pins that second half: without it, escaped identifiers read as comments.

Note the delimiters keep comment *syntax*, so `thing-at-point' does not
see the whole thing as one symbol.  That is a known limitation."
  (lean4-syntax-test--with-buffer "def «hello world» := 1\n"
    (should-not (lean4-syntax-test--face-includes-p
                 (lean4-syntax-test--face-at "hello world")
                 'font-lock-comment-face))
    (should (lean4-syntax-test--face-includes-p
             (lean4-syntax-test--face-at "«")
             'font-lock-comment-face))))

(ert-deftest lean4-syntax-comment-start-is-lean ()
  "`comment-start' is set so that \\[comment-dwim] inserts `--'."
  (lean4-syntax-test--with-buffer ""
    (should (equal (string-trim comment-start) "--"))))

;;;; Menu

(defun lean4-syntax-test--menu-item (command)
  "Return the `menu-item' entry bound to COMMAND in the Lean menu."
  (let ((submenu (lookup-key lean4-mode-map
                             (vector 'menu-bar (intern "lean 4"))))
        found)
    (when (keymapp submenu)
      (map-keymap (lambda (_key item)
                    (when (and (consp item) (memq command item))
                      (setq found item)))
                  submenu))
    found))

(ert-deftest lean4-menu-pin-label-is-dynamic ()
  "The pin menu item says what the next invocation will do.

Regression test.  Easymenu has two vector forms -- [NAME CMD ENABLE]
and [NAME CMD :keyword value ...] -- and mixing them silently drops
every keyword after the positional ENABLE.  Written that way the label
was a constant string, so the menu offered to pin whatever the state.
Easymenu puts a dynamic label in place of the item name, so what this
checks is that the name is a form rather than a string."
  (let ((item (lean4-syntax-test--menu-item 'lean4-info-toggle-pin)))
    (should item)
    (should-not (stringp (nth 1 item)))
    (should (consp (nth 1 item)))
    ;; And that it actually reads both ways.
    (let ((lean4-info--pin nil))
      (should (equal (eval (nth 1 item) t) "Pin goal display")))
    (let ((lean4-info--pin (point-marker)))
      (should (equal (eval (nth 1 item) t) "Unpin goal display")))))

(ert-deftest lean4-menu-pause-label-is-dynamic ()
  "The pause menu item likewise says what it will do."
  (let ((item (lean4-syntax-test--menu-item 'lean4-info-toggle-pause)))
    (should item)
    (should (consp (nth 1 item)))
    (let ((lean4-info-paused nil))
      (should (equal (eval (nth 1 item) t) "Pause goal display")))
    (let ((lean4-info-paused t))
      (should (equal (eval (nth 1 item) t) "Unpause goal display")))))

(provide 'lean4-syntax-test)
;;; lean4-syntax-test.el ends here
