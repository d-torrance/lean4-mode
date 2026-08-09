;;; lean4-input-test.el --- Tests for the Lean input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for `lean4-input', the Quail input method fed by
;; data/abbreviations.json.  That file is synchronised from vscode-lean4 by
;; .github/workflows/update-abbr.yml, so these tests double as a guard on
;; that workflow: if the upstream format changes, the abbreviation table
;; stops loading and these fail.

;;; Code:

(require 'ert)
(require 'seq)
(require 'lean4-input)

(defun lean4-input-test--strings (translation)
  "Normalise a Quail TRANSLATION to a list of strings.
`quail-build-decode-map' yields a character when a key sequence has a
single one-character translation, a string when that translation has
several characters (\"∀ᶠ\"), and a vector mixing the two when the key
sequence has more than one translation."
  (cond ((null translation) nil)
        ((integerp translation) (list (string translation)))
        ((stringp translation) (list translation))
        (t (seq-mapcat #'lean4-input-test--strings translation #'list))))

(defun lean4-input-test--strings-for (key)
  "Return the strings the Lean input method produces for KEY.
KEY is written without its leading backslash."
  (lean4-input-test--strings
   (cdr (assoc (concat "\\" key) (lean4-input-get-translations "Lean")))))

(ert-deftest lean4-input-loads-abbreviations ()
  "The bundled abbreviation table loads and is not trivially small."
  (should (> (length (lean4-input-get-translations "Lean")) 1000)))

(ert-deftest lean4-input-known-translations ()
  "Well-known abbreviations map to the expected characters.
These are stable Lean spellings; if one of them moves, the table was
either mis-parsed or upstream changed something significant."
  (pcase-dolist (`(,key ,text)
                 '(("alpha"  "α")
                   ("beta"   "β")
                   ("lambda" "λ")
                   ("fun"    "λ")
                   ("to"     "→")
                   ("r"      "→")
                   ("N"      "ℕ")
                   ("in"     "∈")
                   ("ne"     "≠")))
    (should (member text (lean4-input-test--strings-for key)))))

(ert-deftest lean4-input-anonymous-constructor-brackets ()
  "The anonymous-constructor brackets are available.
These are what `\\<' and `\\>' produce, and they are the pair most
likely to break if the JSON were parsed with the wrong key type."
  (should (member "⟨" (lean4-input-test--strings-for "<")))
  (should (member "⟩" (lean4-input-test--strings-for ">"))))

(ert-deftest lean4-input-multiple-translations-are-kept ()
  "A key sequence with several translations keeps all of them.
`\\l' is the canonical example: it offers both a left arrow and a
Polish l."
  (let ((translations (lean4-input-test--strings-for "l")))
    (should (> (length translations) 1))
    (should (member "←" translations))))

(ert-deftest lean4-input-drops-cursor-placeholders ()
  "No translation contains vscode-lean4's $CURSOR placeholder.
`lean4-input-setup' filters these out because Quail has no equivalent;
letting one through would insert the literal text \"$CURSOR\"."
  (pcase-dolist (`(,_key . ,translation) (lean4-input-get-translations "Lean"))
    (dolist (text (lean4-input-test--strings translation))
      (should-not (string-match-p "\\$CURSOR" text)))))

(ert-deftest lean4-input-user-translations-are-applied ()
  "Entries in `lean4-input-user-translations' reach the input method."
  (unwind-protect
      (let ((lean4-input-user-translations '(("lean4testsym" . ("✿")))))
        (lean4-input-setup)
        (should (member "✿" (lean4-input-test--strings-for "lean4testsym"))))
    ;; Restore the pristine table for the rest of the run.
    (lean4-input-setup))
  (should-not (lean4-input-test--strings-for "lean4testsym")))

(provide 'lean4-input-test)
;;; lean4-input-test.el ends here
