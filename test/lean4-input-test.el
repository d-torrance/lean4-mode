;;; lean4-input-test.el --- Tests for the Lean input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

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

;;;; Completion at point

(defun lean4-input-test--capf-in (text)
  "Return `lean4-input-completion-at-point' with TEXT before point."
  (with-temp-buffer
    (insert text)
    (lean4-input-completion-at-point)))

(ert-deftest lean4-input-capf-recognises-an-abbreviation ()
  "The capf claims the backslash-prefixed word before point."
  (with-temp-buffer
    (insert "def f := \\alph")
    (let ((capf (lean4-input-completion-at-point)))
      (should capf)
      (pcase-let ((`(,start ,end ,table . ,_) capf))
        ;; The completed region is the backslash and what follows it, not
        ;; the whole word before point.
        (should (equal (buffer-substring-no-properties start end) "\\alph"))
        (should (member "\\alpha" (all-completions "\\alph" table)))))))

(ert-deftest lean4-input-capf-declines-without-a-backslash ()
  "Ordinary words are left to other completion sources."
  (should-not (lean4-input-test--capf-in "def foo"))
  (should-not (lean4-input-test--capf-in "")))

(ert-deftest lean4-input-capf-is-not-exclusive ()
  "Other capfs, Eglot's above all, still get a turn."
  (let ((capf (lean4-input-test--capf-in "\\al")))
    (should (eq (plist-get (nthcdr 3 capf) :exclusive) 'no))))

(ert-deftest lean4-input-capf-exit-inserts-the-character ()
  "Choosing a candidate replaces the abbreviation with its character."
  (with-temp-buffer
    (insert "def f := \\alpha")
    (let* ((capf (lean4-input-completion-at-point))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (should (functionp exit))
      (funcall exit "\\alpha" 'finished)
      (should (equal (buffer-string) "def f := α")))))

(ert-deftest lean4-input-capf-exit-leaves-partial-input-alone ()
  "A candidate chosen mid-typing is not yet replaced."
  (with-temp-buffer
    (insert "def f := \\alpha")
    (let* ((capf (lean4-input-completion-at-point))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (funcall exit "\\alpha" 'try-again)
      (should (equal (buffer-string) "def f := \\alpha")))))

(ert-deftest lean4-input-capf-annotates-with-the-character ()
  "Candidates are annotated with what they produce."
  (should (string-search "α" (lean4-input--annotate "\\alpha"))))

(provide 'lean4-input-test)
;;; lean4-input-test.el ends here
