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
(require 'cl-lib)
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

;;;; Finding a symbol by name

(ert-deftest lean4-input-symbols-group-by-symbol ()
  "Each symbol appears once, with every abbreviation that yields it."
  (cl-letf (((symbol-function 'lean4-input--completion-candidates)
             (lambda () '(("\\a" "α") ("\\alpha" "α") ("\\b" "β")))))
    (let ((symbols (lean4-input-symbols)))
      (should (equal (length symbols) 2))
      (should (equal (cdr (assoc "α" symbols)) '("\\a" "\\alpha")))
      (should (equal (cdr (assoc "β" symbols)) '("\\b"))))))

(ert-deftest lean4-input-symbols-put-the-shortest-first ()
  "The shortest abbreviation is the one worth learning, so it leads.
Ties go alphabetically rather than in whatever order the table was read."
  (cl-letf (((symbol-function 'lean4-input--completion-candidates)
             (lambda () '(("\\alpha" "α") ("\\Ga" "α") ("\\a" "α")))))
    (should (equal (cdr (assoc "α" (lean4-input-symbols)))
                   '("\\a" "\\Ga" "\\alpha")))))

(ert-deftest lean4-input-candidates-can-be-searched-by-abbreviation ()
  "The abbreviations are in the candidate, not merely annotating it.
Completion matches the candidate string, so an annotation could not be
typed to find a symbol."
  (cl-letf (((symbol-function 'lean4-input--completion-candidates)
             (lambda () '(("\\a" "α") ("\\alpha" "α")))))
    (let ((candidates (lean4-input--symbol-candidates)))
      (should (equal candidates '(("α  \\a \\alpha" . "α"))))
      (should (string-search "\\alpha" (car (car candidates)))))))

(ert-deftest lean4-input-insert-symbol-inserts-it ()
  "The command puts the chosen symbol in the buffer."
  (with-temp-buffer
    (lean4-input-insert-symbol "α")
    (should (equal (buffer-string) "α"))))

(ert-deftest lean4-input-copy-symbol-fills-the-kill-ring ()
  "Copying is for pasting where the input method does not reach."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (lean4-input-copy-symbol "∑"))
    (should (equal (current-kill 0) "∑"))))

(ert-deftest lean4-input-find-symbol-then-acts ()
  "Looking a symbol up offers to insert it or copy it."
  (cl-letf (((symbol-function 'lean4-input-symbols) (lambda () '(("α" "\\a")))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?i "insert"))))
        (lean4-input-find-symbol "α"))
      (should (equal (buffer-string) "α")))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?c "copy")))
                ((symbol-function 'message) #'ignore))
        (lean4-input-find-symbol "α"))
      (should (equal (current-kill 0) "α")))
    ;; Declining reports the abbreviations and changes nothing.
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?q "do nothing")))
                ((symbol-function 'message) #'ignore))
        (lean4-input-find-symbol "α"))
      (should (equal (buffer-string) "")))))

(ert-deftest lean4-input-symbols-are-really-there ()
  "Against the shipped abbreviation table rather than a stub."
  (let ((symbols (lean4-input-symbols)))
    (should (> (length symbols) 1000))
    (should (member "\\a" (cdr (assoc "α" symbols))))
    (should (member "\\sum" (cdr (assoc "∑" symbols))))))

;;;; The leader

(defmacro lean4-input-test--with-leader (leader &rest body)
  "Evaluate BODY with LEADER opening every abbreviation.
The rule table is rebuilt for it and rebuilt again afterwards, the input
method being one Quail package shared by every buffer in the session."
  (declare (indent 1) (debug (form body)))
  `(unwind-protect
       (let ((lean4-input-leader ,leader))
         (lean4-input-setup)
         ,@body)
     (lean4-input-setup)))

(ert-deftest lean4-input-leader-opens-the-abbreviations ()
  "With another leader, that is what an abbreviation begins with."
  (lean4-input-test--with-leader "#"
    (should (member "α" (lean4-input-test--strings
                         (cdr (assoc "#alpha"
                                     (lean4-input-get-translations "Lean"))))))))

(ert-deftest lean4-input-leader-replaces-the-backslash ()
  "And the backslash is no longer one, the table being rebuilt rather than
added to.  Otherwise both would work, which is not what was asked for."
  (lean4-input-test--with-leader "#"
    (should-not (assoc "\\alpha" (lean4-input-get-translations "Lean")))))

(ert-deftest lean4-input-leader-is-restored ()
  "The macro above leaves the session as it found it."
  (should (member "α" (lean4-input-test--strings-for "alpha"))))

(ert-deftest lean4-input-capf-follows-the-leader ()
  "Completion claims what the leader opened, not what a backslash did."
  (lean4-input-test--with-leader "#"
    (with-temp-buffer
      (insert "def f := #alph")
      (pcase-let ((`(,start ,end ,table . ,_)
                   (lean4-input-completion-at-point)))
        (should (equal (buffer-substring-no-properties start end) "#alph"))
        (should (member "#alpha" (all-completions "#alph" table)))))
    (should-not (lean4-input-test--capf-in "def f := \\alph"))))

(ert-deftest lean4-input-capf-stops-at-whitespace ()
  "An abbreviation is the leader and the non-whitespace after it, so a
leader further back on the line is not one for the word at point."
  (should-not (lean4-input-test--capf-in "\\alpha and then foo")))

(provide 'lean4-input-test)
;;; lean4-input-test.el ends here
