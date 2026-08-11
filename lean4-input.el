;;; lean4-input.el --- Lean4-Mode Input Method  -*- lexical-binding: t; -*-

;; Copyright (c) 2005-2012 Ulf Norell, Nils Anders Danielsson,
;; Catarina Coquand, Makoto Takeyama, Andreas Abel, Karl Mehltretter,
;; Marcin Benke, Darin Morrison.

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;; Commentary:

;; A highly customisable input method which can inherit from other
;; Quail input methods.  By default the input method is geared towards
;; the input of mathematical and other symbols in Lean programs.

;; Use M-x customize-group lean4-input to customise this input method.
;; Note that the functions defined under "Functions used to tweak
;; translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the
;; ones added specifically for this one.

;; Use lean4-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

;; This file is based on agda-input.el from Agda-Mode:
;; https://github.com/agda/agda/blob/d2cbd2dd/src/data/emacs-mode/agda-input.el

;;; Code:

(require 'quail)
(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'seq)

;; Quail is quite stateful, so be careful when editing this code.  Note
;; that with-temp-buffer is used below whenever buffer-local state is
;; modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun lean4-input-concat-map (f xs)
  "Concat (map F XS)."
  (mapcan f xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs
;;
;; These are the combinators `lean4-input-inherit' is written in terms of,
;; so they are part of that option's interface: a reader who has customized
;; it has these names in their own configuration.

(defun lean4-input-compose (f g)
  "\\x -> concatMap F (G x)."
  (lambda (x) (lean4-input-concat-map f (funcall g x))))

(defun lean4-input-or (f g)
  "\\x -> F x ++ G x."
  (lambda (x) (append (funcall f x) (funcall g x))))

(defun lean4-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
  (lambda (x)
    (if (equal (substring (car x) 0 (length prefix)) prefix)
        (list x))))

(defun lean4-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
  (lambda (x) (unless (member (car x) ss) (list x))))

(defun lean4-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
  (lambda (x) `((,(substring (car x) n) . ,(cdr x)))))

(defun lean4-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (lean4-input-compose
   (lean4-input-drop-beginning (length prefix))
   (lean4-input-prefix prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is set to 'lean4-input-incorporate-changed-setting
;; so that the input method gets updated immediately when users
;; customize it. However, the setup functions cannot be run before all
;; variables have been defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not performed
;; until lean4-input-setup is called at the end of this file.

(defgroup lean4-input nil
  "The Lean input method.
After tweaking these settings you may want to inspect the resulting
translations using `lean4-input-show-translations'."
  :group 'lean4
  :group 'leim)

(defcustom lean4-input-inherit
  `(("TeX" . (lean4-input-compose
              (lean4-input-drop '("geq" "leq" "bullet" "qed" "par"))
              (lean4-input-or
               (lean4-input-drop-prefix "\\")
               (lean4-input-or
                (lean4-input-compose
                 (lean4-input-drop '("^o"))
                 (lean4-input-prefix "^"))
                (lean4-input-prefix "_"))))))
  "List of parent Quail input methods.
Translations from these methods will be inherited by the Lean
input method (with the exception of translations corresponding to
ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same signature as
the argument of `lean4-input-add-translations'.

The inherited translation pairs are added last, after
`lean4-input-user-translations' and `lean4-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `lean4-input-setup' in
order for the change to take effect."
  :group 'lean4-input
  :set 'lean4-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom lean4-input-data-directory
  (expand-file-name "data/" (file-name-directory (or load-file-name (buffer-file-name))))
  "Directory in which abbreviations.json resides."
  :group 'lean4-input
  :type 'directory)

(defcustom lean4-input-user-translations nil
  "A list of translations specific to the Lean input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

These translation pairs are included first, before thoseinherited
from other input methods."
  :group 'lean4-input
  :set 'lean4-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun lean4-input-get-translations (qp)
  "Return all translations from the Quail package QP.
Result is a list of pairs (KEY-SEQUENCE . TRANSLATION)
that contains all translations from QP Except for those corresponding to ASCII."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package" qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

;; The Quail method replaces an abbreviation only once it is unambiguous, so
;; it cannot tell you what is available while you are still typing.  Offering
;; the same table through `completion-at-point-functions' fills that gap and
;; leaves the choice of interface -- company, corfu, the default buffer -- to
;; the user, which is where it belongs.

(defvar lean4-input--completion-cache nil
  "Cached completion candidates, as an alist of (KEY . TRANSLATIONS).
Building this walks the whole Quail map, which is slow enough to be
worth doing once.  `lean4-input-setup' clears it.")

(defun lean4-input--completion-candidates ()
  "Return the abbreviation table as an alist of (KEY . TRANSLATIONS).
KEY includes its leading backslash.  TRANSLATIONS is a list of strings."
  (or lean4-input--completion-cache
      (setq lean4-input--completion-cache
            (mapcar
             (lambda (rule)
               (cons (car rule) (lean4-input--translation-strings (cdr rule))))
             (lean4-input-get-translations "Lean")))))

(defun lean4-input--translation-strings (translation)
  "Normalise a Quail TRANSLATION to a list of strings.
`quail-build-decode-map' yields a character for a single one-character
translation, a string for a longer one, and a vector mixing the two when
a key sequence has several translations."
  (cond ((null translation) nil)
        ((integerp translation) (list (string translation)))
        ((stringp translation) (list translation))
        (t (seq-mapcat #'lean4-input--translation-strings translation #'list))))

(defun lean4-input--annotate (key)
  "Return the annotation shown beside completion candidate KEY."
  (when-let* ((translations (cdr (assoc key (lean4-input--completion-candidates)))))
    (concat " " (string-join translations " "))))

(defun lean4-input--exit (key status)
  "Replace the inserted abbreviation KEY with the character it denotes.
Does nothing unless STATUS is `finished', so that a candidate chosen
only to keep typing is left alone."
  (when (eq status 'finished)
    (when-let* ((translations (cdr (assoc key (lean4-input--completion-candidates))))
                (translation (car translations)))
      (delete-char (- (length key)))
      (insert translation))))

;;;###autoload
(defun lean4-input-completion-at-point ()
  "Complete the Lean Unicode abbreviation before point.
Meant for `completion-at-point-functions'.  Completes text like
\\='\\alpha\\=' and, once chosen, replaces it with the character it
stands for."
  (when-let* ((end (point))
              (start (save-excursion
                       ;; An abbreviation is a backslash and the
                       ;; non-whitespace after it, on one line.
                       (skip-chars-backward "^ \t\n\\\\")
                       (when (eq (char-before) ?\\)
                         (1- (point))))))
    (list start end
          (completion-table-dynamic
           (lambda (_) (mapcar #'car (lean4-input--completion-candidates))))
          :annotation-function #'lean4-input--annotate
          :exit-function #'lean4-input--exit
          ;; Other sources -- Eglot's, above all -- still get a say.
          :exclusive 'no)))

(defun lean4-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "Lean")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (lean4-input-get-translations qp)))))))

(defun lean4-input-add-translations (trans)
  "Add the given translations TRANS to the Lean input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION).  The
translations are appended to the current translations."
  (with-temp-buffer
    (map-do (lambda (key tr)
              (when key
                (quail-defrule (concat "\\" key)
                               tr
                               "Lean" t)))
            trans)))

(defun lean4-input-inherit-package (qp &optional fun)
  "Inherit translations from the Quail package QP.
Add all translations from the Quail package QP (except for those
corresponding to ASCII) to the list of Lean Quail rules.

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (lean4-input-get-translations qp)))
    (lean4-input-add-translations
     (if fun (lean4-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defun lean4-input-setup ()
  "Set up the Lean input method.
Use customisable variables and parent input methods to setup Lean input method."
  (setq lean4-input--completion-cache nil)

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package "Lean" "UTF-8" "∏" t ; guidance
     "Lean input method.
The purpose of this input method is to edit Lean programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

  (lean4-input-add-translations (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
                                        lean4-input-user-translations))
  (with-temp-buffer
    (insert-file-contents (expand-file-name
                           "abbreviations.json"
                           lean4-input-data-directory))
    (thread-last
      ;; A hash table with string keys, which `map' reads as happily as an
      ;; alist.
      (json-parse-buffer)
      (map-filter (lambda (_ s) (not (string-match-p "\\$CURSOR" s))))
      (map-apply (lambda (k s) (cons k (vector s))))
      lean4-input-add-translations))
  (dolist (def lean4-input-inherit)
    (lean4-input-inherit-package (car def)
                                (eval (cdr def)))))

(defun lean4-input-incorporate-changed-setting (sym val)
  "Update the Lean input method.
Set SYM default value to VAL, then call `lean4-input-setup'.
Suitable for use in the :set field of `defcustom'."
  (set-default sym val)
  (lean4-input-setup))

;; Set up the input method.

(cl-eval-when (load eval)
  (lean4-input-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export Translations

(defun lean4-input-export-translations ()
  "Export the current translations in a javascript format.
Print (input, output) pairs in Javascript format to the buffer
*lean4-translations*.  The output can be copy-pasted to
leanprover.github.io/tutorial/js/input-method.js"
  (interactive)
  (with-current-buffer
      (get-buffer-create "*lean4-translations*")
    (let ((exclude-list '("\\newline")))
      (insert "var corrections = {")
      (dolist (rule (seq-remove (lambda (rule) (member (car rule) exclude-list))
                                (lean4-input-get-translations "Lean")))
        (let ((input (substring (car rule) 1))
              (outputs (cdr rule)))
          (insert (format "%s:\"" (prin1-to-string input)))
          (cond ((vectorp outputs)
                 (insert (elt outputs 0)))
                (t (insert-char outputs)))
          (insert (format "\",\n"))))
      (insert "};"))))

(defun lean4-input-export-translations-to-stdout ()
  "Print current translations to stdout."
  (lean4-input-export-translations)
  (with-current-buffer "*lean4-translations*"
    (princ (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative details

;;;; Finding a symbol by name

;; The input method goes from an abbreviation to a symbol.  These go the other
;; way: they list every symbol with the abbreviations that produce it, so that
;; one can be found without knowing what it is called.  VS Code offers the same
;; three commands, and shows each symbol with its abbreviations beside it.

(defun lean4-input-symbols ()
  "Return every symbol the input method can produce, with its abbreviations.

An alist of symbol to the abbreviations that yield it, shortest first --
the order VS Code shows them in, the shortest being the one worth
learning.  Symbols reachable more than one way appear once."
  (let ((symbols nil))
    (pcase-dolist (`(,key . ,translations) (lean4-input--completion-candidates))
      (dolist (symbol translations)
        (let ((cell (assoc symbol symbols)))
          (if cell
              (setcdr cell (cons key (cdr cell)))
            (push (cons symbol (list key)) symbols)))))
    (dolist (cell symbols)
      (setcdr cell (sort (nreverse (cdr cell))
                         (lambda (a b)
                           (if (= (length a) (length b))
                               (string< a b)
                             (< (length a) (length b)))))))
    (nreverse symbols)))

(defun lean4-input--symbol-candidates ()
  "Return the symbols to offer, as an alist of what to show to the symbol.
The abbreviations are part of what is shown rather than an annotation, so
that a symbol can be found by typing one of them: completion matches
against the candidate itself."
  (mapcar (lambda (cell)
            (cons (format "%s  %s" (car cell) (string-join (cdr cell) " "))
                  (car cell)))
          (lean4-input-symbols)))

(defun lean4-input--read-symbol (prompt)
  "Read one of the input method\='s symbols, with PROMPT."
  (let ((candidates (lean4-input--symbol-candidates)))
    (unless candidates
      (user-error "The Lean input method has no abbreviations loaded"))
    (or (cdr (assoc (completing-read prompt candidates nil t) candidates))
        (user-error "No symbol chosen"))))

;;;###autoload
(defun lean4-input-insert-symbol (symbol)
  "Insert SYMBOL at point.

Interactively, offers every symbol the Lean input method can produce,
each shown with the abbreviations that yield it -- so it can be found by
typing either the symbol or an abbreviation.  For a symbol whose
abbreviation is already known, typing that abbreviation is quicker; this
is for the ones that are not.  The counterpart of VS Code\='s \"Insert
Unicode Symbol\"."
  (interactive (list (lean4-input--read-symbol "Insert symbol: ")))
  (insert symbol))

;;;###autoload
(defun lean4-input-copy-symbol (symbol)
  "Put SYMBOL in the kill ring.
For pasting somewhere the input method does not reach -- another
program, or a minibuffer prompt that has no input method of its own.  The
counterpart of VS Code\='s \"Copy Unicode Symbol\"."
  (interactive (list (lean4-input--read-symbol "Copy symbol: ")))
  (kill-new symbol)
  (message "Copied %s" symbol))

;;;###autoload
(defun lean4-input-find-symbol (symbol)
  "Look SYMBOL up and then choose what to do with it.

Reports the abbreviations that produce it, and offers to insert it or
copy it.  The counterpart of VS Code\='s \"Find Unicode Symbol\", which
likewise asks what to do only after the symbol has been found."
  (interactive (list (lean4-input--read-symbol "Find symbol: ")))
  (let ((abbreviations (cdr (assoc symbol (lean4-input-symbols)))))
    (pcase (car (read-multiple-choice
                 (format "%s (%s) " symbol
                         (if abbreviations
                             (string-join abbreviations " ")
                           "no abbreviation"))
                 '((?i "insert") (?c "copy") (?q "do nothing"))))
      (?i (insert symbol))
      (?c (kill-new symbol) (message "Copied %s" symbol))
      (_ (message "%s is %s" symbol
                  (if abbreviations
                      (string-join abbreviations " ")
                    "reachable by no abbreviation"))))))

(provide 'lean4-input)
;;; lean4-input.el ends here
