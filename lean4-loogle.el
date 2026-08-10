;;; lean4-loogle.el --- Search Mathlib with Loogle -*- lexical-binding: t -*-

;; Copyright (C) 2026 Doug Torrance

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

;; Loogle <https://loogle.lean-lang.org> searches Mathlib by shape rather than
;; by name: `?a + ?b = ?b + ?a' finds the commutativity lemmas, `Nat.succ_le'
;; finds what is named that, and `|- tsum _ = _' finds what concludes it.  This
;; asks it over its JSON endpoint and lists the answers.
;;
;; The one thing worth saying about the query: it is read with the Lean input
;; method inherited, so `\\alpha' becomes α in the minibuffer exactly as it
;; would in a Lean buffer.  Loogle's syntax leans on notation, and a search
;; tool that cannot type ∑ is not much use for Mathlib.
;;
;; Nothing here needs a Lean server: Loogle is a web service, and the answers
;; are about Mathlib rather than about the project in hand.

;;; Code:

(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'url)

(defgroup lean4-loogle nil
  "Searching Mathlib with Loogle."
  :group 'lean4)

(defcustom lean4-loogle-url "https://loogle.lean-lang.org/json"
  "The Loogle endpoint to ask.
Point this at your own instance to search a library of your own."
  :group 'lean4-loogle
  :type 'string)

(defcustom lean4-loogle-documentation-url
  "https://leanprover-community.github.io/mathlib4_docs/"
  "Where the documentation for a result lives.
A module\\='s page is this, then the module name with its dots turned into
slashes, then \".html\"."
  :group 'lean4-loogle
  :type 'string)

(defconst lean4-loogle-buffer-name "*Lean Loogle*")

(defvar-local lean4-loogle--query nil
  "The query the results on display answer.")

;;;; Faces

(defface lean4-loogle-name
  '((t :inherit magit-section-heading))
  "Face for the name of a result."
  :group 'lean4-loogle)

(defface lean4-loogle-module
  '((t :inherit shadow))
  "Face for the module a result lives in."
  :group 'lean4-loogle)

(defface lean4-loogle-header
  '((t :inherit font-lock-comment-face))
  "Face for what Loogle says about the search as a whole."
  :group 'lean4-loogle)

;;;; Asking

(defun lean4-loogle--client-string ()
  "Return how to identify this client to Loogle.
It asks for one, in a header of its own, and an operator fielding an odd
query is owed the chance to know where it came from."
  (format "Emacs/%s lean4-mode" emacs-version))

(defun lean4-loogle--query-url (query)
  "Return the URL that asks Loogle QUERY."
  (concat lean4-loogle-url "?q=" (url-hexify-string query)))

(defun lean4-loogle--parse (buffer)
  "Return the JSON body of the response in BUFFER.
Signals if there is no body or it does not parse, which is what a
proxy\\='s error page looks like from here."
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (re-search-forward "^\r?$" nil t)
      (error "Loogle sent no response body"))
    (forward-line)
    (condition-case nil
        (json-parse-buffer :object-type 'plist :array-type 'list
                           :null-object nil :false-object nil)
      (error (error "Loogle sent something that is not JSON")))))

(defun lean4-loogle--fetch (query callback)
  "Ask Loogle QUERY and call CALLBACK with the parsed response.
CALLBACK is called with nil if the request failed, having reported why."
  (let ((url-request-extra-headers
         (list (cons "X-Loogle-Client" (lean4-loogle--client-string)))))
    (url-retrieve
     (lean4-loogle--query-url query)
     (lambda (status)
       (let ((response
              (condition-case error
                  (if-let* ((problem (plist-get status :error)))
                      (progn (message "Could not reach Loogle: %s"
                                      (error-message-string problem))
                             nil)
                    (lean4-loogle--parse (current-buffer)))
                (error (message "Could not read Loogle's answer: %s"
                                (error-message-string error))
                       nil))))
         (kill-buffer (current-buffer))
         (funcall callback response)))
     nil 'silent 'inhibit-cookies)))

;;;; Reading the answer

(defun lean4-loogle--hits (response)
  "Return the results in RESPONSE."
  (plist-get response :hits))

(defun lean4-loogle--documentation-url (hit)
  "Return where HIT is documented.

Lean\\='s documentation is a page per module with an anchor per
declaration.  The dots of a module name are the separators of a path;
this is not quite right for a name with a dot escaped inside french
quotes, and VS Code says the same of its own version."
  (let ((module (or (plist-get hit :module) ""))
        (name (or (plist-get hit :name) "")))
    (concat lean4-loogle-documentation-url
            (string-replace "." "/" module)
            ".html#" (url-hexify-string name))))

;;;; Drawing

(defclass lean4-loogle-section (magit-section) nil
  :documentation "A section of the Loogle results.")

(defun lean4-loogle--insert-hit (hit)
  "Insert HIT as a section of its own."
  ;; The hit itself is the section's value, so the commands below have it
  ;; whole rather than reading it back out of the text.
  (magit-insert-section (lean4-loogle-section (list 'hit hit))
    (magit-insert-heading
     (concat (propertize (or (plist-get hit :name) "?")
                         'font-lock-face 'lean4-loogle-name)
             (when-let* ((module (plist-get hit :module)))
               (concat (propertize " @ " 'font-lock-face 'lean4-loogle-module)
                       (propertize module 'font-lock-face
                                   'lean4-loogle-module)))
             "\n"))
    (magit-insert-section-body
      (insert "  " (string-trim (or (plist-get hit :type) "")) "\n")
      (when-let* ((doc (plist-get hit :doc))
                  ((not (string-empty-p (string-trim doc)))))
        (insert "  " (propertize (string-trim doc)
                                 'font-lock-face 'font-lock-doc-face)
                "\n")))))

(defun lean4-loogle--insert-suggestions (suggestions)
  "Insert SUGGESTIONS, each a button that searches for it."
  (when suggestions
    (magit-insert-section (lean4-loogle-section 'suggestions)
      (magit-insert-heading "Did you mean\n")
      (magit-insert-section-body
        (dolist (suggestion suggestions)
          (insert "  ")
          (insert-text-button
           suggestion
           'action (lambda (button)
                     (lean4-loogle-search (button-get button 'lean4-query)))
           'lean4-query suggestion
           'help-echo "Search Loogle for this instead"
           'follow-link t)
          (insert "\n"))))))

(defun lean4-loogle--render (query response)
  "Draw RESPONSE to QUERY in the Loogle buffer."
  (with-current-buffer (get-buffer-create lean4-loogle-buffer-name)
    (unless (derived-mode-p 'lean4-loogle-mode)
      (lean4-loogle-mode))
    (setq lean4-loogle--query query)
    (let ((inhibit-read-only t)
          (hits (lean4-loogle--hits response)))
      (erase-buffer)
      (setq header-line-format (format "Loogle: %s" query))
      (magit-insert-section (lean4-loogle-section 'root)
        (when-let* ((header (plist-get response :header))
                    ((not (string-empty-p header))))
          (insert (propertize header 'font-lock-face 'lean4-loogle-header)
                  "\n\n"))
        (when-let* ((problem (plist-get response :error))
                    ((not (string-empty-p problem))))
          (insert (propertize problem 'font-lock-face 'error) "\n\n"))
        (cond
         (hits
          (magit-insert-section (lean4-loogle-section 'hits)
            (magit-insert-heading
             (format "%d result%s\n" (length hits)
                     (if (= (length hits) 1) "" "s")))
            (mapc #'lean4-loogle--insert-hit hits)))
         ;; Loogle explains an unparseable query in `error'; saying "no
         ;; results" as well would suggest the search ran and found none.
         ((string-empty-p (or (plist-get response :error) ""))
          (insert "No results.\n")))
        (insert "\n")
        (lean4-loogle--insert-suggestions (plist-get response :suggestions))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;; The buffer

(defun lean4-loogle--hit-at-point ()
  "Return the result point is in, as Loogle sent it, or nil.
Walks out to the enclosing result: point is usually on the type or the
docstring rather than on the name."
  (let ((section (magit-current-section)))
    (while (and section
                (not (let ((value (oref section value)))
                       (and (consp value) (eq (car value) 'hit)))))
      (setq section (oref section parent)))
    (when section (cadr (oref section value)))))

(defun lean4-loogle-browse-documentation ()
  "Open the documentation for the result at point.
What clicking the name does in VS Code."
  (interactive)
  (let ((hit (or (lean4-loogle--hit-at-point)
                 (user-error "No result at point"))))
    (browse-url (lean4-loogle--documentation-url hit))))

(defun lean4-loogle-copy-name ()
  "Put the name of the result at point in the kill ring."
  (interactive)
  (let* ((hit (or (lean4-loogle--hit-at-point)
                  (user-error "No result at point")))
         (name (or (plist-get hit :name) (user-error "This result has no name"))))
    (kill-new name)
    (message "Copied %s" name)))

(defun lean4-loogle-again ()
  "Ask Loogle the same query again."
  (interactive)
  (lean4-loogle-search (or lean4-loogle--query
                           (user-error "No query to repeat"))))

(defvar-keymap lean4-loogle-mode-map
  :doc "Keymap for the *Lean Loogle* buffer."
  :parent magit-section-mode-map
  "RET" #'lean4-loogle-browse-documentation
  "w"   #'lean4-loogle-copy-name
  "g"   #'lean4-loogle-again
  "q"   #'quit-window)

(easy-menu-define lean4-loogle-mode-menu lean4-loogle-mode-map
  "Menu for the *Lean Loogle* buffer."
  '("Loogle"
    ["Browse this result's documentation" lean4-loogle-browse-documentation t]
    ["Copy this result's name" lean4-loogle-copy-name t]
    ["Search again" lean4-loogle-again t]
    "--"
    ["Quit" quit-window t]))

(define-derived-mode lean4-loogle-mode magit-section-mode "Loogle"
  "Major mode for the *Lean Loogle* buffer.

\\{lean4-loogle-mode-map}"
  :interactive nil
  :group 'lean4-loogle
  (setq-local revert-buffer-function (lambda (&rest _) (lean4-loogle-again))))

;;;###autoload
(defun lean4-loogle-search (query)
  "Search Mathlib for QUERY with Loogle.

Interactively, the query is read with the Lean input method inherited, so
`\\alpha' becomes α as it would in a Lean buffer -- Loogle\\='s syntax
leans on notation.  An active region is offered as the starting point.

Loogle searches by shape: `?a + ?b = ?b + ?a' for the commutativity
lemmas, `|- tsum _ = _' for what concludes such a thing, a name for what
is called that.  Its own page documents the syntax.  The counterpart of
VS Code\\='s \"Search With Loogle\"."
  (interactive
   (list (read-string
          "Loogle: "
          (when (use-region-p)
            (string-trim (buffer-substring-no-properties
                          (region-beginning) (region-end))))
          nil nil
          ;; Inherit the input method: this is the whole reason the prompt
          ;; is worth having rather than a browser tab.
          t)))
  (when (string-empty-p (string-trim query))
    (user-error "Nothing to search for"))
  (message "Asking Loogle about %s..." query)
  (lean4-loogle--fetch
   query
   (lambda (response)
     (when response
       (lean4-loogle--render query response)))))

(provide 'lean4-loogle)
;;; lean4-loogle.el ends here
