;;; lean4-module.el --- Lean's module hierarchy -*- lexical-binding: t -*-

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

;; What a module imports, and what imports it, as a tree.  Lean answers three
;; requests for this:
;;
;;   `$/lean/prepareModuleHierarchy'    the module a file is, if it is one
;;   `$/lean/moduleHierarchy/imports'   what a module imports
;;   `$/lean/moduleHierarchy/importedBy'  what imports a module
;;
;; A level at a time, which is the only way it could be: Mathlib's import
;; graph is far too large to send at once, and most of it is never looked at.
;; So the tree is fetched as it is opened, the way traces are in
;; `lean4-info'.  A module already on the path to the root is drawn as a leaf
;; rather than expanded, since following it would go round for ever.
;;
;; VS Code puts this in its own view and calls it Alt+Shift+M and Alt+Shift+N.
;; Here it is a `magit-section' buffer, which is what the rest of this package
;; draws trees with, so it folds and moves the way the goal display does.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'seq)

(require 'lean4-eglot)

(defgroup lean4-module nil
  "Lean\\='s module hierarchy."
  :group 'lean4)

(defconst lean4-module-buffer-name "*Lean Module Hierarchy*")

(defvar-local lean4-module--root nil
  "The `LeanModule' the tree on display is rooted at.")

(defvar-local lean4-module--direction 'imports
  "Which way the tree runs: `imports' or `imported-by'.")

(defvar-local lean4-module--children nil
  "What each module\\='s children turned out to be.
Keyed by path -- the module names from the node up to the root -- rather
than by module, so that the same module appearing in two places has its
own subtree in each.  A value is a list of `LeanImport'.")

(defvar-local lean4-module--pending nil
  "Modules whose children have not been asked for yet.
Keyed by path, as `lean4-module--children' is; the value is the
`LeanModule' to ask about.  A path leaves here as soon as it has been
asked for, so that opening a section cannot ask twice.")

(defvar-local lean4-module--source nil
  "A buffer the server can be reached through.
This buffer is not one Eglot manages, so the connection has to be
borrowed from one that is.")

;;;; Reading what the server sends

(defun lean4-module--name (module)
  "Return MODULE\\='s name."
  (plist-get module :name))

(defun lean4-module--uri (module)
  "Return MODULE\\='s URI."
  (plist-get module :uri))

(defun lean4-module--import-module (import)
  "Return the module IMPORT is of."
  (plist-get import :module))

(defun lean4-module--modifiers (import)
  "Return IMPORT\\='s modifiers, as a list of strings.

Lean records these only for modules using the `module' keyword; on
anything else the flags are absent and this is empty.  `full' means the
module is imported twice, once with `meta' and once without."
  (let ((kind (plist-get import :kind)))
    (delq nil
          (list (and (eq (plist-get kind :isPrivate) t) "private")
                (and (eq (plist-get kind :isAll) t) "all")
                (pcase (plist-get kind :metaKind)
                  ("meta" "meta")
                  ("full" "meta and non-meta")
                  (_ nil))))))

;;;; Asking

(defun lean4-module--server ()
  "Return the server to ask, or signal if there is none."
  (or (and (buffer-live-p lean4-module--source)
           (with-current-buffer lean4-module--source (eglot-current-server)))
      (user-error "No Lean server is running for this hierarchy any more")))

(defun lean4-module-supported-p ()
  "Return non-nil if the server for this buffer reports a module hierarchy.
Lean advertises it under `experimental', and servers older than the
feature say nothing at all."
  (and (eglot-current-server)
       (lean4--server-capable :experimental :moduleHierarchyProvider)
       t))

(defun lean4-module--request (method params success)
  "Ask the server METHOD with PARAMS and call SUCCESS with the result.
An error is reported rather than ignored: unlike a goal request, nothing
here is asked speculatively, so a failure is one the reader asked for and
should hear about."
  (jsonrpc-async-request
   (lean4-module--server) method params
   :success-fn success
   :error-fn (lambda (error)
               (message "Lean could not answer %s: %s" method
                        (plist-get error :message)))
   :timeout-fn (lambda () (message "Lean did not answer %s" method))))

(defun lean4-module--children-method (direction)
  "Return the request that walks DIRECTION."
  (if (eq direction 'imported-by)
      :$/lean/moduleHierarchy/importedBy
    :$/lean/moduleHierarchy/imports))

(defun lean4-module--fetch-children (path module)
  "Ask what MODULE at PATH leads to, and redraw when the answer comes."
  (let ((buffer (current-buffer))
        (method (lean4-module--children-method lean4-module--direction)))
    (lean4-module--request
     method (list :module module)
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (puthash path (append result nil) lean4-module--children)
           (lean4-module--redisplay)))))))

(defun lean4-module--fetch-open ()
  "Ask about any node the reader has opened and not yet asked about.

`magit-section' offers no hook that runs when one of its own commands
opens a section, so notice it afterwards -- the same arrangement
`lean4-info' uses for a trace\\='s children."
  (when lean4-module--pending
    (lean4-module--map-sections
     (lambda (section)
       (let ((value (oref section value)))
         (when (and (consp value) (eq (car value) 'module)
                    (not (oref section hidden)))
           (let* ((path (cadr value))
                  (module (gethash path lean4-module--pending)))
             (when module
               ;; Once: the answer comes back asynchronously, and asking
               ;; again in the meantime would ask for ever.
               (remhash path lean4-module--pending)
               (lean4-module--fetch-children path module)))))))))

(defun lean4-module--map-sections (function)
  "Call FUNCTION on every section of the tree, the root included."
  (when (and (boundp 'magit-root-section) magit-root-section)
    (letrec ((walk (lambda (section)
                     (funcall function section)
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))))

;;;; Drawing

(defclass lean4-module-section (magit-section) nil
  :documentation "A node of the module hierarchy.")

(defmacro lean4-module--section-body (&rest body)
  "Insert BODY as a section\\='s body, remembering how far in it sits.

`magit-section' puts the body of a section that starts folded aside and
runs it when the reader opens the section.  How far in we were is a
dynamic binding, and it has long unwound by then, so a body run that way
would come out at the outermost level -- every child of a folded node
drawn hard left.  Captured here, and bound again when the body finally
runs.  `lean4-info' needs the same thing of its traces."
  (declare (indent 0) (debug t))
  `(let ((indent lean4-module--indent))
     (magit-insert-section-body
       (let ((lean4-module--indent indent))
         ,@body))))

(defface lean4-module-name
  '((t :inherit magit-section-heading))
  "Face for a module\\='s name in the hierarchy."
  :group 'lean4-module)

(defface lean4-module-modifier
  '((t :inherit shadow))
  "Face for an import\\='s modifiers."
  :group 'lean4-module)

(defface lean4-module-repeated
  '((t :inherit shadow :slant italic))
  "Face for the note on a module already shown further up."
  :group 'lean4-module)

(defvar lean4-module--indent 0
  "How far in the node being drawn sits.")

(defun lean4-module--heading (module modifiers repeated)
  "Return the heading for MODULE, with MODIFIERS, marked if REPEATED."
  (concat
   (make-string (* 2 lean4-module--indent) ?\s)
   (propertize (or (lean4-module--name module) "(unnamed)")
               'font-lock-face 'lean4-module-name)
   (when modifiers
     (concat " " (propertize (mapconcat #'identity modifiers ", ")
                             'font-lock-face 'lean4-module-modifier)))
   (when repeated
     (propertize "  (shown above)" 'font-lock-face 'lean4-module-repeated))
   "\n"))

(defun lean4-module--insert-node (module modifiers path)
  "Insert MODULE at PATH as a section, headed with MODIFIERS.

A node whose children are known is given them whether or not it is open,
and `magit-section' hides them; one whose children have not arrived is
given a placeholder and remembered, so that opening it asks.  A node with
no children at all is given no body, because `magit-section' draws its
fold indicator for anything it could unfold and unfolding nothing reads
like a tree that has broken.

A module already on the path to the root is drawn as a leaf and said to
be repeated: its subtree is the one being drawn, and Lean\\='s import
graph does contain diamonds."
  (let* ((name (lean4-module--name module))
         (repeated (member name (cdr path)))
         (children (gethash path lean4-module--children :absent))
         (root (null (cdr path))))
    (magit-insert-section (lean4-module-section (list 'module path)
                                               (not root))
      (magit-insert-heading
       (lean4-module--heading module modifiers repeated))
      (cond
       (repeated)
       ((eq children :absent)
        (lean4-module--section-body
          (puthash path module lean4-module--pending)
          (insert (make-string (* 2 (1+ lean4-module--indent)) ?\s)
                  (propertize "...\n" 'font-lock-face 'shadow))))
       ;; Known to lead nowhere: a leaf, and no indicator promising more.
       ((null children))
       (t
        (lean4-module--section-body
          (let ((lean4-module--indent (1+ lean4-module--indent)))
            (dolist (import children)
              (let ((child (lean4-module--import-module import)))
                (lean4-module--insert-node
                 child
                 (lean4-module--modifiers import)
                 (cons (lean4-module--name child) path)))))))))))

(defun lean4-module--header-line ()
  "Return what the header line should say about the tree on display."
  (format "%s %s"
          (if (eq lean4-module--direction 'imported-by)
              "Modules importing"
            "Imports of")
          (or (lean4-module--name lean4-module--root) "?")))

(defun lean4-module--redisplay ()
  "Redraw the hierarchy buffer from what has been fetched so far."
  (when-let* ((buffer (get-buffer lean4-module-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (position (point))
            (lean4-module--indent 0))
        (erase-buffer)
        (setq header-line-format (lean4-module--header-line))
        (magit-insert-section (lean4-module-section 'root)
          (lean4-module--insert-node
           lean4-module--root nil (list (lean4-module--name lean4-module--root))))
        (goto-char (min position (point-max)))))))

;;;; The buffer

(defun lean4-module-module-at-point ()
  "Return the `LeanModule' the section at point is for, or nil."
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              ((consp value))
              ((eq (car value) 'module))
              (path (cadr value)))
    ;; The path names the node; the module itself is whatever the tree holds
    ;; for it, which for the root is the root and otherwise a child of the
    ;; entry above.
    (lean4-module--module-at-path path)))

(defun lean4-module--module-at-path (path)
  "Return the `LeanModule' PATH names, or nil."
  (if (null (cdr path))
      lean4-module--root
    (let ((siblings (gethash (cdr path) lean4-module--children)))
      (seq-some (lambda (import)
                  (let ((module (lean4-module--import-module import)))
                    (and (equal (lean4-module--name module) (car path))
                         module)))
                siblings))))

(defun lean4-module-visit ()
  "Visit the file of the module at point."
  (interactive)
  (let* ((module (or (lean4-module-module-at-point)
                     (user-error "No module at point")))
         (uri (or (lean4-module--uri module)
                  (user-error "Lean gave no file for %s"
                              (lean4-module--name module))))
         (file (lean4--uri-to-path uri)))
    (unless (file-exists-p file)
      (user-error "No such file: %s" file))
    (pop-to-buffer (find-file-noselect file))))

(defun lean4-module-refresh ()
  "Ask again for the whole tree, forgetting what was fetched before."
  (interactive)
  (clrhash lean4-module--children)
  (clrhash lean4-module--pending)
  (lean4-module--redisplay)
  (lean4-module--fetch-children (list (lean4-module--name lean4-module--root))
                                lean4-module--root))

(defvar-keymap lean4-module-mode-map
  :doc "Keymap for the *Lean Module Hierarchy* buffer."
  :parent magit-section-mode-map
  "RET" #'lean4-module-visit
  "g"   #'lean4-module-refresh
  "q"   #'quit-window)

(easy-menu-define lean4-module-mode-menu lean4-module-mode-map
  "Menu for the *Lean Module Hierarchy* buffer."
  '("Lean Module Hierarchy"
    ["Visit this module" lean4-module-visit t]
    ["Refresh" lean4-module-refresh t]
    "--"
    ["Quit" quit-window t]))

(define-derived-mode lean4-module-mode magit-section-mode "Lean Modules"
  "Major mode for the *Lean Module Hierarchy* buffer.

\\{lean4-module-mode-map}"
  :interactive nil
  :group 'lean4-module
  (setq-local revert-buffer-function (lambda (&rest _) (lean4-module-refresh)))
  ;; `magit-section' has no hook for its own opening commands, so the tree
  ;; notices afterwards which nodes now want asking about.
  (add-hook 'post-command-hook #'lean4-module--fetch-open nil 'local))

(defun lean4-module--show (root direction source)
  "Show ROOT\\='s hierarchy in DIRECTION, asking through SOURCE."
  (with-current-buffer (get-buffer-create lean4-module-buffer-name)
    (unless (derived-mode-p 'lean4-module-mode)
      (lean4-module-mode))
    (setq lean4-module--root root
          lean4-module--direction direction
          lean4-module--source source
          lean4-module--children (make-hash-table :test #'equal)
          lean4-module--pending (make-hash-table :test #'equal))
    (lean4-module--redisplay)
    (lean4-module--fetch-children (list (lean4-module--name root)) root)
    (display-buffer (current-buffer))))

(defun lean4-module--start (direction)
  "Show the hierarchy of the current buffer\\='s module, running DIRECTION."
  (unless (eglot-current-server)
    (user-error "No Lean server is running for this buffer"))
  (unless (lean4-module-supported-p)
    (user-error "This Lean server does not report a module hierarchy"))
  (let ((source (current-buffer)))
    (jsonrpc-async-request
     (eglot-current-server)
     :$/lean/prepareModuleHierarchy
     (list :textDocument (lean4-text-document-identifier))
     :success-fn
     (lambda (root)
       (if root
           (lean4-module--show root direction source)
         (message "%s is not a module Lean knows about"
                  (buffer-name source))))
     :error-fn (lambda (error)
                 (message "Lean could not say which module this is: %s"
                          (plist-get error :message)))
     :timeout-fn (lambda () (message "Lean did not say which module this is")))))

;;;###autoload
(defun lean4-module-hierarchy ()
  "Show what this file\\='s module imports, as a tree.
The counterpart of VS Code\\='s \"Show Module Hierarchy\"."
  (interactive)
  (lean4-module--start 'imports))

;;;###autoload
(defun lean4-module-hierarchy-inverse ()
  "Show what imports this file\\='s module, as a tree.
The counterpart of VS Code\\='s \"Show Inverse Module Hierarchy\"."
  (interactive)
  (lean4-module--start 'imported-by))

;;;###autoload
(defun lean4-module-copy-name ()
  "Put the name of this file\\='s module in the kill ring.
The counterpart of VS Code\\='s \"Copy Module Name\"."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Lean server is running for this buffer"))
  (jsonrpc-async-request
   (eglot-current-server)
   :$/lean/prepareModuleHierarchy
   (list :textDocument (lean4-text-document-identifier))
   :success-fn
   (lambda (module)
     (if-let* ((name (and module (lean4-module--name module))))
         (progn (kill-new name)
                (message "Copied %s" name))
       (message "Lean does not know this file as a module")))
   :error-fn (lambda (error)
               (message "Lean could not say which module this is: %s"
                        (plist-get error :message)))
   :timeout-fn #'ignore))

(provide 'lean4-module)
;;; lean4-module.el ends here
