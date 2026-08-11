;;; lean4-mode.el --- Major mode for Lean language  -*- lexical-binding: t; -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Copyright (c) 2014, 2015 Soonho Kong. All rights reserved.
;; Copyright (C) 2026 Doug Torrance

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Doug Torrance <dtorrance9@gatech.edu>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (eglot "1.12") (eri "1.0") (magit-section "3.3.0"))
;; URL: https://github.com/d-torrance/lean4-mode
;; SPDX-License-Identifier: Apache-2.0
;; Version: 2.0.0

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

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;; See the README.org for more advanced features and the
;; associated keybindings.
;;
;; This continues the lean4-mode developed at
;; <https://github.com/leanprover-community/lean4-mode>, begun by
;; Leonardo de Moura, Soonho Kong, Gabriel Ebner and Sebastian Ullrich,
;; and maintained by Yury G. Kudryashov and latterly Mekeor Melire.  The
;; move from lsp-mode to Eglot draws on the forks by Buster Copley and
;; Paul D. Nelson; lean4-rpc.el and lean4-render.el are derived from Jan
;; Mas Rovira's neo-lean-mode.  See NOTICE.

;;; Code:

(require 'lisp-mnt)
(require 'pcase)

(require 'eri)
(require 'lean4-indent)
(require 'lean4-util)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-eglot)
(require 'lean4-diagnostics)
(require 'lean4-info)
(require 'lean4-fringe)
(require 'lean4-lake)
(require 'lean4-module)
(require 'lean4-loogle)
(require 'lean4-toolchain)

;; Declare symbols defined in external dependencies.  This silences
;; byte-compiler warnings:
(defvar markdown-code-lang-modes)
(declare-function flymake-proc-init-create-temp-buffer-copy "flymake-proc")
(declare-function quail-show-key "quail")
;; lean4-input is loaded lazily from the mode body: building the Quail
;; package parses the whole abbreviation table, which is not worth doing
;; until a Lean buffer actually exists.
(declare-function lean4-input-completion-at-point "lean4-input")

(defun lean4-compile-string (lake-name exe-name args file-name)
  "Command to run EXE-NAME with extra ARGS and FILE-NAME.
If LAKE-NAME is nonempty, then prepend \"LAKE-NAME env\" to the command
\"EXE-NAME ARGS FILE-NAME\"."
  (if lake-name
      (format "%s env %s %s %s" lake-name exe-name args file-name)
    (format "%s %s %s" exe-name args file-name)))

(defun lean4-create-temp-in-system-tempdir (file-name prefix)
  "Create a temp lean file and return its name.
The new file has prefix PREFIX (defaults to `flymake') and the same extension as
FILE-NAME."
  (make-temp-file (or prefix "flymake") nil (file-name-extension file-name)))

(defun lean4-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  (when (called-interactively-p 'any)
    (setq arg (read-string "arg: " arg)))
  (let* ((cc compile-command)
         (dd default-directory)
         (use-lake (lean4-lake-find-dir))
         (default-directory (if use-lake (lean4-lake-find-dir) dd))
         (target-file-name
          (or
           (buffer-file-name)
           (flymake-proc-init-create-temp-buffer-copy
            'lean4-create-temp-in-system-tempdir))))
    (compile (lean4-compile-string
              (when use-lake
                (shell-quote-argument (lean4--program lean4-lake-name)))
              (shell-quote-argument (lean4--program lean4-executable-name))
              (or arg "")
              (shell-quote-argument (expand-file-name target-file-name))))
    ;; `compile' assigns this; `default-directory' needs no restoring, being
    ;; let-bound above.
    (setq compile-command cc)))

;;;###autoload
(define-obsolete-function-alias 'lean4-std-exe #'lean4-execute "2.0.0")

(defcustom lean4-indent-function #'lean4-indent-line
  "How TAB indents a line in a Lean buffer.

`lean4-indent-line' guesses from the layout and cycles through the
alternatives when pressed again.  `eri-indent' does not guess at all: it
cycles through the columns of the lines above, which some people prefer
and which is what this mode did before."
  :group 'lean4
  :type '(choice (const :tag "Guess, then cycle" lean4-indent-line)
                 (const :tag "Cycle only" eri-indent)
                 function))

(defun lean4-tab-indent ()
  "Indent the current line, or complete, depending on where point is."
  (interactive)
  (cond ((looking-back (rx line-start (* white)) nil)
         (call-interactively lean4-indent-function))
        (t (indent-for-tab-command))))

(defvar-keymap lean4-mode-map
  :doc "Keymap used in Lean 4 mode."
  "C-c C-x"     #'lean4-execute
  "C-c C-l"     #'lean4-execute
  "C-c C-k"     #'quail-show-key
  "C-c C-i"     #'lean4-toggle-info
  "C-c C-p C-l" #'lean4-lake-build
  "C-c C-p C-c" #'lean4-lake-fetch-cache
  "C-c C-p C-f" #'lean4-lake-fetch-file-cache
  "C-c C-d"     #'lean4-refresh-file-dependencies
  "C-c C-r"     #'eglot-reconnect
  "C-c C-s"     #'lean4-info-toggle-pin
  "C-c C-SPC"   #'lean4-info-toggle-pause
  "C-c C-o"     #'lean4-info-toggle-message-order
  "C-c C-a"     #'lean4-info-toggle-all-messages-pause
  "C-c C-g"     #'lean4-info-refresh-paused
  "TAB"         #'lean4-tab-indent)

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  `("Lean 4"
    ["Execute lean"         lean4-execute                     t]
    ["Toggle info display"  lean4-toggle-info                 t]
    "--"
    ;; The goal display's controls, shared with `lean4-info-mode-menu'.
    ,@lean4-info-menu-items
    ,lean4-info-display-menu
    "--"
    ["List of errors"       flymake-show-buffer-diagnostics   flymake-mode]
    ;; Labelled as Lean's own message asks for it: the server says to use
    ;; the "Restart File" command when a file's imports are stale.
    ["Restart File"         lean4-refresh-file-dependencies    t]
    ["Restart lean process" eglot-reconnect                   t]
    "--"
    ("Project"
     ["New project..."                 lean4-new-project             t]
     ["New Mathlib project..."         lean4-new-mathlib-project     t]
     ["Download a project..."          lean4-download-project        t]
     "--"
     ["Build with Lake"                lean4-lake-build              t]
     ["Clean build artefacts"          lean4-lake-clean              t]
     ["Update a dependency..."         lean4-lake-update-dependency  t]
     "--"
     ;; Mathlib's own executable, so these are absent in a project that does
     ;; not depend on it; whether it is there is Lake's answer to give.
     ["Fetch Mathlib cache"            lean4-lake-fetch-cache        t]
     ["Fetch cache for this file"      lean4-lake-fetch-file-cache
      buffer-file-name]
     ["Fetch cache for open files"     lean4-lake-fetch-open-file-caches t])
    ["Search Mathlib with Loogle..." lean4-loogle-search t]
    ("Unicode input"
     ["Insert a symbol..."          lean4-input-insert-symbol      t]
     ["Copy a symbol..."            lean4-input-copy-symbol        t]
     ["Look a symbol up..."         lean4-input-find-symbol        t]
     "--"
     ["Key for the symbol at point" quail-show-key                 t]
     ["All abbreviations..."        lean4-input-show-translations  t])
    ("Lean version"
     ["Set this project's Lean version..." lean4-select-project-toolchain t]
     ["Set the default Lean version..."    lean4-select-default-toolchain t]
     "--"
     ["Update a release channel..."        lean4-update-release-channel   t]
      ["Install a Lean version..."         lean4-install-toolchain        t]
     ["Remove Lean versions..."            lean4-uninstall-toolchains     t]
     "--"
     ["Update elan"                        lean4-update-elan              t]
     ["Install elan..."                    lean4-install-elan             t]
     ["Install Git and curl..."            lean4-install-dependencies     t]
     ["Remove elan..."                     lean4-uninstall-elan           t])
    ("Module hierarchy"
     ["What this file imports"    lean4-module-hierarchy         t]
     ["What imports this file"    lean4-module-hierarchy-inverse t]
     "--"
     ["Copy this module's name"   lean4-module-copy-name         t])
    "--"
    ("Help"
     ["Lean4-Mode manual"     lean4-show-manual                t]
     ["Troubleshooting"       lean4-show-troubleshooting-guide t]
     "--"
     ["Lean's setup guide"    lean4-show-setup-guide           t]
     ["Lean's documentation"  lean4-show-documentation         t]
     "--"
     ["Report on this setup"  lean4-diagnose                   t])
    ["Customize lean4-mode" (customize-group 'lean4)          t]))

;;;; Documentation

;; VS Code shows its own manual and setup guide in editor tabs.  The
;; counterparts of the first two are this package's own manual, which is
;; shipped as Info; the other two are pages on lean-lang.org, and VS Code opens
;; the same ones.

(defun lean4--show-manual (&optional node)
  "Show this package\='s Info manual, at NODE if given.

Prefers the \"lean4-mode.info\" shipped beside the source, since a
package installed from a checkout is not in `Info-directory-list' and
would otherwise not be found at all."
  (require 'info)
  (let* ((library (locate-library "lean4-mode"))
         (directory (and library (file-name-directory library)))
         (file (and directory (expand-file-name "lean4-mode.info" directory))))
    (info (format "(%s)%s"
                  (if (and file (file-readable-p file)) file "lean4-mode")
                  (or node "")))))

;;;###autoload
(defun lean4-show-manual ()
  "Show this package\='s manual.
The counterpart of VS Code\='s \"Show Manual\", which shows its own."
  (interactive)
  (lean4--show-manual))

;;;###autoload
(defun lean4-show-troubleshooting-guide ()
  "Show what to do when something is not working.
The counterpart of VS Code\='s \"Show Troubleshooting Guide\"."
  (interactive)
  (lean4--show-manual "Troubleshooting"))

;;;###autoload
(defun lean4-show-setup-guide ()
  "Open Lean\='s own instructions for installing it.
The counterpart of VS Code\='s \"Show Setup Guide\"."
  (interactive)
  (browse-url "https://lean-lang.org/lean4/doc/setup.html"))

;;;###autoload
(defun lean4-show-documentation ()
  "Open Lean\='s documentation.
The page VS Code\='s \"Show Documentation Resources\" opens."
  (interactive)
  (browse-url "https://lean-lang.org/learn/"))

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean language.

\\{lean4-mode-map}"
  :syntax-table lean4-mode-syntax-table
  :group 'lean4
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults lean4-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'lean4-indent-line-function)
  (require 'lean4-input)
  ;; `activate-input-method' rather than `set-input-method': the latter is
  ;; the interactive command, and it also assigns `default-input-method',
  ;; which is global.  Opening one Lean file would otherwise leave `C-\'
  ;; toggling the Lean input method in every other buffer.
  (activate-input-method "Lean")
  ;; Offered alongside whatever Eglot contributes; the user's own
  ;; completion frontend decides how to present them.
  (add-hook 'completion-at-point-functions
            #'lean4-input-completion-at-point nil 'local)
  ;; Appended, so it is consulted only where the server offered nothing.
  (add-hook 'completion-at-point-functions
            #'lean4-keyword-completion-at-point 'append 'local)
  ;; Not `electric-indent-local-mode': switching off a minor mode the
  ;; reader turned on is not a major mode's business.  This is the
  ;; variable Emacs provides for a mode to say that reindenting on a
  ;; newline does not suit its language.
  (setq-local electric-indent-inhibit t)
  (add-hook 'before-save-hook #'lean4-whitespace-cleanup nil 'local)
  (add-hook 'eglot-managed-mode-hook #'lean4--setup-semantic-tokens nil 'local)
  (add-hook 'eglot-managed-mode-hook #'lean4--setup-completion nil 'local)
  ;; Before `eglot-ensure' below, which is what asks project.el where
  ;; this file's root is.
  (lean4-register-project-backend)
  (lean4-info--maybe-auto-open)
  ;; Deliberately not conditional on finding a workspace root.  A file
  ;; outside any Lake package is still worth serving -- `lean4--server-command'
  ;; falls back to `lean --server' with elan's default toolchain for exactly
  ;; this case, and VS Code serves such files too.  Requiring a root meant a
  ;; scratch .lean file got no server at all.
  (when (and lean4-auto-start-server buffer-file-name)
    (eglot-ensure)))

(defun lean4--version ()
  "Return Lean version as a list `(MAJOR MINOR PATCH)'."
  (with-temp-buffer
    (call-process (lean4--program lean4-executable-name)
                  nil (list t nil) nil "-v")
    (goto-char (point-min))
    (re-search-forward
     (rx bol "Lean (version " (group (+ digit) (+ "." (+ digit)))))
    (version-to-list (match-string 1))))

(defun lean4--library-version (library)
  "Return the version of LIBRARY as a string, or a note saying it is absent.
Prefers what package.el knows, since a built-in library carries no
Version header worth reading."
  (cond
   ((not (locate-library (symbol-name library))) "NOT INSTALLED")
   ((when-let* ((description (cadr (assq library package-alist))))
      (package-version-join (package-desc-version description))))
   ((ignore-errors
      (lm-with-file (replace-regexp-in-string "\\.elc\\'" ".el"
                                              (locate-library
                                               (symbol-name library)))
        (lm-header "version"))))
   (t "built in")))

(defun lean4--program-version (program)
  "Return the first line PROGRAM prints for --version, or why it did not."
  (let ((executable (lean4--program program)))
    (if (not (or (file-name-absolute-p executable)
                 (executable-find executable)))
        (format "%s: not found on `exec-path'" executable)
      (with-temp-buffer
        (if (zerop (ignore-errors
                     (call-process executable nil t nil "--version")))
            (car (split-string (buffer-string) "\n"))
          (format "%s: could not be run" executable))))))

(defun lean4--diagnose-buffer ()
  "Return the Lean buffer to report on: this one, or the likeliest other."
  (if (derived-mode-p 'lean4-mode)
      (current-buffer)
    (seq-find (lambda (buffer)
                (with-current-buffer buffer (derived-mode-p 'lean4-mode)))
              (buffer-list))))

;;;###autoload
(defun lean4-diagnose ()
  "Report how Lean4-Mode is set up, for troubleshooting.

Reports on a Lean buffer even when run from elsewhere, since the answers
that matter -- whether a server is running, and which -- are buffer
local, and the buffer one happens to be in when the question arises is
rarely the Lean one."
  (interactive)
  (let ((source (lean4--diagnose-buffer)))
    (with-current-buffer (get-buffer-create "*lean4-diagnose*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Lean4-Mode setup\n"
                "================\n\n")
        (insert (format "Emacs           %s\n" emacs-version))
        (dolist (library '(eglot eri magit-section jsonrpc))
          (insert (format "%-15s %s\n" library
                          (lean4--library-version library))))
        (insert (format "%-15s %s\n" "lean4-mode"
                        (lean4--library-version 'lean4-mode)))
        (insert "\n")
        (insert (format "%-15s %s\n" "lean"
                        (lean4--program-version lean4-executable-name)))
        (insert (format "%-15s %s\n" "lake"
                        (lean4--program-version lean4-lake-name)))
        (insert "\n")
        (if (not source)
            (insert "No Lean buffer is open, so there is nothing more to say.\n")
          ;; Everything below is read in the Lean buffer and written here.
          ;; Inserting while that buffer is current would write the report
          ;; into the user's source file.
          (let (facts diagnostics)
            (with-current-buffer source
              (let ((server (eglot-current-server)))
                (setq facts
                      (list (cons "Buffer" (buffer-name))
                            (cons "Workspace"
                                  (or (lean4--workspace-root) "none found"))
                            (cons "Toolchain"
                                  (or (lean4--toolchain-string)
                                      "no lean-toolchain"))
                            (cons "Server command"
                                  (format "%S" (lean4--server-command
                                                (lean4--workspace-root))))
                            (cons "Server"
                                  (if server
                                      (format "%s" (eieio-object-class server))
                                    "NOT RUNNING"))
                            (cons "Lean extensions"
                                  (if server
                                      (format "%S"
                                              (thread-first
                                                (eglot--capabilities server)
                                                (plist-get :experimental)))
                                    "n/a"))
                            (cons "Silent filter"
                                  (if (lean4--silent-filter-installed-p)
                                      "installed" "MISSING"))))
                (setq diagnostics
                      (mapcar (lambda (diagnostic)
                                (let ((raw (lean4-diagnostic-lsp-data
                                            diagnostic)))
                                  (format "%-14s silent=%-5s tags=%-6s %s"
                                          (flymake-diagnostic-type diagnostic)
                                          (plist-get raw :isSilent)
                                          (plist-get raw :leanTags)
                                          (car (split-string
                                                (lean4-diagnostic-message
                                                 diagnostic)
                                                "\n")))))
                              (flymake-diagnostics)))))
            (pcase-dolist (`(,label . ,value) facts)
              (insert (format "%-15s %s\n" label value)))
            (insert "\nDiagnostics\n-----------\n")
            (if (null diagnostics)
                (insert "none\n")
              (dolist (line diagnostics) (insert line "\n")))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun lean4--toolchain-string ()
  "Return the contents of the workspace's lean-toolchain file, or nil."
  (when-let* ((root (lean4--workspace-root))
              (file (expand-file-name "lean-toolchain" root))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))

(defun lean4--silent-filter-installed-p ()
  "Return non-nil if the silent-diagnostic filter is in place.
Tested by whether its library is loaded rather than by looking the
method up: `cl-find-method' wants specializer objects and is easy to
call wrongly, and the library defines the method unconditionally."
  (featurep 'lean4-diagnostics))

(defun lean4-show-version ()
  "Print Lean 4 version."
  (interactive)
  (message "Lean %s" (mapconcat #'number-to-string (lean4--version) ".")))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))

;;;###autoload
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("lean" . lean4-mode)))

;; Use utf-8 encoding
;;;###autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(provide 'lean4-mode)
;;; lean4-mode.el ends here
