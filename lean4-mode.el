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

(require 'cl-lib)
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

;; Declare symbols defined in external dependencies.  This silences
;; byte-compiler warnings:
(defvar compilation-mode-font-lock-keywords)
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
    ;; restore old value
    (setq compile-command cc)
    (setq default-directory dd)))

(defun lean4-std-exe ()
  "Execute Lean in the current buffer."
  (interactive)
  (lean4-execute))

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

(define-abbrev-table 'lean4-abbrev-table
  '())

(defvar-keymap lean4-mode-map
  :doc "Keymap used in Lean 4 mode."
  "C-c C-x"     #'lean4-std-exe
  "C-c C-l"     #'lean4-std-exe
  "C-c C-k"     #'quail-show-key
  "C-c C-i"     #'lean4-toggle-info
  "C-c C-p C-l" #'lean4-lake-build
  "C-c C-d"     #'lean4-refresh-file-dependencies
  "C-c C-r"     #'eglot-reconnect
  "C-c C-s"     #'lean4-info-toggle-pin
  "C-c C-SPC"   #'lean4-info-toggle-pause
  "TAB"         #'lean4-tab-indent)

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  `("Lean 4"
    ["Execute lean"         lean4-execute                     t]
    ["Toggle info display"  lean4-toggle-info                 t]
    ["Pin goal display"     lean4-info-toggle-pin             t]
    ["Pause goal display"   lean4-info-toggle-pause           t]
    ["List of errors"       flymake-show-buffer-diagnostics   flymake-mode]
    ["Restart lean process" eglot-reconnect                   t]
    ["Customize lean4-mode" (customize-group 'lean4)          t]))

(defconst lean4-hooks-alist
  '((before-save-hook . lean4-whitespace-cleanup)
    ;; Update the goal display as point moves.  Diagnostics arrive
    ;; separately and are handled in `lean4-eglot'.
    (post-command-hook . lean4-info-buffer-redisplay-debounced))
  "Hooks which lean4-mode needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable.")

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean language.

\\{lean4-mode-map}"
  :syntax-table lean4-syntax-table
  :abbrev-table lean4-abbrev-table
  :group 'lean4
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults lean4-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-indent-function 'common-lisp-indent-function)
  (setq-local indent-line-function #'lean4-indent-line-function)
  (set 'compilation-mode-font-lock-keywords '())
  (require 'lean4-input)
  (set-input-method "Lean")
  ;; Offered alongside whatever Eglot contributes; the user's own
  ;; completion frontend decides how to present them.
  (add-hook 'completion-at-point-functions
            #'lean4-input-completion-at-point nil 'local)
  (electric-indent-local-mode -1)
  (pcase-dolist (`(,hook . ,fn) lean4-hooks-alist)
    (add-hook hook fn nil 'local))
  (add-hook 'eglot-managed-mode-hook #'lean4--setup-semantic-tokens nil 'local)
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
