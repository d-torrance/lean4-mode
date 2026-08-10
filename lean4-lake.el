;;; lean4-lake.el --- Lean4-Mode Lake Integration  -*- lexical-binding: t; -*-

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

;; This library provides integration with Lake, Lean 4 build system and package
;; manager.

;;; Code:

(require 'lean4-util)
(require 'lean4-settings)

(defun lean4-root-dir-p (dir)
  "Check if directory DIR contains \"lakefile.lean\" or \"lakefile.toml\"."
  (or
   (file-exists-p (expand-file-name "lakefile.lean" dir))
   (file-exists-p (expand-file-name "lakefile.toml" dir))))

(defun lean4-lake-find-dir ()
  "Find a parent directory of the current file with a Lake file.

  It looks for files named \"lakefile.lean\" or \"lakefile.toml\" file."
  (and (buffer-file-name)
       (locate-dominating-file (buffer-file-name) #'lean4-root-dir-p)))

(defun lean4-lake-find-dir-safe ()
  "Call `lean4-lake-find-dir', error on failure."
  (or (lean4-lake-find-dir)
      (error "Cannot find lakefile in any directory above %s" (buffer-file-name))))

(defun lean4-lake--run (&rest arguments)
  "Run Lake with ARGUMENTS in the enclosing package, through `compile'.
The output lands in a compilation buffer, so Lake's own errors are
navigated the way every other build's are."
  (let ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (compile (mapconcat #'shell-quote-argument
                        (cons (lean4--program lean4-lake-name) arguments)
                        " "))))

(defun lean4-lake-build ()
  "Call lake build."
  (interactive)
  (lean4-lake--run "build"))

;;;; Mathlib's build cache

;; `lake exe cache get' is Mathlib's own executable rather than anything Lake
;; provides, so it exists only in a project that depends on Mathlib.  VS Code
;; probes for it first in order to say "This command cannot be used in
;; non-Mathlib projects" in a notification; here the attempt itself says so, in
;; the compilation buffer, which is where the answer would have to be looked
;; for anyway.

(defun lean4-lake--relative-name (file root)
  "Return FILE relative to ROOT, or nil if it does not lie under ROOT."
  (and (file-in-directory-p file root)
       (file-relative-name file root)))

(defun lean4-lake--open-files (root)
  "Return the Lean files being visited under ROOT, relative to it.
`lake exe cache get' takes paths relative to the package."
  (delq nil
        (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'lean4-mode)
                         buffer-file-name
                         (lean4-lake--relative-name buffer-file-name root))))
                (buffer-list))))

(defun lean4-lake-fetch-cache ()
  "Fetch Mathlib's build cache for the whole package.
Downloads what has already been built upstream instead of building it
here, which is the difference between minutes and hours on Mathlib.  The
counterpart of VS Code's \"Fetch Mathlib Build Cache\"."
  (interactive)
  (lean4-lake--run "exe" "cache" "get"))

(defun lean4-lake-fetch-file-cache ()
  "Fetch Mathlib's build cache for this file and what it imports.
Quicker than `lean4-lake-fetch-cache' when only one file is being worked
on.  The counterpart of VS Code's \"Fetch Mathlib Build Cache For
Current File\"."
  (interactive)
  ;; The file is checked for first: without one there is no directory to look
  ;; for a lakefile above, and "cannot find lakefile above nil" is not the
  ;; problem worth reporting.
  (let* ((file (or buffer-file-name
                   (user-error "This buffer is not visiting a file")))
         (root (lean4-lake-find-dir-safe))
         (relative (or (lean4-lake--relative-name file root)
                       (user-error "%s is not inside the package at %s"
                                   file root))))
    (lean4-lake--run "exe" "cache" "get" relative)))

(defun lean4-lake-fetch-open-file-caches ()
  "Fetch Mathlib's build cache for every open Lean file in the package.
The counterpart of VS Code's \"Fetch Mathlib Build Cache For All Open
Files\"."
  (interactive)
  (let* ((root (lean4-lake-find-dir-safe))
         (files (or (lean4-lake--open-files root)
                    (user-error "No Lean file in %s is open" root))))
    (apply #'lean4-lake--run "exe" "cache" "get" files)))

(provide 'lean4-lake)
;;; lean4-lake.el ends here
