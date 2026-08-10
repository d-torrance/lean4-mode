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

(defun lean4-lake--command (&rest arguments)
  "Return the shell command that runs Lake with ARGUMENTS."
  (mapconcat #'shell-quote-argument
             (cons (lean4--program lean4-lake-name) arguments)
             " "))

(defun lean4-lake--run-shell (command)
  "Run COMMAND in the enclosing package, through `compile'.
The output lands in a compilation buffer, so Lake\\='s own errors are
navigated the way every other build\\='s are."
  (let ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (compile command)))

(defun lean4-lake--run (&rest arguments)
  "Run Lake with ARGUMENTS in the enclosing package, through `compile'."
  (lean4-lake--run-shell (apply #'lean4-lake--command arguments)))

(defun lean4-lake--build-command ()
  "Return the command VS Code\\='s \"Build Project\" amounts to.

Three steps, each conditional on the one before, which is what makes this
different from a bare `lake build':

  `lake resolve-deps' clones a dependency the manifest names but the
  checkout has not got.  `lake build' would fail on a missing one, and
  less clearly.

  `lake exe cache get' downloads Mathlib\\='s build artefacts, so that they
  are not built here.  This is the step that matters: without it, building
  a project that depends on Mathlib means hours rather than minutes.

  `lake build' builds what is left.

The cache step is Mathlib\\='s own executable and does not exist elsewhere,
so it is guarded by asking Lake for it first -- exactly what VS Code does,
and for the same reason: a project without the executable should build
anyway, while a cache download that *fails* should stop the build rather
than leave it to do the work locally.  The guard\\='s own output is
discarded, being the executable\\='s help text; VS Code filters it too.

Joined with `&&' rather than run one at a time so that all three report
into a single compilation buffer, where `next-error' walks whatever any of
them said.  A POSIX shell is assumed, as `compile' assumes one anyway."
  (format "%s && { if %s >/dev/null 2>&1; then %s; fi; } && %s"
          (lean4-lake--command "resolve-deps")
          (lean4-lake--command "exe" "cache")
          (lean4-lake--command "exe" "cache" "get")
          (lean4-lake--command "build")))

(defun lean4-lake-build ()
  "Build the package, as VS Code\\='s \"Build Project\" does.

Resolves missing dependencies, fetches Mathlib\\='s build cache where there
is one to fetch, and then builds; see `lean4-lake--build-command' for why
each step is there.  For a bare `lake build', with no network and no
dependency resolution, use \\[compile]."
  (interactive)
  (lean4-lake--run-shell (lean4-lake--build-command)))

(defun lean4-lake-clean ()
  "Delete the package\\='s build artefacts, with `lake clean'.

Asks first, as VS Code does: on a project the size of Mathlib this
discards hours of building, though `lean4-lake-fetch-cache' can get most
of it back.  `lean4-lake-build' rebuilds."
  (interactive)
  (let ((root (lean4-lake-find-dir-safe)))
    (unless (yes-or-no-p (format "Delete all build artefacts in %s? " root))
      (user-error "Not cleaning %s" root))
    (lean4-lake--run "clean")))

;;;; Dependencies

;; `lake-manifest.json' is where Lake records what it resolved each dependency
;; to.  Two shapes are in the wild: up to manifest version 6 each package is
;; `{"git": {"name": ...}}', and from 7 on -- which is what any current Lake
;; writes -- it is `{"type": "git", "name": ...}'.  Both are read here, since
;; a checkout old enough to have the first is exactly the sort this would
;; otherwise fail on silently.

(defun lean4-lake--manifest-packages (root)
  "Return the packages named in ROOT\\='s Lake manifest, as plists.
Nil when there is no manifest, or it cannot be parsed: a manifest is
Lake\\='s to write, and a client that errors on one it does not recognise
is a client that breaks on the next format."
  (let ((file (expand-file-name "lake-manifest.json" root)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (plist-get (json-parse-buffer :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                     :packages))))))

(defun lean4-lake--direct-dependencies (root)
  "Return the names of ROOT\\='s direct Git dependencies.

Inherited ones are left out -- they are a dependency\\='s dependencies, and
updating one from here is not what `lake update' means -- as are path
dependencies, which have no revision to move."
  (delq nil
        (mapcar (lambda (package)
                  (let ((git (plist-get package :git)))
                    (when (or git (equal (plist-get package :type) "git"))
                      (let ((fields (or git package)))
                        (unless (plist-get fields :inherited)
                          (plist-get fields :name))))))
                (lean4-lake--manifest-packages root))))

(defun lean4-lake-update-dependency (name)
  "Update the dependency NAME to its most recent version, with `lake update'.

Read from `lake-manifest.json', which is what Lake resolved the
package\\='s dependencies to.

Asks first, and means it.  VS Code words the warning this way: the
command is intended for maintainers of the project, and if the updated
version is incompatible with another dependency or with the code here,
the project may not build any more.  It also rewrites the manifest, which
is under version control in most projects.

Does not touch \"lean-toolchain\".  VS Code offers to match it to the
updated dependency\\='s; that is a second decision about a file Lake and
elan both read, and is better made deliberately."
  (interactive
   (let* ((root (lean4-lake-find-dir-safe))
          (names (lean4-lake--direct-dependencies root)))
     (unless names
       (user-error "No Git dependencies recorded in %slake-manifest.json"
                   (file-name-as-directory root)))
     (list (completing-read "Update dependency: " names nil t))))
  (unless (yes-or-no-p
           (format "Update %s to its latest version, and rewrite the manifest? "
                   name))
    (user-error "Not updating %s" name))
  (lean4-lake--run "update" name))

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
