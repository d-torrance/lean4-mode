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

(require 'compile)
(require 'dired)
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

;;;; Creating and downloading projects

(defconst lean4-lake-mathlib-toolchain
  "leanprover-community/mathlib4:lean-toolchain"
  "The toolchain a new Mathlib project is created with.
Not a version but a reference to Mathlib\\='s own \"lean-toolchain\", so
that the project starts on whatever Mathlib currently wants.")

(defconst lean4-lake-project-presets
  '(("Mathlib" . "https://github.com/leanprover-community/mathlib4")
    ("Mathematics in Lean"
     . "https://github.com/leanprover-community/mathematics_in_lean"))
  "Projects offered by name when downloading one.
The two VS Code offers: Lean\\='s mathematics library, and the book that
introduces it.")

(defun lean4-lake--run-in (directory command &optional toolchain then)
  "Run COMMAND in DIRECTORY through `compile'.

TOOLCHAIN, if given, is the Lean version to run under, passed as
`ELAN_TOOLCHAIN' so that elan installs it where it is missing -- which is
how VS Code creates a project on a toolchain that is not there yet.

THEN, if given, is called with no arguments once the command has
succeeded.  `compile' is asynchronous, so anything that should follow a
project being created has to wait for it like this."
  (let* ((default-directory (file-name-as-directory directory))
         (process-environment
          (if toolchain
              (cons (format "ELAN_TOOLCHAIN=%s" toolchain) process-environment)
            process-environment))
         (buffer (compile command)))
    (when then
      (with-current-buffer buffer
        (setq-local compilation-finish-functions
                    (list (lambda (_buffer status)
                            (when (string-prefix-p "finished" status)
                              (funcall then)))))))
    buffer))

(defun lean4-lake--read-new-directory (prompt)
  "Read the directory to make a project in, with PROMPT.
Refuses one that already has anything in it: `lake init' expects to be
the thing that fills it."
  (let ((directory (expand-file-name (read-directory-name prompt))))
    (when (and (file-exists-p directory)
               (not (file-directory-p directory)))
      (user-error "%s is not a directory" directory))
    (when (and (file-directory-p directory)
               (directory-files directory nil
                                directory-files-no-dot-files-regexp t))
      (user-error "%s is not empty" directory))
    directory))

(defun lean4-lake--initial-commit-command ()
  "Return the command that commits a newly created project.

Git is configured on the command line rather than relied on: a machine
with no `user.name' set would otherwise fail the commit and report the
whole creation as failed, when all that went wrong was the last step.  VS
Code sidesteps the same problem by committing as itself; this prefers
whatever identity is configured and only falls back."
  (let* ((name (string-trim (shell-command-to-string
                             "git config --get user.name 2>/dev/null")))
         (email (string-trim (shell-command-to-string
                              "git config --get user.email 2>/dev/null")))
         (identity (if (and (not (string-empty-p name))
                            (not (string-empty-p email)))
                       nil
                     (list "-c" "user.name=Lean 4 project"
                           "-c" "user.email=<>"))))
    (concat "git add --all && "
            (mapconcat #'shell-quote-argument
                       (append '("git") identity
                               '("commit" "-m" "Initial commit"))
                       " "))))

(defun lean4-lake--create-project (directory kind toolchain)
  "Create a Lean project of KIND in DIRECTORY, on TOOLCHAIN.

KIND is what `lake init' calls a template -- nil for a plain project,
\"math\" for one depending on Mathlib.  The steps are VS Code\\='s: `lake
init', then `lake update' to resolve what the template asked for, then
Mathlib\\='s cache where there is one to fetch, then `lake build', then an
initial commit.  Each waits on the one before, and the whole thing reports
into a single compilation buffer."
  (make-directory directory 'parents)
  (let* ((name (file-name-nondirectory (directory-file-name directory)))
         (steps (delq nil
                      (list (apply #'lean4-lake--command
                                   (append '("init") (list name)
                                           (when kind (list kind))))
                            (lean4-lake--command "update")
                            (when (equal kind "math")
                              (lean4-lake--command "exe" "cache" "get"))
                            (lean4-lake--command "build")
                            (lean4-lake--initial-commit-command)))))
    (lean4-lake--run-in directory (string-join steps " && ") toolchain
                        (lambda () (dired directory)))
    (message "Creating %s in %s..." name directory)))

;;;###autoload
(defun lean4-new-project (directory)
  "Create a new Lean project in DIRECTORY.

The counterpart of VS Code\\='s \"Create Standalone Project\": `lake init'
on the current stable Lean, then `lake update' and `lake build', then an
initial commit.  DIRECTORY has to be empty or absent, and its last
component becomes the package name, so it wants to be something Lean will
accept as a module name.

Dired opens on the project once it has been built."
  (interactive
   (list (lean4-lake--read-new-directory "Create a Lean project in: ")))
  (lean4-lake--create-project directory nil "leanprover/lean4:stable"))

;;;###autoload
(defun lean4-new-mathlib-project (directory)
  "Create a new Lean project in DIRECTORY that depends on Mathlib.

The counterpart of VS Code\\='s \"Create Project Using Mathlib\": as
`lean4-new-project', but with `lake init'\\='s \"math\" template, on
Mathlib\\='s own toolchain, and fetching Mathlib\\='s build cache before
building -- without which the first build compiles Mathlib from source."
  (interactive
   (list (lean4-lake--read-new-directory
          "Create a Mathlib project in: ")))
  (lean4-lake--create-project directory "math" lean4-lake-mathlib-toolchain))

;;;###autoload
(defun lean4-download-project (url directory)
  "Clone the Lean project at URL into DIRECTORY and build it.

The counterpart of VS Code\\='s \"Download Project\", and it offers the same
two by name -- Mathlib and Mathematics in Lean -- as well as taking any
Git URL.  After cloning it resolves dependencies, fetches Mathlib\\='s cache
where there is one, and builds, which is what `lean4-lake-build' does.

Dired opens on the project once it has been built."
  (interactive
   (let* ((choice (completing-read
                   "Download project (name or Git URL): "
                   (mapcar #'car lean4-lake-project-presets)))
          (url (or (cdr (assoc choice lean4-lake-project-presets))
                   choice)))
     (when (string-empty-p (string-trim url))
       (user-error "No project named"))
     (list url (lean4-lake--read-new-directory "Clone it into: "))))
  (make-directory directory 'parents)
  (let ((command (concat (mapconcat #'shell-quote-argument
                                    (list "git" "clone" url directory) " ")
                         " && " (lean4-lake--build-command))))
    (lean4-lake--run-in directory command nil (lambda () (dired directory)))
    (message "Downloading %s into %s..." url directory)))

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
