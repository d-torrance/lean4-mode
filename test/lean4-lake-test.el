;;; lean4-lake-test.el --- Tests for Lake project detection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for locating the enclosing Lake package.
;;
;; These build a throwaway directory tree rather than checking fixtures into
;; the repository, because the interesting cases are about *nesting*: a Lake
;; package vendored inside another one, which is exactly the shape
;; `.lake/packages/' produces in any project that depends on Mathlib.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-lake)

(defmacro lean4-lake-test--with-tree (spec &rest body)
  "Create a temporary directory tree described by SPEC and evaluate BODY.
SPEC is a list of relative file names; each is created empty, along with
any directories it needs.  Inside BODY, `root' is bound to the tree's
root directory, with a trailing slash.  The tree is deleted afterwards."
  (declare (indent 1) (debug (form body)))
  `(let ((root (file-name-as-directory
                (make-temp-file "lean4-lake-test" 'directory))))
     (unwind-protect
         (progn
           (dolist (relative ,spec)
             (let ((file (expand-file-name relative root)))
               (make-directory (file-name-directory file) 'parents)
               (write-region "" nil file nil 'silent)))
           ,@body)
       (delete-directory root 'recursive))))

(defun lean4-lake-test--find-dir-from (file)
  "Return the Lake directory `lean4-lake-find-dir' finds for FILE.
Only `buffer-file-name' is consulted, so set that directly rather than
visiting FILE: `find-file-noselect' on a .lean file would activate
`lean4-mode', which shells out to `lean -v' and starts a language
server."
  (with-temp-buffer
    (setq buffer-file-name file)
    (unwind-protect
        (lean4-lake-find-dir)
      ;; A temp buffer with `buffer-file-name' set would otherwise prompt
      ;; about unsaved changes when killed.
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))))

(ert-deftest lean4-lake-root-dir-p-recognises-both-lakefiles ()
  "A directory with either flavour of lakefile is a Lake root."
  (lean4-lake-test--with-tree '("lakefile.lean")
    (should (lean4-root-dir-p root)))
  (lean4-lake-test--with-tree '("lakefile.toml")
    (should (lean4-root-dir-p root)))
  (lean4-lake-test--with-tree '("README.md")
    (should-not (lean4-root-dir-p root))))

(ert-deftest lean4-lake-find-dir-walks-up ()
  "The Lake directory is found from a file nested below it."
  (lean4-lake-test--with-tree '("lakefile.toml" "Pkg/Deep/Mod.lean")
    (should (equal (file-truename (lean4-lake-test--find-dir-from
                                   (expand-file-name "Pkg/Deep/Mod.lean" root)))
                   (file-truename root)))))

(ert-deftest lean4-lake-find-dir-returns-nil-without-lakefile ()
  "A file outside any Lake package yields nil rather than an error."
  (lean4-lake-test--with-tree '("Loose.lean")
    (should-not (lean4-lake-test--find-dir-from
                 (expand-file-name "Loose.lean" root)))))

(ert-deftest lean4-lake-find-dir-stops-at-innermost-package ()
  "A vendored dependency resolves to its own lakefile, not the outer one.
`lean4-lake-find-dir' is used to pick the directory `lake build' runs
in, so the innermost package is the right answer here.  Note this is the
opposite of how the *language server* root is chosen, which deliberately
walks to the outermost `lean-toolchain'."
  (lean4-lake-test--with-tree '("lakefile.toml"
                                ".lake/packages/dep/lakefile.toml"
                                ".lake/packages/dep/Dep.lean")
    (should (equal (file-truename
                    (lean4-lake-test--find-dir-from
                     (expand-file-name ".lake/packages/dep/Dep.lean" root)))
                   (file-truename
                    (expand-file-name ".lake/packages/dep/" root))))))

(ert-deftest lean4-lake-find-dir-safe-errors-without-lakefile ()
  "`lean4-lake-find-dir-safe' reports a useful error rather than nil."
  (lean4-lake-test--with-tree '("Loose.lean")
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "Loose.lean" root))
      (unwind-protect
          (should-error (lean4-lake-find-dir-safe) :type 'error)
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))))

;;;; Running Lake

(defmacro lean4-lake-test--capture (&rest body)
  "Evaluate BODY and return the command `compile' was called with.
Also returns the directory it was called from, as a cons cell; nil if
`compile' was never reached."
  (declare (indent 0) (debug (body)))
  `(let (captured)
     (cl-letf (((symbol-function 'compile)
                (lambda (command &rest _)
                  (setq captured (cons command default-directory)))))
       ,@body)
     captured))

(defmacro lean4-lake-test--in-file (relative spec &rest body)
  "Visit RELATIVE in a tree built from SPEC and evaluate BODY.
The buffer is a temporary one with `buffer-file-name' set and
`lean4-mode' claimed as the major mode, rather than a really visited
file: `find-file-noselect' on a .lean file would start a language
server.  Inside BODY, `root' is the tree's root."
  (declare (indent 2) (debug (form form body)))
  `(lean4-lake-test--with-tree ,spec
     (with-temp-buffer
       (setq buffer-file-name (expand-file-name ,relative root))
       (setq major-mode 'lean4-mode)
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p nil)
         (setq buffer-file-name nil)))))

(ert-deftest lean4-lake-build-runs-lake-build-at-the-root ()
  "The build runs in the package directory, not the file's, and ends in one."
  (lean4-lake-test--in-file "Pkg/Mod.lean" '("lakefile.toml" "Pkg/Mod.lean")
    (let ((captured (lean4-lake-test--capture (lean4-lake-build))))
      (should (string-suffix-p "build" (car captured)))
      (should (equal (file-truename (cdr captured)) (file-truename root))))))

(ert-deftest lean4-lake-build-has-the-three-steps-in-order ()
  "Resolving comes before the cache, and the cache before the build.
This is VS Code's \"Build Project\", which is not a bare `lake build'."
  (let* ((command (lean4-lake--build-command))
         (resolve (string-search "resolve-deps" command))
         (probe (string-search "exe cache >" command))
         (fetch (string-search "exe cache get" command))
         (build (string-search "build" command)))
    (should resolve)
    (should probe)
    (should fetch)
    (should build)
    (should (< resolve probe fetch build))
    ;; The probe's output is the executable's help text, and is discarded.
    (should (string-search ">/dev/null 2>&1" command))))

;; Run against a stub rather than Lake itself: what is being checked is which
;; steps run and which do not, and that needs no toolchain -- only a shell,
;; which `compile' needs anyway.
(defun lean4-lake-test--stub-build (available get-ok)
  "Run the build command with a stubbed Lake, returning what it did.

AVAILABLE says whether `lake exe cache' exists, GET-OK whether `lake exe
cache get' succeeds.  Returns a cons of the exit status and the list of
argument lists the stub was called with."
  (let* ((directory (make-temp-file "lean4-lake-stub" 'directory))
         (stub (expand-file-name "lake" directory))
         (log (expand-file-name "log" directory)))
    (unwind-protect
        (progn
          (write-region
           (concat "#!/bin/sh\n"
                   "echo \"$*\" >> \"$LEAN4_TEST_LOG\"\n"
                   "if [ \"$1 $2\" = \"exe cache\" ] && [ -z \"$3\" ]; then\n"
                   "  [ \"$LEAN4_TEST_AVAILABLE\" = 1 ] && exit 0\n"
                   "  exit 1\n"
                   "fi\n"
                   "if [ \"$1 $2 $3\" = \"exe cache get\" ]; then\n"
                   "  [ \"$LEAN4_TEST_GET_OK\" = 1 ] && exit 0\n"
                   "  exit 1\n"
                   "fi\n"
                   "exit 0\n")
           nil stub nil 'silent)
          (set-file-modes stub #o755)
          (let* ((lean4-rootdir nil)
                 (lean4-lake-name stub)
                 (default-directory directory)
                 (process-environment
                  (append (list (format "LEAN4_TEST_LOG=%s" log)
                                (format "LEAN4_TEST_AVAILABLE=%d"
                                        (if available 1 0))
                                (format "LEAN4_TEST_GET_OK=%d" (if get-ok 1 0)))
                          process-environment))
                 (status (call-process-shell-command
                          (lean4-lake--build-command) nil nil)))
            (cons status
                  (and (file-readable-p log)
                       (with-temp-buffer
                         (insert-file-contents log)
                         (split-string (buffer-string) "\n" t))))))
      (delete-directory directory 'recursive))))

(ert-deftest lean4-lake-build-skips-the-cache-where-there-is-none ()
  "A project without Mathlib's `cache' executable still builds.
The probe failing is not an error; it is the answer that there is nothing
to fetch."
  (skip-unless (executable-find "sh"))
  (pcase-let ((`(,status . ,calls) (lean4-lake-test--stub-build nil nil)))
    (should (equal status 0))
    (should (equal calls '("resolve-deps" "exe cache" "build")))))

(ert-deftest lean4-lake-build-fetches-the-cache-where-there-is-one ()
  "With the executable present, the cache is fetched before building."
  (skip-unless (executable-find "sh"))
  (pcase-let ((`(,status . ,calls) (lean4-lake-test--stub-build t t)))
    (should (equal status 0))
    (should (equal calls
                   '("resolve-deps" "exe cache" "exe cache get" "build")))))

(ert-deftest lean4-lake-build-stops-when-the-cache-fetch-fails ()
  "A cache download that fails stops the build.

The whole point of fetching first: left to itself, `lake build' would
answer a failed download by building Mathlib locally, which is hours.  VS
Code stops here too."
  (skip-unless (executable-find "sh"))
  (pcase-let ((`(,status . ,calls) (lean4-lake-test--stub-build t nil)))
    (should-not (equal status 0))
    (should (equal calls '("resolve-deps" "exe cache" "exe cache get")))
    (should-not (member "build" calls))))

(ert-deftest lean4-lake-fetch-cache-asks-for-the-whole-package ()
  "Fetching the cache names no files."
  (lean4-lake-test--in-file "Pkg/Mod.lean" '("lakefile.toml" "Pkg/Mod.lean")
    (should (string-suffix-p "exe cache get"
                             (car (lean4-lake-test--capture
                                    (lean4-lake-fetch-cache)))))))

(ert-deftest lean4-lake-fetch-file-cache-names-a-relative-path ()
  "`lake exe cache get' takes paths relative to the package."
  (lean4-lake-test--in-file "Pkg/Mod.lean" '("lakefile.toml" "Pkg/Mod.lean")
    (should (string-suffix-p "exe cache get Pkg/Mod.lean"
                             (car (lean4-lake-test--capture
                                    (lean4-lake-fetch-file-cache)))))))

(ert-deftest lean4-lake-fetch-file-cache-needs-a-file ()
  "A buffer visiting nothing is a user error, not a wrong-type one."
  (lean4-lake-test--with-tree '("lakefile.toml")
    (with-temp-buffer
      (setq default-directory root)
      (should-error (lean4-lake-fetch-file-cache) :type 'user-error))))

(ert-deftest lean4-lake-open-files-are-relative-and-filtered ()
  "Only Lean buffers under the package are collected, relative to it."
  (lean4-lake-test--with-tree '("lakefile.toml" "Pkg/Mod.lean" "Pkg/Other.lean")
    (let ((buffers nil))
      (unwind-protect
          (progn
            ;; Two Lean files under the package, one Lean file outside it, and
            ;; one buffer that is not Lean at all.
            (dolist (spec `((,(expand-file-name "Pkg/Mod.lean" root) lean4-mode)
                            (,(expand-file-name "Pkg/Other.lean" root) lean4-mode)
                            ("/elsewhere/Far.lean" lean4-mode)
                            (,(expand-file-name "Pkg/notes.org" root) text-mode)))
              (let ((buffer (generate-new-buffer " *lean4-lake-test*")))
                (push buffer buffers)
                (with-current-buffer buffer
                  (setq buffer-file-name (nth 0 spec))
                  (setq major-mode (nth 1 spec)))))
            (should (equal (sort (lean4-lake--open-files root) #'string<)
                           '("Pkg/Mod.lean" "Pkg/Other.lean"))))
        (dolist (buffer buffers)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (setq buffer-file-name nil))
          (kill-buffer buffer))))))

;;;; Creating and downloading projects

(defmacro lean4-lake-test--capture-in (&rest body)
  "Evaluate BODY, returning what `compile' was called with and where.
Also returns `ELAN_TOOLCHAIN' as it was set for the run, as a list of
command, directory and toolchain."
  (declare (indent 0) (debug (body)))
  `(let (captured)
     (cl-letf (((symbol-function 'compile)
                (lambda (command &rest _)
                  (setq captured
                        (list command default-directory
                              (getenv "ELAN_TOOLCHAIN")))
                  ;; `lean4-lake--run-in' sets a finish function on this.
                  (generate-new-buffer " *lean4-lake-test-compile*")))
               ((symbol-function 'dired) #'ignore)
               ((symbol-function 'message) #'ignore))
       ,@body)
     captured))

(ert-deftest lean4-lake-new-project-runs-the-four-steps ()
  "Init, update, build, commit -- each waiting on the one before."
  (lean4-lake-test--with-tree '()
    (let* ((directory (expand-file-name "MyPkg" root))
           (captured (lean4-lake-test--capture-in
                       (lean4-new-project directory)))
           (command (nth 0 captured)))
      (should (string-search "init MyPkg" command))
      (should (string-search "update" command))
      (should (string-search "build" command))
      (should (string-search "git add --all" command))
      (should (string-search "commit" command))
      ;; The package name is the directory's last component.
      (should-not (string-search "init MyPkg math" command))
      ;; Ordering, and a failure stopping the rest.
      (should (< (string-search "init" command)
                 (string-search "update" command)
                 (string-search "build" command)
                 (string-search "git add" command)))
      (should (string-search " && " command))
      (should (equal (file-truename (nth 1 captured))
                     (file-truename (file-name-as-directory directory))))
      (should (equal (nth 2 captured) "leanprover/lean4:stable"))
      (should (file-directory-p directory)))))

(ert-deftest lean4-lake-new-mathlib-project-uses-the-math-template ()
  "The `math' template, Mathlib's own toolchain, and the cache first."
  (lean4-lake-test--with-tree '()
    (let* ((directory (expand-file-name "MyMath" root))
           (captured (lean4-lake-test--capture-in
                       (lean4-new-mathlib-project directory)))
           (command (nth 0 captured)))
      (should (string-search "init MyMath math" command))
      (should (string-search "exe cache get" command))
      ;; Without the cache the first build compiles Mathlib from source.
      (should (< (string-search "exe cache get" command)
                 (string-search "build" command)))
      (should (equal (nth 2 captured) lean4-lake-mathlib-toolchain)))))

(ert-deftest lean4-lake-download-project-clones-then-builds ()
  "Clone, then exactly what `lean4-lake-build' does."
  (lean4-lake-test--with-tree '()
    (let* ((directory (expand-file-name "Cloned" root))
           (captured (lean4-lake-test--capture-in
                       (lean4-download-project
                        "https://example.invalid/pkg" directory)))
           (command (nth 0 captured)))
      ;; `shell-quote-argument' escapes the URL's colon, which the shell
      ;; undoes; compare against what it actually emits.
      (should (string-search (concat "git clone "
                                     (shell-quote-argument
                                      "https://example.invalid/pkg"))
                             command))
      (should (string-search (lean4-lake--build-command) command))
      (should (< (string-search "git clone" command)
                 (string-search "resolve-deps" command))))))

(ert-deftest lean4-lake-project-presets-are-the-two-vs-code-offers ()
  "Mathlib and the book that introduces it, each a real URL."
  (should (equal (mapcar #'car lean4-lake-project-presets)
                 '("Mathlib" "Mathematics in Lean")))
  (dolist (preset lean4-lake-project-presets)
    (should (string-prefix-p "https://github.com/leanprover-community/"
                             (cdr preset)))))

(ert-deftest lean4-lake-new-project-refuses-a-used-directory ()
  "`lake init' expects to be what fills the directory."
  (lean4-lake-test--with-tree '("Occupied/something.txt")
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) (expand-file-name "Occupied" root))))
      (should-error (call-interactively #'lean4-new-project)
                    :type 'user-error))
    ;; An absent directory is fine: it is about to be created.
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) (expand-file-name "Fresh" root))))
      (should (equal (lean4-lake--read-new-directory "x: ")
                     (expand-file-name "Fresh" root))))))

(ert-deftest lean4-lake-initial-commit-always-has-an-identity ()
  "A machine with no `user.name' must not fail the whole creation."
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (&rest _) "")))
    (let ((command (lean4-lake--initial-commit-command)))
      (should (string-search (shell-quote-argument "user.name=Lean 4 project")
                             command))
      (should (string-search "commit" command))))
  ;; With one configured, it is used rather than overridden.
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (&rest _) "Ada\n")))
    (should-not (string-search (shell-quote-argument "user.name=")
                               (lean4-lake--initial-commit-command)))))

;;;; Cleaning

(ert-deftest lean4-lake-clean-asks-before-deleting ()
  "Cleaning is confirmed first, and declining runs nothing."
  (lean4-lake-test--in-file "Pkg/Mod.lean" '("lakefile.toml" "Pkg/Mod.lean")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (string-suffix-p "clean"
                               (car (lean4-lake-test--capture
                                      (lean4-lake-clean))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (lean4-lake-test--capture (lean4-lake-clean))
                    :type 'user-error))))

;;;; Dependencies

(defmacro lean4-lake-test--with-manifest (json &rest body)
  "Write JSON as the Lake manifest of a package and evaluate BODY.
Inside BODY, `root' is the package directory."
  (declare (indent 1) (debug (form body)))
  `(lean4-lake-test--with-tree '("lakefile.toml")
     (write-region ,json nil (expand-file-name "lake-manifest.json" root)
                   nil 'silent)
     ,@body))

(ert-deftest lean4-lake-dependencies-read-the-current-manifest ()
  "From manifest version 7 on, a package's fields are at the top level."
  (lean4-lake-test--with-manifest
      "{\"version\": \"1.2.0\", \"name\": \"pkg\",
        \"packagesDir\": \".lake/packages\",
        \"packages\": [{\"type\": \"git\", \"name\": \"mathlib\",
                      \"url\": \"https://example.invalid/mathlib\",
                      \"rev\": \"abc\", \"inherited\": false}]}"
    (should (equal (lean4-lake--direct-dependencies root) '("mathlib")))))

(ert-deftest lean4-lake-dependencies-read-a-legacy-manifest ()
  "Up to version 6 the fields sat under a `git' object.
A checkout old enough to have one is exactly what would otherwise fail
silently."
  (lean4-lake-test--with-manifest
      "{\"version\": 6, \"packagesDir\": \".lake/packages\",
        \"packages\": [{\"git\": {\"name\": \"batteries\",
                               \"url\": \"https://example.invalid/b\",
                               \"rev\": \"abc\", \"inherited\": false}}]}"
    (should (equal (lean4-lake--direct-dependencies root) '("batteries")))))

(ert-deftest lean4-lake-dependencies-skip-inherited-and-path ()
  "Only direct Git dependencies can be updated from here.
An inherited package is a dependency's own dependency, and a path
dependency has no revision to move."
  (lean4-lake-test--with-manifest
      "{\"version\": \"1.2.0\", \"name\": \"pkg\",
        \"packagesDir\": \".lake/packages\",
        \"packages\": [{\"type\": \"git\", \"name\": \"direct\",
                      \"url\": \"https://example.invalid/d\",
                      \"rev\": \"a\", \"inherited\": false},
                     {\"type\": \"git\", \"name\": \"indirect\",
                      \"url\": \"https://example.invalid/i\",
                      \"rev\": \"b\", \"inherited\": true},
                     {\"type\": \"path\", \"name\": \"local\"}]}"
    (should (equal (lean4-lake--direct-dependencies root) '("direct")))))

(ert-deftest lean4-lake-dependencies-tolerate-a-bad-manifest ()
  "A missing or unreadable manifest yields nil rather than an error.
The format is Lake's to change, and erroring on one we do not recognise
is how a client breaks on the next version."
  (lean4-lake-test--with-tree '("lakefile.toml")
    (should-not (lean4-lake--direct-dependencies root)))
  (lean4-lake-test--with-manifest "not json at all {"
    (should-not (lean4-lake--direct-dependencies root)))
  (lean4-lake-test--with-manifest "{\"version\": \"1.2.0\"}"
    (should-not (lean4-lake--direct-dependencies root))))

(ert-deftest lean4-lake-update-dependency-names-it-and-asks ()
  "The chosen dependency is passed to `lake update', once confirmed."
  (lean4-lake-test--in-file "Pkg/Mod.lean" '("lakefile.toml" "Pkg/Mod.lean")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (string-suffix-p "update mathlib"
                               (car (lean4-lake-test--capture
                                      (lean4-lake-update-dependency
                                       "mathlib"))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (lean4-lake-test--capture
                     (lean4-lake-update-dependency "mathlib"))
                    :type 'user-error))))

(provide 'lean4-lake-test)
;;; lean4-lake-test.el ends here
