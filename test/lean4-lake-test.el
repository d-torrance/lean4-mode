;;; lean4-lake-test.el --- Tests for Lake project detection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

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

(provide 'lean4-lake-test)
;;; lean4-lake-test.el ends here
