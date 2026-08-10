;;; lean4-eglot-test.el --- Tests for the Eglot glue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for workspace discovery and for the URI-to-buffer lookup that every
;; server notification is routed through.  No server is started: the one
;; place a connection would be needed is stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-eglot)
(require 'lean4-lake)

;; Eglot declares this with a valueless `defvar', which marks it special only
;; within eglot.el.  Repeat the declaration here so that let-binding it below
;; is a dynamic binding rather than a lexical one.
(defvar eglot-lsp-context)

(defmacro lean4-eglot-test--with-tree (spec &rest body)
  "Create a temporary directory tree described by SPEC and evaluate BODY.
SPEC is a list of relative file names, each created empty along with any
directories it needs.  Inside BODY, `root' is bound to the tree's root,
with a trailing slash."
  (declare (indent 1) (debug (form body)))
  `(let ((root (file-name-as-directory
                (file-truename (make-temp-file "lean4-eglot-test" 'directory)))))
     (unwind-protect
         (progn
           (dolist (relative ,spec)
             (let ((file (expand-file-name relative root)))
               (make-directory (file-name-directory file) 'parents)
               (write-region "" nil file nil 'silent)))
           ,@body)
       (delete-directory root 'recursive))))

(defun lean4-eglot-test--root-for (file)
  "Return `lean4--workspace-root' for FILE, as a true name."
  (when-let* ((root (lean4--workspace-root file)))
    (file-truename root)))

;;;; Workspace discovery

(ert-deftest lean4-eglot-workspace-root-finds-toolchain ()
  "The directory holding \"lean-toolchain\" is the workspace root."
  (lean4-eglot-test--with-tree '("lean-toolchain" "Pkg/Mod.lean")
    (should (equal (lean4-eglot-test--root-for
                    (expand-file-name "Pkg/Mod.lean" root))
                   root))))

(ert-deftest lean4-eglot-workspace-root-prefers-outermost ()
  "A vendored package resolves to the *enclosing* project's toolchain.
A dependency under `.lake/packages' ships its own \"lean-toolchain\",
but it has to be elaborated by the toolchain of the project that pulled
it in, so the outer one wins.  This is the opposite of how the Lake
build directory is chosen."
  (lean4-eglot-test--with-tree '("lean-toolchain"
                                 ".lake/packages/dep/lean-toolchain"
                                 ".lake/packages/dep/Dep.lean")
    (should (equal (lean4-eglot-test--root-for
                    (expand-file-name ".lake/packages/dep/Dep.lean" root))
                   root))))

(ert-deftest lean4-eglot-workspace-root-nil-without-toolchain ()
  "A file outside any Lean project has no workspace root."
  (lean4-eglot-test--with-tree '("Loose.lean")
    (should-not (lean4-eglot-test--root-for
                 (expand-file-name "Loose.lean" root)))))

(ert-deftest lean4-eglot-workspace-exclusions-suppress-the-server ()
  "No workspace is reported below a directory that has been excluded."
  (lean4-eglot-test--with-tree '("lean-toolchain" "vendor/Mod.lean")
    (let ((lean4-workspace-exclusions (list (expand-file-name "vendor" root))))
      (should-not (lean4-eglot-test--root-for
                   (expand-file-name "vendor/Mod.lean" root)))
      ;; A sibling outside the exclusion is unaffected.
      (should (equal (lean4-eglot-test--root-for
                      (expand-file-name "Other.lean" root))
                     root)))))

(ert-deftest lean4-eglot-workspace-roots-win-over-toolchain ()
  "A configured root is used even when an outer toolchain file exists."
  (lean4-eglot-test--with-tree '("lean-toolchain" "sub/lean-toolchain"
                                 "sub/Mod.lean")
    (let ((lean4-workspace-roots (list (expand-file-name "sub" root))))
      (should (equal (lean4-eglot-test--root-for
                      (expand-file-name "sub/Mod.lean" root))
                     (file-name-as-directory (expand-file-name "sub" root)))))))

(ert-deftest lean4-eglot-project-find-answers-only-for-eglot ()
  "`lean4-project-find' claims a directory only when Eglot is asking.
Lean workspaces are rooted at a toolchain file, which is usually not
what the user means by \"project\", so answering unconditionally would
hijack `project.el' for the whole repository."
  (lean4-eglot-test--with-tree '("lean-toolchain" "Mod.lean")
    (let ((file (expand-file-name "Mod.lean" root)))
      (let ((eglot-lsp-context t))
        (let ((project (lean4-project-find file)))
          (should (eq (car project) 'lean4))
          (should (equal (file-truename (project-root project)) root))))
      (let ((eglot-lsp-context nil))
        (should-not (lean4-project-find file))))))

(ert-deftest lean4-eglot-project-find-ignores-non-files ()
  "Buffers not visiting a file yield no project."
  (let ((eglot-lsp-context t))
    (should-not (lean4-project-find nil))))

(ert-deftest lean4-eglot-project-backend-is-registered-by-the-mode ()
  "project.el learns about us when a Lean file is opened, not before.

`project-find-functions' is a global hook, and a package that has been
installed but not used has no business being in it.  It has to be
registered before `eglot-ensure', which is what asks project.el where
the file's root is."
  (let ((project-find-functions nil))
    (with-temp-buffer
      (let ((lean4-mode-hook nil)
            (lean4-auto-start-server nil)
            (lean4-info-auto-open nil))
        (lean4-mode))
      (should (memq #'lean4-project-find project-find-functions)))))

;;;; Server command

(ert-deftest lean4-eglot-server-command-uses-lake-for-a-package ()
  "A directory with a lakefile is served by Lake, so deps resolve."
  (lean4-eglot-test--with-tree '("lean-toolchain" "lakefile.toml")
    (let ((lean4-rootdir nil)
          (lean4-server-arguments nil))
      (should (equal (lean4--server-command root)
                     (list lean4-lake-name "serve" "--"))))))

(ert-deftest lean4-eglot-server-command-uses-lean-without-a-package ()
  "A loose file is served by Lean itself; Lake would fail outside a package."
  (let ((lean4-rootdir nil)
        (lean4-server-arguments nil))
    (should (equal (lean4--server-command nil)
                   (list lean4-executable-name "--server")))))

(ert-deftest lean4-eglot-server-command-appends-user-arguments ()
  "`lean4-server-arguments' reaches the command line."
  (let ((lean4-rootdir nil)
        (lean4-server-arguments '("--memory=4096")))
    (should (equal (last (lean4--server-command nil)) '("--memory=4096")))))

;;;; URI to buffer

(ert-deftest lean4-eglot-with-uri-buffers-finds-the-right-buffer ()
  "`lean4-with-uri-buffers' runs its body in the buffer visiting the URI.

Regression test.  A previous implementation keyed the lookup on
`eglot--TextDocumentIdentifier-cache', which does not exist before Eglot
1.16.  Guarded with `boundp', that silently matched no buffers at all on
Emacs 29, disabling everything driven from server notifications --
fringe progress, and the info-buffer refresh -- with no error anywhere."
  (lean4-eglot-test--with-tree '("lean-toolchain" "A.lean" "B.lean")
    (let* ((file-a (expand-file-name "A.lean" root))
           (file-b (expand-file-name "B.lean" root))
           (buffer-a (find-file-noselect file-a))
           (buffer-b (find-file-noselect file-b))
           (visited '()))
      (unwind-protect
          (cl-letf (((symbol-function 'eglot--managed-buffers)
                     (lambda (_server) (list buffer-a buffer-b))))
            (lean4-with-uri-buffers nil (lean4--path-to-uri file-a)
              (push (buffer-name) visited))
            (should (equal visited (list (buffer-name buffer-a)))))
        (kill-buffer buffer-a)
        (kill-buffer buffer-b)))))

(ert-deftest lean4-eglot-with-uri-buffers-ignores-unrelated-uris ()
  "A URI naming no open buffer runs the body nowhere."
  (lean4-eglot-test--with-tree '("lean-toolchain" "A.lean")
    (let* ((file-a (expand-file-name "A.lean" root))
           (buffer-a (find-file-noselect file-a))
           (ran nil))
      (unwind-protect
          (cl-letf (((symbol-function 'eglot--managed-buffers)
                     (lambda (_server) (list buffer-a))))
            (lean4-with-uri-buffers nil
                (lean4--path-to-uri (expand-file-name "Absent.lean" root))
              (setq ran t))
            (should-not ran))
        (kill-buffer buffer-a)))))

;; Declared, not defined: an Eglot old enough to lack it must still leave
;; `boundp' answering no, and `let' binds it dynamically only if it is
;; known to be special.
(defvar eglot-semantic-token-types)

(ert-deftest lean4-eglot-semantic-tokens-ask-for-leans-own-type ()
  "`leanSorryLike' is registered, and answers to the name Eglot looks up.

Eglot paints only the types listed in `eglot-semantic-token-types', with
the face called eglot-semantic-TYPE, and defines those faces for the
types it knows -- which Lean's own is not, so the alias is what gives
the token a face at all."
  (let ((eglot-semantic-token-types '("keyword"))
        (enabled nil))
    (cl-letf (((symbol-function 'eglot-semantic-tokens-mode)
               (lambda (&rest _) (setq enabled t))))
      (lean4--setup-semantic-tokens))
    (should enabled)
    (should (member "leanSorryLike" eglot-semantic-token-types)))
  (should (eq (get 'eglot-semantic-leanSorryLike 'face-alias)
              'lean4-semantic-leanSorryLike))
  (should (facep 'eglot-semantic-leanSorryLike)))

(ert-deftest lean4-eglot-semantic-tokens-are-skipped-by-an-older-eglot ()
  "An Eglot without semantic tokens is left alone rather than patched."
  (cl-letf (((symbol-function 'eglot-semantic-tokens-mode) nil))
    (should-not (fboundp 'eglot-semantic-tokens-mode))
    (should-not (lean4--setup-semantic-tokens))))

;;;; Dependency builds

;; A subclass, so that the `:around' method under test still applies while the
;; primary method that would write to a process is replaced by one that
;; records instead.  Stubbing `jsonrpc-connection-send' itself would shadow the
;; very method being tested, and defining the replacement on
;; `lean4-eglot-lsp-server' would shadow it for the end-to-end suite too.
(defclass lean4-eglot-test--recorder (lean4-eglot-lsp-server) nil
  :documentation "A server that records what it is asked to send.")

(defvar lean4-eglot-test--sent nil
  "Keyword-argument lists the recording server was asked to send.")

(cl-defmethod jsonrpc-connection-send ((_server lean4-eglot-test--recorder)
                                       &rest args &key &allow-other-keys)
  "Record ARGS rather than writing them to a process."
  (push args lean4-eglot-test--sent))

(defun lean4-eglot-test--send (&rest args)
  "Send ARGS through a recording server and return them as they went out.
The server needs a live process to be constructed at all -- jsonrpc sets
filters and buffers on it -- so it gets a pipe, which has no subprocess
behind it and never reads what is written."
  (let* ((lean4-eglot-test--sent nil)
         (process (make-pipe-process :name "lean4-eglot-test" :noquery t)))
    (unwind-protect
        (let ((server (lean4-eglot-test--recorder
                       :name "lean4-eglot-test" :process process)))
          (apply #'jsonrpc-connection-send server args)
          (car lean4-eglot-test--sent))
      (delete-process process)
      (dolist (buffer '(" *lean4-eglot-test output*" " *lean4-eglot-test stderr*"
                        "*lean4-eglot-test events*"))
        (when (get-buffer buffer) (kill-buffer buffer))))))

(defun lean4-eglot-test--did-open-mode (&optional once)
  "Return the `dependencyBuildMode' that goes out with a `didOpen'.
With ONCE, as `lean4-refresh-file-dependencies' asks for it."
  (let ((lean4--build-dependencies-once once))
    (plist-get (plist-get (lean4-eglot-test--send
                           :method :textDocument/didOpen
                           :params '(:textDocument (:uri "file:///x.lean")))
                          :params)
               :dependencyBuildMode)))

(ert-deftest lean4-eglot-did-open-declines-dependency-builds ()
  "An ordinary open says so explicitly rather than saying nothing.
Lean reads a missing `dependencyBuildMode' as \"always\", so silence
here would build Mathlib on a file opened in order to read it."
  (let ((lean4-automatically-build-dependencies nil))
    (should (equal (lean4-eglot-test--did-open-mode) "never"))))

(ert-deftest lean4-eglot-refresh-asks-for-one-build ()
  "A refresh asks for \"once\", not \"always\".
\"Once\" reverts to \"never\" on the server, so a file worker that
crashes afterwards does not set the build going again."
  (let ((lean4-automatically-build-dependencies nil))
    (should (equal (lean4-eglot-test--did-open-mode 'once) "once"))))

(ert-deftest lean4-eglot-dependency-builds-can-be-automatic ()
  "The option overrides both, the way VS Code's setting does."
  (let ((lean4-automatically-build-dependencies t))
    (should (equal (lean4-eglot-test--did-open-mode) "always"))
    (should (equal (lean4-eglot-test--did-open-mode 'once) "always"))))

(ert-deftest lean4-eglot-did-open-keeps-the-document ()
  "The field is added to the params rather than replacing them."
  (should (equal (plist-get (plist-get
                             (lean4-eglot-test--send
                              :method :textDocument/didOpen
                              :params '(:textDocument (:uri "file:///x.lean")))
                             :params)
                            :textDocument)
                 '(:uri "file:///x.lean"))))

(ert-deftest lean4-eglot-other-messages-are-untouched ()
  "Only `textDocument/didOpen' gains the field."
  (should-not (plist-member (plist-get (lean4-eglot-test--send
                                        :method :textDocument/didClose
                                        :params '(:textDocument nil))
                                       :params)
                            :dependencyBuildMode)))

(provide 'lean4-eglot-test)
;;; lean4-eglot-test.el ends here
