;;; lean4-setup-test.el --- Tests for the setup checks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; What is said about a setup, and -- as much to the point -- how seldom.  A
;; warning that arrives on every file rather than on every machine is a nag,
;; and a nag is turned off, taking the useful ones with it.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lean4-mode)

(defmacro lean4-setup-test--recording (&rest body)
  "Evaluate BODY with the setup checks fresh and warnings recorded.
The warnings appear in `warnings' as (LEVEL . TEXT), oldest first."
  (declare (indent 0) (debug (body)))
  `(let ((warnings nil)
         (lean4-show-setup-warnings t))
     (lean4-setup-forget)
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type message &optional level &rest _)
                  (push (cons (or level :warning) message) warnings))))
       ,@body)
     (setq warnings (nreverse warnings))
     ,'(ignore warnings)
     warnings))

(defmacro lean4-setup-test--with-machine (missing lean lake elan &rest body)
  "Evaluate BODY with a machine pretended.
MISSING is what `lean4-missing-dependencies' answers, LEAN and LAKE
whether those programs are found, and ELAN elan\\='s major version or nil."
  (declare (indent 4) (debug (form form form form body)))
  `(cl-letf (((symbol-function 'lean4-missing-dependencies) (lambda () ,missing))
             ((symbol-function 'executable-find)
              (lambda (program &rest _)
                (cond ((equal program lean4-executable-name) ,lean)
                      ((equal program lean4-lake-name) ,lake)
                      (t t))))
             ((symbol-function 'lean4-toolchain--elan-major-version)
              (lambda () ,elan)))
     ,@body))

(defun lean4-setup-test--levels (warnings)
  "Return the levels of WARNINGS."
  (mapcar #'car warnings))

(defun lean4-setup-test--says (warnings text)
  "Return non-nil if any of WARNINGS mentions TEXT."
  (seq-some (lambda (warning) (string-search text (cdr warning))) warnings))

;;;; The machine

(ert-deftest lean4-setup-says-nothing-about-a-good-machine ()
  "A machine with everything on it is not worth a word."
  (should-not (lean4-setup-test--recording
                (lean4-setup-test--with-machine nil t t 4
                  (lean4-setup--check-machine)))))

(ert-deftest lean4-setup-reports-missing-dependencies ()
  "Curl and Git are what elan and Lake need, and an error without."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine '("git" "curl") t t 4
                      (lean4-setup--check-machine)))))
    (should (equal (lean4-setup-test--levels warnings) '(:error)))
    (should (lean4-setup-test--says warnings "git and curl"))
    (should (lean4-setup-test--says warnings "lean4-install-dependencies"))))

(ert-deftest lean4-setup-reports-a-missing-lean ()
  "No Lean at all is the error that matters most."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine nil nil t 4
                      (lean4-setup--check-machine)))))
    (should (equal (lean4-setup-test--levels warnings) '(:error)))
    (should (lean4-setup-test--says warnings "lean4-install-elan"))))

(ert-deftest lean4-setup-reports-a-missing-lake ()
  "And no Lake means a file in a package goes unserved."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine nil t nil 4
                      (lean4-setup--check-machine)))))
    (should (equal (lean4-setup-test--levels warnings) '(:error)))
    (should (lean4-setup-test--says warnings "serve"))))

(ert-deftest lean4-setup-reports-a-missing-elan-as-a-warning ()
  "Elan missing is not fatal: a Lean on the path still runs."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine nil t t nil
                      (lean4-setup--check-machine)))))
    (should (equal (lean4-setup-test--levels warnings) '(:warning)))
    (should (lean4-setup-test--says warnings "Elan not found"))))

(ert-deftest lean4-setup-reports-an-old-elan ()
  "Elan 3 runs Lean and cannot say what a version resolves to."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine nil t t 3
                      (lean4-setup--check-machine)))))
    (should (equal (lean4-setup-test--levels warnings) '(:warning)))
    (should (lean4-setup-test--says warnings "dump-state"))))

(ert-deftest lean4-setup-checks-the-machine-once-a-session ()
  "Said once.  A warning on every file opened is a nag, and a nag gets
turned off along with the warnings worth having."
  (let ((warnings (lean4-setup-test--recording
                    (lean4-setup-test--with-machine nil nil t 4
                      (lean4-setup--check-machine)
                      (lean4-setup--check-machine)
                      (lean4-setup--check-machine)))))
    (should (= (length warnings) 1))))

;;;; The project

(defmacro lean4-setup-test--with-project (files &rest body)
  "Evaluate BODY with a temporary project directory containing FILES.
The directory is `root' within BODY, and is the workspace root as far as
the checks can tell."
  (declare (indent 1) (debug (form body)))
  `(let ((root (file-name-as-directory (make-temp-file "lean4-setup" t))))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (with-temp-file (expand-file-name file root) (insert "x\n")))
           (cl-letf (((symbol-function 'lean4--workspace-root) (lambda (&rest _) root)))
             ,@body))
       (delete-directory root t))))

(ert-deftest lean4-setup-says-nothing-about-a-good-project ()
  "A project with a toolchain file is in order."
  (lean4-setup-test--with-project '("lean-toolchain" "lakefile.toml")
    (should-not (lean4-setup-test--recording (lean4-setup--check-project)))))

(ert-deftest lean4-setup-reports-a-project-without-a-toolchain ()
  "Without one, elan's default decides the version, which moves."
  (lean4-setup-test--with-project '("lakefile.toml")
    (let ((warnings (lean4-setup-test--recording
                      (lean4-setup--check-project))))
      (should (equal (lean4-setup-test--levels warnings) '(:warning)))
      (should (lean4-setup-test--says warnings "lean-toolchain")))))

(ert-deftest lean4-setup-reports-a-lean-3-project ()
  "A `leanpkg.toml' and no toolchain file is Lean 3, which this is not."
  (lean4-setup-test--with-project '("leanpkg.toml")
    (let ((warnings (lean4-setup-test--recording
                      (lean4-setup--check-project))))
      (should (equal (lean4-setup-test--levels warnings) '(:error)))
      (should (lean4-setup-test--says warnings "Lean 3")))))

(ert-deftest lean4-setup-a-lean-4-project-may-keep-a-leanpkg-toml ()
  "One that has both is a Lean 4 project which has not tidied up."
  (lean4-setup-test--with-project '("leanpkg.toml" "lean-toolchain")
    (should-not (lean4-setup-test--recording (lean4-setup--check-project)))))

;;;; The Lean a project pins

(defmacro lean4-setup-test--pinning (pinned installed &rest body)
  "Evaluate BODY with a project pinning PINNED and elan holding INSTALLED."
  (declare (indent 2) (debug (form form body)))
  `(cl-letf (((symbol-function 'lean4--toolchain-string) (lambda () ,pinned))
             ((symbol-function 'lean4-toolchain-installed) (lambda () ,installed)))
     ,@body))

(ert-deftest lean4-setup-reports-a-toolchain-not-installed ()
  "A project pinning a version elan has not got downloads it on the first
file opened, which can outlast the connection timeout and look like a hang."
  (lean4-setup-test--with-project '("lean-toolchain")
    (lean4-setup-test--pinning "leanprover/lean4:v4.33.0"
        '("leanprover/lean4:v4.32.2")
      (let ((warnings (lean4-setup-test--recording
                        (lean4-setup--check-project))))
        (should (equal (lean4-setup-test--levels warnings) '(:warning)))
        (should (lean4-setup-test--says warnings "v4.33.0"))
        (should (lean4-setup-test--says warnings "lean4-install-toolchain"))))))

(ert-deftest lean4-setup-says-nothing-when-the-toolchain-is-there ()
  "Having it is the ordinary case and not worth a word."
  (lean4-setup-test--with-project '("lean-toolchain")
    (lean4-setup-test--pinning "leanprover/lean4:v4.33.0"
        '("leanprover/lean4:v4.33.0" "leanprover/lean4:v4.32.2")
      (should-not (lean4-setup-test--recording (lean4-setup--check-project))))))

(ert-deftest lean4-setup-compares-versions-not-names ()
  "A `lean-toolchain' may say `4.33.0' where elan reports `v4.33.0'."
  (lean4-setup-test--with-project '("lean-toolchain")
    (lean4-setup-test--pinning "leanprover/lean4:4.33.0"
        '("leanprover/lean4:v4.33.0")
      (should-not (lean4-setup-test--recording (lean4-setup--check-project))))))

(ert-deftest lean4-setup-says-nothing-about-a-channel ()
  "A project pinning a channel is pinning whatever it resolves to today,
which is elan's to decide and may be a version nobody has yet."
  (lean4-setup-test--with-project '("lean-toolchain")
    (lean4-setup-test--pinning "leanprover/lean4:stable"
        '("leanprover/lean4:v4.33.0")
      (should-not (lean4-setup-test--recording (lean4-setup--check-project))))))

(ert-deftest lean4-setup-says-nothing-when-elan-cannot-be-asked ()
  "Without elan there is no list to compare against, and the machine check
has already said so."
  (lean4-setup-test--with-project '("lean-toolchain")
    (lean4-setup-test--pinning "leanprover/lean4:v4.33.0"
        (error "No elan here")
      (should-not (lean4-setup-test--recording (lean4-setup--check-project))))))

(ert-deftest lean4-setup-checks-a-project-once ()
  "Once per project, however many of its files are opened."
  (lean4-setup-test--with-project '("lakefile.toml")
    (let ((warnings (lean4-setup-test--recording
                      (lean4-setup--check-project)
                      (lean4-setup--check-project))))
      (should (= (length warnings) 1)))))

;;;; Saying nothing

(ert-deftest lean4-setup-can-be-turned-off ()
  "`lean4-show-setup-warnings' nil says nothing at all."
  (let ((warnings (lean4-setup-test--recording
                    (let ((lean4-show-setup-warnings nil))
                      (lean4-setup-test--with-machine '("git") nil nil nil
                        (lean4-setup-check))))))
    (should-not warnings)))

(ert-deftest lean4-setup-says-nothing-about-a-buffer-with-no-file ()
  "A buffer with no file behind it is ordinary in Emacs, not a setup gone
wrong: editing a Lean block in an Org document gives one, and so does
every `with-temp-buffer'.  VS Code warns about its unsaved files; warning
about all of these would be a nag, and a nag is turned off along with the
warnings worth keeping."
  (with-temp-buffer
    (should-not (lean4-setup-test--recording
                  (lean4-setup-test--with-machine nil t t 4
                    (lean4-setup-check))))))

(provide 'lean4-setup-test)
;;; lean4-setup-test.el ends here
