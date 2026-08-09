;;; build.el --- Build helpers for lean4-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lean4-Mode contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Batch entry points driven by the Makefile.  Each one is invoked as
;;
;;     emacs -Q --batch -l build.el -f lean4-build-TARGET
;;
;; and exits non-zero on failure.  Keeping this in Elisp rather than in
;; shell-quoted `--eval' arguments makes it possible to read and to debug.

;;; Code:

(require 'package)
(require 'lisp-mnt)
(require 'seq)
(require 'cl-lib)

(defconst lean4-build--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this file, i.e. the project root.")

(defconst lean4-build--main (expand-file-name "lean4-mode.el" lean4-build--dir)
  "The library carrying the package headers.")

(defun lean4-build--libraries ()
  "Return the absolute names of the package's libraries, in load order.
`build.el' itself and the test suite are excluded."
  (directory-files lean4-build--dir t (rx bos "lean4-" (+ nonl) ".el" eos)))

(defun lean4-build--exit (failures what)
  "Exit non-zero if FAILURES is non-nil, describing the run as WHAT."
  (if failures
      (progn
        (message "\n%s: %d problem(s)" what (length failures))
        (kill-emacs 1))
    (message "%s: clean" what)
    (kill-emacs 0)))

;;; Dependencies

(defun lean4-build--requirements ()
  "Return the package's dependencies as a list of `package-desc' requirements.
Reads the Package-Requires header of `lean4-mode.el' so that the
Makefile never has to be kept in sync with it by hand."
  (with-temp-buffer
    (insert-file-contents lean4-build--main)
    (let ((deps (lm-header "package-requires")))
      (when deps
        (package--prepare-dependencies
         (package-read-from-string deps))))))

(defun lean4-build-deps ()
  "Install the package's dependencies into `package-user-dir'."
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (package-refresh-contents)
  ;; package-lint is a build-time tool rather than a runtime dependency, so
  ;; it is not in Package-Requires.
  (dolist (req (cons '(package-lint "0") (lean4-build--requirements)))
    (let ((name (car req)))
      (cond
       ((eq name 'emacs))
       ((package-installed-p name (cadr req))
        (message "  %s already installed" name))
       (t
        (message "  installing %s" name)
        (package-install name)))))
  (kill-emacs 0))

;;; Targets

(defun lean4-build-compile ()
  "Byte-compile every library, treating warnings as errors."
  (let* ((byte-compile-error-on-warn t)
         failures)
    (dolist (file (lean4-build--libraries))
      (unless (byte-compile-file file)
        (push file failures)))
    (lean4-build--exit failures "byte-compile")))

(defun lean4-build-checkdoc ()
  "Run checkdoc over every library.
`checkdoc-file' never signals and writes nothing to standard output: it
binds `checkdoc-diagnostic-buffer' to \"*warn*\", which routes every
complaint through `warn'.  Intercept `warn' itself -- overriding
`display-warning' does not work, because preloaded `warn' does not reach
it through the symbol."
  (require 'checkdoc)
  ;; The verb check is flagged "experimental" by checkdoc itself and reports
  ;; false positives on any docstring where the verb's subject is not the
  ;; function: "pairs whose key sequence starts with PREFIX" is correct
  ;; English, and "start" would not be.
  (let ((checkdoc-verb-check-experimental-flag nil)
        complaints)
    (cl-letf (((symbol-function 'warn)
               (lambda (message &rest args)
                 (push (string-trim (apply #'format-message message args))
                       complaints))))
      (dolist (file (lean4-build--libraries))
        (checkdoc-file file)))
    (setq complaints (nreverse complaints))
    (dolist (complaint complaints)
      (princ (format "%s\n" complaint)))
    (lean4-build--exit complaints "checkdoc")))

(defvar package-lint-main-file)

(defun lean4-build-lint ()
  "Run package-lint over every library.
This is a multi-file package: only `lean4-mode.el' carries the headers,
so point package-lint at it, otherwise every other library is linted as
if it were a package of its own and reports missing headers and
dependencies that are in fact declared."
  (package-initialize)
  (require 'package-lint)
  (let ((package-lint-main-file (file-name-nondirectory lean4-build--main))
        (default-directory lean4-build--dir)
        errors)
    (dolist (file (lean4-build--libraries))
      (with-temp-buffer
        (insert-file-contents file t)
        ;; `package-lint-buffer' insists on a real Elisp buffer.
        (delay-mode-hooks (emacs-lisp-mode))
        (let ((buffer-file-name file))
          (dolist (complaint (package-lint-buffer))
            (pcase-let ((`(,line ,col ,severity ,message) complaint))
              (princ (format "%s:%d:%d: %s: %s\n"
                             (file-name-nondirectory file)
                             line col severity message))
              ;; Warnings are advisory -- package-lint flags idioms that are
              ;; merely discouraged.  Only errors fail the build.
              (when (eq severity 'error)
                (push complaint errors)))))))
    (lean4-build--exit errors "package-lint")))

(provide 'build)
;;; build.el ends here
