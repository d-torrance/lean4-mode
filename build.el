;;; build.el --- Build helpers for lean4-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

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
  "Return the package's dependencies as a list of (NAME VERSION-LIST).
Reads the Package-Requires header of `lean4-mode.el' so that the
Makefile never has to be kept in sync with it by hand."
  (with-temp-buffer
    (insert-file-contents lean4-build--main)
    (when-let* ((header (lm-header "package-requires")))
      (mapcar (lambda (dep)
                (list (car dep) (version-to-list (cadr dep))))
              (package-read-from-string header)))))

(defun lean4-build-deps ()
  "Install the package's dependencies into `package-user-dir'."
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (package-refresh-contents)
  ;; package-lint is a build-time tool rather than a runtime dependency, so
  ;; it is not in Package-Requires.  Its version is a list, not a string:
  ;; that is what `package--prepare-dependencies' produces, and what
  ;; `package-installed-p' expects.
  (dolist (req (cons '(package-lint (0)) (lean4-build--requirements)))
    (let ((name (car req)))
      (cond
       ((eq name 'emacs))
       ((package-installed-p name (cadr req))
        (message "  %s already installed" name))
       (t
        (message "  installing %s" name)
        (package-install name)))))
  (kill-emacs 0))

;;; Abbreviations

(defconst lean4-build--abbreviations-url
  "https://raw.githubusercontent.com/leanprover/vscode-lean4/master/lean4-unicode-input/src/abbreviations.json"
  "Where the Unicode abbreviation table is maintained.
It belongs to the VS Code extension; keeping our copy in step with it is
what stops the two editors disagreeing about what \\=\\alpha means.")

(defun lean4-build-abbreviations ()
  "Refresh data/abbreviations.json from vscode-lean4.

The file stays checked in -- package managers need it present, and a
build that reaches the network is a build that fails on a train -- but
refreshing it should not be a manual copy-and-paste.  The CI workflow
in .github/workflows/update-abbr.yml does the same thing on a schedule.

Entries containing $CURSOR are dropped: that is the extension's way of
saying where to leave point after expanding, and Quail has no
equivalent, so keeping them would insert the literal text."
  (require 'url)
  (require 'json)
  (let ((target (expand-file-name "data/abbreviations.json" lean4-build--dir))
        (kept 0) (dropped 0))
    (with-temp-buffer
      (url-insert-file-contents lean4-build--abbreviations-url)
      (goto-char (point-min))
      (let* ((table (let ((json-key-type 'string))
                      (if (fboundp 'json-parse-buffer)
                          (json-parse-buffer :object-type 'alist)
                        (json-read))))
             (filtered (seq-remove
                        (lambda (entry)
                          (if (string-match-p "\\$CURSOR" (cdr entry))
                              (progn (cl-incf dropped) t)
                            (cl-incf kept) nil))
                        table)))
        (with-temp-file target
          (insert "{\n")
          (insert (mapconcat
                   (lambda (entry)
                     (format "  %s: %s"
                             (json-encode-string (car entry))
                             (json-encode-string (cdr entry))))
                   filtered ",\n"))
          (insert "\n}\n"))))
    (message "abbreviations: kept %d, dropped %d containing $CURSOR"
             kept dropped)
    (kill-emacs 0)))

;;; Manual

(defconst lean4-build--html-dir (expand-file-name "public" lean4-build--dir)
  "Where `lean4-build-html' writes the web manual.
Named for what the GitHub Pages action expects to upload rather than
for what it holds.")

(defconst lean4-build--html-assets '("manual.css" "manual.js")
  "Files copied out of doc/ to sit beside the exported manual.")

(defun lean4-build--anchor (heading)
  "Return an HTML anchor derived from HEADING.
Left to itself Org numbers anchors from the document's structure, so
inserting a section renames every anchor below it and quietly breaks
whatever links people have already made.  Deriving the anchor from the
text keeps it stable across edits, and readable besides -- the same
bargain Texinfo node names strike."
  (let* ((text (substring-no-properties heading))
         ;; Dropped rather than replaced: "Mathlib's" should become
         ;; "Mathlibs", not "Mathlib-s".
         (text (replace-regexp-in-string "['’]" "" text))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" text)))
    (string-trim slug "-+" "-+")))

(defun lean4-build--label-headlines ()
  "Give every headline in the current Org buffer a CUSTOM_ID.
`org-html-prefer-user-labels' then uses these as the anchors, in place
of the generated ones.  Collisions are numbered, so two sections that
happen to share a name still get an anchor apiece."
  (let ((seen (make-hash-table :test #'equal)))
    (org-map-entries
     (lambda ()
       (let* ((base (lean4-build--anchor (org-get-heading t t t t)))
              (count (puthash base (1+ (gethash base seen 0)) seen)))
         (org-entry-put nil "CUSTOM_ID"
                        (if (= count 1) base (format "%s-%d" base count))))))))

(defun lean4-build-html ()
  "Export README.org to a web manual under `lean4-build--html-dir'.
This is the same source the .texi and .info manuals are made from, so
the website cannot drift away from what the package ships.

Org's own stylesheet and scripts are turned off in favour of doc/,
and the export is kept free of timestamps so that rebuilding an
unchanged README produces an unchanged page."
  (require 'org)
  (require 'ox-html)
  (let ((readme (expand-file-name "README.org" lean4-build--dir))
        (target (expand-file-name "index.html" lean4-build--html-dir)))
    (make-directory lean4-build--html-dir t)
    (with-temp-buffer
      (insert-file-contents readme)
      ;; Relative links and the export machinery both resolve against this.
      (setq default-directory lean4-build--dir)
      (delay-mode-hooks (org-mode))
      (lean4-build--label-headlines)
      (let ((org-html-doctype "html5")
            (org-html-html5-fancy t)
            (org-html-head-include-default-style nil)
            (org-html-head-include-scripts nil)
            (org-html-head
             (concat
              "<meta name=\"description\" content=\"Manual for lean4-mode,"
              " the Emacs major mode for the Lean 4 language.\">\n"
              "<link rel=\"stylesheet\" href=\"manual.css\">\n"
              "<script defer src=\"manual.js\"></script>"))
            (org-html-prefer-user-labels t)
            (org-html-validation-link nil)
            ;; Syntax highlighting would mean htmlize, a theme, and a
            ;; frame's worth of faces in a batch Emacs, all to colour nine
            ;; short blocks.  Left off, and the stylesheet labels each
            ;; block with its language instead.
            (org-html-htmlize-output-type nil)
            ;; Org still decorates tables with border, cellpadding and
            ;; friends, which no stylesheet can talk it out of.
            (org-html-table-default-attributes nil)
            (org-html-postamble
             (concat
              "<p>Generated from README.org in the "
              "<a href=\"https://github.com/d-torrance/lean4-mode\">"
              "lean4-mode repository</a>.  The same text ships with the"
              " package as an Info manual: <code>C-h i</code>, then"
              " <code>m Lean4-Mode</code>.</p>"))
            (org-export-with-author nil)
            (org-export-with-creator nil)
            ;; Numbering the sections would be no worse in itself, but a
            ;; cross-reference with no description of its own then exports
            ;; as the bare number: README's "see [[*Acknowledgements]]"
            ;; becomes "see 6".  Unnumbered, it says "see Acknowledgements",
            ;; which is both readable and what the Info manual says.
            (org-export-with-section-numbers nil)
            (org-export-time-stamp-file nil))
        (org-export-to-file 'html target)))
    (dolist (asset lean4-build--html-assets)
      (copy-file (expand-file-name (concat "doc/" asset) lean4-build--dir)
                 (expand-file-name asset lean4-build--html-dir)
                 t))
    ;; Without this, Pages runs the output through Jekyll, which discards
    ;; any file whose name begins with an underscore.
    (write-region "" nil (expand-file-name ".nojekyll" lean4-build--html-dir))
    (message "html: wrote %s" target)
    (kill-emacs 0)))

;;; Targets

;; Declared, not merely bound: recent Emacs made this lexical, and
;; `let'-binding a lexical variable in a file that never declared it
;; special fails with "Defining as dynamic an already lexical var".
(defvar byte-compile-error-on-warn)

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
