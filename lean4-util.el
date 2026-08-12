;;; lean4-util.el --- Lean4-Mode Utilities  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

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

;; This library provides utilities for `lean4-mode'.

;;; Code:

(require 'magit-section)
(require 'seq)

(require 'lean4-settings)

;;;; JSON

;; Lean, Lake and elan all speak JSON, and every one of them is read the
;; same way: plists, because that is what the rest of this package reads
;; with `plist-get', and with JSON's `null' and `false' both arriving as
;; nil, because Elisp has no third thing to distinguish them by.  Spelt
;; out at each call site, the four keywords were four chances to get one
;; of them wrong.

(defun lean4--json-parse-buffer ()
  "Parse the JSON after point in the current buffer, as plists.
Signals the way `json-parse-buffer' does if it does not parse."
  (json-parse-buffer :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun lean4--json-parse-string (string)
  "Parse STRING as JSON, as plists.
Signals the way `json-parse-string' does if it does not parse."
  (json-parse-string string :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(define-error 'lean4-no-http-body "No response body")

(defun lean4--json-parse-http-body ()
  "Parse the JSON body of the HTTP response in the current buffer.

Point is moved past the headers first.  Signals `lean4-no-http-body'
where there are none to move past, which is what a proxy\\='s error page
looks like from here, and lets a parse error through as it comes:
callers word both for themselves."
  (goto-char (point-min))
  (unless (re-search-forward "^\r?$" nil t)
    (signal 'lean4-no-http-body nil))
  (forward-line)
  (lean4--json-parse-buffer))

;;;; Sections

;; The goal display, the module hierarchy and the Loogle results are all
;; `magit-section' buffers, and all three want the same two things of it.

(defun lean4--map-sections (function)
  "Call FUNCTION on every section of the current buffer, the root included.
Does nothing where there is no section tree yet.

`magit-map-sections' does this, but only from magit-section 4, and 3.3.0
is what Debian and Ubuntu still ship; this can give way to it when that
stops mattering."
  (when (bound-and-true-p magit-root-section)
    (letrec ((walk (lambda (section)
                     (funcall function section)
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))))

(defmacro lean4--section-body (variable &rest body)
  "Insert BODY as a section\\='s body, preserving VARIABLE across the wait.

`magit-section' puts the body of a section that starts folded aside and
runs it when the reader opens the section.  How far in we were is a
dynamic binding, and it has long unwound by then, so a body run that way
comes out at the outermost level -- every child of a folded node drawn
hard left.  VARIABLE is captured here and bound again when the body
finally runs."
  (declare (indent 1) (debug (symbolp body)))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved ,variable))
       (magit-insert-section-body
         (let ((,variable ,saved))
           ,@body)))))

(defun lean4--section-at-point (tag)
  "Return the section at point whose value is tagged TAG, or nil.

A section\\='s value is (TAG . REST) where it stands for something the
buffer\\='s commands act on.  Walks out to the enclosing one, point being
usually on the body rather than on the heading."
  (let ((section (magit-current-section)))
    (while (and section
                (not (let ((value (oref section value)))
                       (and (consp value) (eq (car value) tag)))))
      (setq section (oref section parent)))
    section))

;;;; Glyphs

;; Unicode with a fallback, the way `magit-section' picks its own
;; visibility indicators.  Emacs runs in terminals and on machines with no
;; emoji font, so a glyph that is merely likely to work is not good enough
;; on its own.
;;
;; Asked per frame rather than once at load: one Emacs can serve a
;; graphical frame and a terminal at the same time, and the answer is not
;; the same in both.  Deciding at load time would give whichever frame
;; happened to come first.

(defun lean4--char-displayable-p (character)
  "Return non-nil if CHARACTER has a glyph to draw it with here.

`char-displayable-p' is not that question.  On a graphical frame it asks
whether the frame\\='s fontset claims CHARACTER, which it answers by
block rather than by character: it says yes to U+1F6E0 on a machine
whose emoji font stops short of that one, and what the reader gets is a
box with 1F6E0 printed inside it.  Ask instead for the font that would
actually draw the character, which is what `describe-char' reports.

A terminal has no font to ask about, and there `char-displayable-p' --
whether the terminal\\='s coding system can encode the character -- is
the whole of the question."
  (and (char-displayable-p character)
       (or (not (display-graphic-p))
           (and (internal-char-font nil character) t))))

(defun lean4--displayable-p (string)
  "Return non-nil if every character of STRING can be drawn here."
  (seq-every-p #'lean4--char-displayable-p string))

(defun lean4--glyph (configured candidates fallback)
  "Return CONFIGURED, or the first of CANDIDATES this frame can draw.
FALLBACK is used when it can draw none of them."
  (or configured
      (seq-find #'lean4--displayable-p candidates)
      fallback))

;;;; Programs

(defun lean4--program (name)
  "Return how to invoke the Lean toolchain program NAME.

Normally NAME is returned unchanged and resolved through the variable
`exec-path', which is what an elan installation wants: elan's shims read the
project's \"lean-toolchain\" and dispatch to the right version, so
pinning an absolute path defeats it.  `lean4-rootdir' overrides this for
installations that are not on the variable `exec-path'."
  (if lean4-rootdir
      (expand-file-name name (expand-file-name "bin" lean4-rootdir))
    name))

(defconst lean4--dependency-directories '(".lake" ".elan")
  "Directories whose Lean files belong to something else.
`.lake' holds the packages Lake fetched and what it built; `.elan' holds
the toolchains elan installed, Lean's own source among them.  The two VS
Code names in `files.readonlyInclude'.")

(defun lean4--dependency-file-p (&optional file-name)
  "Return non-nil if FILE-NAME is a Lean file belonging to a dependency.
FILE-NAME defaults to the current buffer's.  True of anything under a
`.lake' or `.elan' directory, at any depth: a package vendored under
`.lake/packages' keeps its own sources several levels down."
  (when-let* ((file-name (or file-name buffer-file-name)))
    ;; `file-name-split' rather than splitting on a slash by hand: it is
    ;; what Emacs provides for taking a name apart, and it knows the
    ;; separator this system uses.
    (seq-some (lambda (part)
                (member part lean4--dependency-directories))
              (file-name-split (expand-file-name file-name)))))

(defun lean4-whitespace-cleanup ()
  "Delete trailing whitespace if `lean4-delete-trailing-whitespace' is t."
  (when lean4-delete-trailing-whitespace
    (delete-trailing-whitespace)))

(provide 'lean4-util)
;;; lean4-util.el ends here
