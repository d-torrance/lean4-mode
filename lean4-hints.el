;;; lean4-hints.el --- Inlay hints for Lean 4 -*- lexical-binding: t -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Write down what Lean inferred, and say what it inferred it to be.
;;
;; Given a declaration whose type mentions a variable never bound,
;;
;;     def myId (a : α) : α := a
;;
;; Lean binds α for you and reports having done so as an inlay hint: a ` {α}'
;; the editor draws after the name without putting it in the file.  Eglot
;; draws it, and drops the two things the server attached to it -- the edit
;; that would make it real, and the tooltip saying what α was inferred to be.
;; Both are read here instead.
;;
;; `lean4-insert-inlay-hint' is the edit, which is what VS Code's double-click
;; on the hint does.  The tooltip goes through ElDoc, which is where the rest
;; of what this package knows about the text under point already goes, and
;; which Eglot has already set to `eldoc-documentation-compose' -- so this
;; joins the hover and signature reports rather than displacing them.
;;
;; ElDoc also names the command, through `substitute-command-keys', so the
;; report says how to act on what it just described.  That is Eglot's own
;; idiom for the same problem: `eglot-code-action-suggestion' announces a code
;; action the same way, since Emacs has no lightbulb to click and no
;; double-click to offer.  It also means this needs no key of its own -- the
;; announcement reads `M-x lean4-insert-inlay-hint' until someone binds one,
;; and then reads the key.
;;
;; Auto-bound implicits are the only hint Lean sends as of 4.33, and Mathlib
;; sets `autoImplicit' false, so this has nothing to say in a Mathlib file.
;; Nothing here is specific to that hint, though: it reads whatever the server
;; sends.

;;; Code:

(require 'eglot)
(require 'jsonrpc)
(require 'seq)

(require 'lean4-eglot)

(defcustom lean4-inlay-hints-in-eldoc t
  "Whether to report the inlay hint on the current line through ElDoc.
The report says what Lean inferred and how to write it down.  Turning
this off stops the request that fetches it, which ElDoc would otherwise
make every time point comes to rest."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-inlay-hint-timeout 2
  "Seconds to wait for the server when inserting an inlay hint.
Only `lean4-insert-inlay-hint' waits: the ElDoc report is asynchronous
and simply says nothing if the answer is slow."
  :group 'lean4
  :type 'number)

(defun lean4-hints--available-p ()
  "Return non-nil if this buffer's server offers inlay hints."
  (and (eglot-current-server)
       (lean4--server-capable :inlayHintProvider)))

(defun lean4-hints--params ()
  "Return the `textDocument/inlayHint' parameters for the current line.
A line rather than the window: this is asked in order to say something
about where point is, and Eglot is already asking for the window."
  (list :textDocument (eglot--TextDocumentIdentifier)
        :range (list :start (eglot--pos-to-lsp-position
                             (line-beginning-position))
                     :end (eglot--pos-to-lsp-position
                           (line-end-position)))))

(defun lean4-hints--position (hint)
  "Return the buffer position HINT is drawn at."
  (eglot--lsp-position-to-point (plist-get hint :position)))

(defun lean4-hints--label (hint)
  "Return HINT\\='s label as a string.
LSP allows either a string or a list of parts, and Lean has sent both:
the parts carry a location each, for a label whose pieces link
somewhere."
  (let ((label (plist-get hint :label)))
    (if (stringp label)
        label
      (mapconcat (lambda (part) (or (plist-get part :value) "")) label ""))))

(defun lean4-hints--nearest (hints position)
  "Return the hint of HINTS nearest POSITION on its line, or nil.
Nearest, because a hint is drawn where Lean would write the thing it is
offering -- after the declaration\\='s name -- which is not where the
reader is standing when they ask for it.  A tie goes to the earlier
hint."
  (let ((from (save-excursion (goto-char position) (line-beginning-position)))
        (to (save-excursion (goto-char position) (line-end-position)))
        (best nil)
        (best-distance nil))
    (seq-doseq (hint hints)
      (let ((where (lean4-hints--position hint)))
        (when (and (<= from where) (<= where to))
          (let ((distance (abs (- where position))))
            (when (or (null best-distance) (< distance best-distance))
              (setq best hint
                    best-distance distance))))))
    best))

(defun lean4-hints--insertion (hint)
  "Return the text edits which write HINT into the buffer, or nil."
  (let ((edits (plist-get hint :textEdits)))
    (and edits (not (seq-empty-p edits)) edits)))

(defun lean4-hints--announcement (hint)
  "Return the line saying how to insert HINT, or nil if it cannot be.
`substitute-command-keys' names a key where one is bound and names the
command itself where none is, so this stays true whether or not the
reader has bound one."
  (when (lean4-hints--insertion hint)
    ;; `lean4-apply-suggestion' rather than `lean4-insert-inlay-hint': it is
    ;; the one with a key, and the key is worth teaching, since it reaches
    ;; everything else Lean offers to write as well.  It offers rather than
    ;; inserts, hence the wording.  Quoted and untrimmed: the label begins
    ;; with the space Lean means to insert, and what is shown in quotes
    ;; should be what lands in the file.
    (substitute-command-keys
     (format "\\[lean4-apply-suggestion] offers to insert %S"
             (lean4-hints--label hint)))))

(defun lean4-hints--documentation (hint)
  "Return what ElDoc should say about HINT, or nil if there is nothing.
The tooltip is rendered by the same Eglot function that renders a hover,
so a docstring in a hint reads as one anywhere else does -- fontified
Lean inside its code fences, and with `markdown-mode' installed, as
markdown."
  (let* ((tooltip (plist-get hint :tooltip))
         (rendered (and tooltip (eglot--format-markup tooltip)))
         (announcement (lean4-hints--announcement hint))
         (parts (delq nil (list (and rendered (not (string-empty-p rendered))
                                    rendered)
                                announcement))))
    (when parts (string-join parts "\n"))))

(defun lean4-hints-eldoc-function (callback &rest _ignored)
  "Report the inlay hint nearest point, and how to write it down.
CALLBACK is called with the report, this being a member of
`eldoc-documentation-functions'.  The request is deferred, so that
moving through a file coalesces into one."
  (when (and lean4-inlay-hints-in-eldoc (lean4-hints--available-p))
    (let ((buffer (current-buffer))
          (origin (point)))
      (jsonrpc-async-request
       (eglot-current-server) :textDocument/inlayHint (lean4-hints--params)
       :success-fn
       (lambda (hints)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             ;; Point has moved on: whatever this says is about somewhere
             ;; else, and ElDoc has already asked again about here.
             (when (eq origin (point))
               (when-let* ((hint (lean4-hints--nearest hints origin))
                           (documentation (lean4-hints--documentation hint)))
                 (funcall callback documentation))))))
       ;; The server rejects position requests for a region it is still
       ;; elaborating, which is routine and not worth reporting.
       :error-fn #'ignore
       :timeout-fn #'ignore
       :deferred 'lean4-hints-eldoc-function)
      ;; Non-nil and not a string: ElDoc waits for the callback.
      t)))

(defun lean4-hints--insert (hint)
  "Apply the text edits HINT carries, leaving point where it was.
The hint says what to insert and where, so both are Lean\\='s rather than
this package\\='s guess at them."
  (save-excursion (eglot--apply-text-edits (lean4-hints--insertion hint)))
  (message "Inserted %s" (string-trim (lean4-hints--label hint))))

(defun lean4-insert-inlay-hint ()
  "Write the inlay hint nearest point into the buffer.
Lean draws a hint where it silently inserted something -- the implicit
binders of a declaration whose type mentions a variable never bound, so
that `def myId (a : α) : α := a' is drawn with a ` {α}' after its name.
This puts that text in the file, which is what VS Code\\='s double-click
on the hint does, leaving point where it was.

The hint says what to insert and where, so the text and its position are
Lean\\='s rather than this package\\='s guess at them."
  (interactive)
  (unless (lean4-hints--available-p)
    (user-error "No Lean server offering inlay hints for this buffer"))
  ;; Flush pending edits first: the positions in the answer are for the text
  ;; the server has, and Eglot sends changes on an idle timer.
  (eglot--signal-textDocument/didChange)
  (let* ((hints (jsonrpc-request (eglot-current-server)
                                 :textDocument/inlayHint
                                 (lean4-hints--params)
                                 :timeout lean4-inlay-hint-timeout))
         (hint (lean4-hints--nearest hints (point)))
         (edits (and hint (lean4-hints--insertion hint))))
    (cond ((null hint) (user-error "No inlay hint on this line"))
          ((null edits) (user-error "This inlay hint has nothing to insert"))
          (t (lean4-hints--insert hint)))))

(defun lean4-hints--setup ()
  "Report inlay hints through ElDoc where the server offers them.
Run from `eglot-managed-mode-hook', which is also where Eglot installs
its own ElDoc functions -- and where it leaves
`eldoc-documentation-strategy' set to `eldoc-documentation-compose', so
that this is shown alongside them rather than instead of them.  Appended
for the same reason: what Lean inferred is a footnote to the type of the
thing under point, not a replacement for it."
  (if (and lean4-inlay-hints-in-eldoc (lean4-hints--available-p))
      (add-hook 'eldoc-documentation-functions
                #'lean4-hints-eldoc-function t t)
    (remove-hook 'eldoc-documentation-functions
                 #'lean4-hints-eldoc-function t)))

(provide 'lean4-hints)
;;; lean4-hints.el ends here
