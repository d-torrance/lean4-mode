;;; lean4-suggest.el --- Applying what Lean suggests -*- lexical-binding: t -*-

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

;; One key for everything Lean offers to write for you.
;;
;; Lean suggests edits in two quite different ways, and until now neither had
;; a home here.  Most arrive as LSP code actions: the `Try this' of `simp?',
;; `exact?', `apply?' and `rw?', the import or the spelling correction offered
;; for an unknown identifier, the replacement `#guard_msgs' wants.  One
;; arrives as an inlay hint instead -- the implicit binders Lean bound for a
;; declaration that never declared them.  Both are the same thing to read:
;; Lean worked something out and is offering to put it in the file.
;;
;; `lean4-apply-suggestion' asks for both and offers them in one list, which
;; is what VS Code's `Ctrl+.' does for the code actions and what its
;; double-click on a hint does for the other.  Executing a code action is
;; handed back to Eglot, which resolves it, applies its workspace edit and
;; runs its command; nothing about that is Lean's business or ours.
;;
;; Code actions come first and the hint last, since an action is something
;; Lean is proposing about the code as written and the hint only writes down
;; what it already decided.

;;; Code:

(require 'eglot)
(require 'flymake)
(require 'seq)

(require 'lean4-eglot)
(require 'lean4-hints)

(defun lean4-suggest--bounds ()
  "Return the span to ask about, as a list of two positions.
The active region, if there is one.  Failing that the span of any
diagnostic covering point, so that a fix offered for an error is found
from anywhere within it rather than only where the squiggle starts.
Failing that the term at point, and failing that point itself.

Eglot works this out too, but under a name that changed between the
Eglot in Emacs 29 and the current one -- and the older of the two
signals an error where there is no term at point, which in a Lean buffer
is any blank line inside a proof."
  (cond
   ((use-region-p) (list (region-beginning) (region-end)))
   ((flymake-diagnostics (point))
    (let ((beg (point))
          (end (point)))
      (dolist (diagnostic (flymake-diagnostics (point)))
        (setq beg (min beg (flymake-diagnostic-beg diagnostic))
              end (max end (flymake-diagnostic-end diagnostic))))
      (list beg end)))
   ((bounds-of-thing-at-point 'sexp)
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (list (car bounds) (cdr bounds))))
   (t (list (point) (point)))))

(defun lean4-suggest--code-actions ()
  "Return the code actions Lean offers over `lean4-suggest--bounds'.
Fetched through `eglot-code-actions' with its INTERACTIVE argument nil,
which is Eglot\\='s own way of asking for the list without offering it."
  (when (lean4--server-capable :codeActionProvider)
    (seq-into (apply #'eglot-code-actions
                     (append (lean4-suggest--bounds) '(nil nil)))
              'list)))

(defun lean4-suggest--hint ()
  "Return the inlay hint nearest point, if it can be inserted."
  (when (lean4-hints--available-p)
    (let ((hint (lean4-hints--nearest
                 (jsonrpc-request (eglot-current-server)
                                  :textDocument/inlayHint
                                  (lean4-hints--params)
                                  :timeout lean4-inlay-hint-timeout)
                 (point))))
      (and hint (lean4-hints--insertion hint) hint))))

(defun lean4-suggest--candidates (actions hint)
  "Return an alist of what to offer, from ACTIONS and HINT.
Each element is a title and what to do about it: `(action . ACTION)' or
`(hint . HINT)'."
  (append
   (mapcar (lambda (action)
             (cons (or (plist-get action :title) "Untitled action")
                   (cons 'action action)))
           actions)
   (when hint
     (list (cons (format "Insert %S" (lean4-hints--label hint))
                 (cons 'hint hint))))))

(defun lean4-suggest--default (candidates)
  "Return the title to offer as the default among CANDIDATES.
Lean marks one action preferred where it has an opinion, as LSP provides
for; failing that the first, the server having sent them in the order it
thinks best."
  (car (or (seq-find (lambda (candidate)
                       (let ((what (cdr candidate)))
                         (and (eq (car what) 'action)
                              (plist-get (cdr what) :isPreferred))))
                     candidates)
           (car candidates))))

(defun lean4-suggest--execute (action)
  "Carry out the code ACTION through Eglot.
Eglot resolves the action, applies its workspace edit and runs its
command, which is work there is no reason to do again here.

Its helper prompts unless told a kind, so the action\\='s own kind is
passed in order to say that the choice has already been made: this is
called with the one action the reader picked."
  (eglot--read-execute-code-action
   (list action) (eglot-current-server)
   (or (plist-get action :kind) t)))

(defun lean4-suggest--apply (what)
  "Carry out WHAT, one of the two things a candidate can be."
  (pcase what
    (`(action . ,action) (lean4-suggest--execute action))
    (`(hint . ,hint) (lean4-hints--insert hint))))

(defun lean4-apply-suggestion ()
  "Apply something Lean suggests at point, choosing from what it offers.
That is every code action here -- the `Try this' of `simp?', `exact?',
`apply?' and `rw?', an import or a spelling offered for an unknown
identifier, the output `#guard_msgs' wants -- and the inlay hint, which
is the implicit binders Lean bound for a declaration that did not
declare them.

VS Code offers the first group behind `Ctrl+.' and the second by
double-clicking the hint.  Both are here, in one list.

Where there is only one suggestion it is applied without asking, and
named in the echo area afterwards; VS Code shows its menu either way,
but a prompt offering a single answer is a keystroke spent on nothing.

With an active region, the code actions are asked for over the region;
otherwise over any diagnostic covering point, the term at point, or
point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Lean server for this buffer"))
  ;; The positions in either answer are for the text the server has, and
  ;; Eglot sends changes on an idle timer.
  (eglot--signal-textDocument/didChange)
  (let* ((candidates (lean4-suggest--candidates (lean4-suggest--code-actions)
                                                (lean4-suggest--hint)))
         (default (lean4-suggest--default candidates)))
    (cond
     ;; Not an error: a key pressed hopefully and answered honestly.
     ((null candidates) (message "Lean suggests nothing here"))
     ;; Nothing to choose between, so nothing to ask.  What was done is said
     ;; instead, since with no prompt there is otherwise nothing to show that
     ;; the buffer changed and why.
     ((null (cdr candidates))
      (message "%s" (car (car candidates)))
      (lean4-suggest--apply (cdr (car candidates))))
     (t
      (let ((title (completing-read
                    (format-prompt "Apply which suggestion" default)
                    candidates nil t nil nil default)))
        (lean4-suggest--apply (cdr (assoc title candidates))))))))

(defun lean4-apply-suggestion-at-mouse (event)
  "Apply something Lean suggests where EVENT points.
`lean4-apply-suggestion' acts at point; this acts where the mouse was
clicked, which is what Eglot\\='s clickable things expect of the command
they run -- the indicator it draws in the fringe or the margin where a
code action is available, and the errors and warnings themselves, all of
which carry `mouse-2' and a margin `mouse-1'.

Written out rather than made with Eglot\\='s own `eglot--mouse-call',
which does the same in the same way: eight lines are cheaper than another
private function to keep watch over."
  (interactive "e")
  (let ((start (event-start event)))
    (with-selected-window (posn-window start)
      (save-excursion
        (goto-char (or (posn-point start) (point)))
        (call-interactively #'lean4-apply-suggestion)))))

(provide 'lean4-suggest)
;;; lean4-suggest.el ends here
