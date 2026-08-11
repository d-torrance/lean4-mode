;;; lean4-outline.el --- Folding Lean 4 by its structure -*- lexical-binding: t -*-

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

;; What `outline-minor-mode' needs in order to fold a Lean file the way VS
;; Code's carets do: a namespace holding sections holding declarations.
;;
;; The structure is not guessed at.  Lean's server answers
;; `textDocument/foldingRange', and for
;;
;;     namespace Outer / section Inner / theorem multi / def withWhere /
;;     structure Point / end Inner / end Outer
;;
;; it returns five nested regions -- the namespace to its `end', the section
;; to its `end', and each declaration that spans more than one line -- and
;; nothing for a single-line declaration or for a block comment.  That is
;; the model implemented here, checked against the server rather than
;; imagined.
;;
;; It is implemented by reading the buffer even so.  Outline derives the end
;; of a fold from the next heading at the same level or higher, which lands
;; in the same place as the server's ranges, so asking would buy nothing but
;; a dependency on having asked -- and folding is something one does to find
;; one's way around a file that will not compile as often as around one that
;; will.  `lean4-defun.el' already knows where a declaration begins; this
;; adds the two levels above it.
;;
;; `end' is a heading, which reads oddly for a moment and is what makes a
;; section fold stop at its own `end' rather than swallowing everything up
;; to the next section.  It is a structural line, and outline has no other
;; way to be told where a level closes.
;;
;; Nothing is turned on here.  `outline-minor-mode' is the reader's to
;; enable; this only makes it right when they do.

;;; Code:

(require 'outline)

(require 'lean4-defun)
(require 'lean4-indent)

(defconst lean4-outline--opener-regexp
  (rx (or "namespace" "section" "mutual") symbol-end)
  "A line opening a block which an `end' closes.
`mutual' is counted with the other two: it is closed the same way, so a
file containing one would otherwise leave every level below it out by
one.")

(defun lean4-outline--at-opener-p ()
  "Return non-nil if this line opens a namespace, section or mutual block."
  (save-excursion
    (back-to-indentation)
    (and (not (lean4-indent--in-string-or-comment-p))
         (looking-at-p lean4-outline--opener-regexp))))

(defun lean4-outline--at-end-p ()
  "Return non-nil if this line closes one."
  (save-excursion
    (back-to-indentation)
    (and (not (lean4-indent--in-string-or-comment-p))
         (looking-at-p lean4-indent--block-end-regexp))))

(defun lean4-outline--at-heading-p ()
  "Return non-nil if this line is an outline heading.
That is a namespace, a section, an `end', or a declaration."
  (or (lean4-outline--at-opener-p)
      (lean4-outline--at-end-p)
      (lean4-defun--at-start-p)))

(defconst lean4-outline--block-regexp
  (rx bol (* space) (group (or "namespace" "section" "mutual" "end"))
      symbol-end)
  "A line which opens a block or closes one.
Anchored and searched for rather than tested line by line: a file has few
of these and many lines, and walking the lines meant asking the syntax
about every one of them, which on a two-thousand-line file took seconds.")

(defvar-local lean4-outline--depths nil
  "Cached block depths, as a cons of a modification tick and an alist.
Each element of the alist is the position at which a block is opened or
closed and the depth in force after it.  Discarded whenever the buffer
changes, which is cheaper than working out what part of it to keep.")

(defun lean4-outline--depths ()
  "Return where the block depth changes, and to what, as an alist.
Built in one pass over the buffer and kept until it is edited."
  (unless (eq (car lean4-outline--depths) (buffer-chars-modified-tick))
    (setq lean4-outline--depths
          (cons (buffer-chars-modified-tick)
                (save-excursion
                  (goto-char (point-min))
                  (let ((depth 0)
                        (changes nil))
                    (while (re-search-forward lean4-outline--block-regexp nil t)
                      (unless (lean4-indent--in-string-or-comment-p
                               (match-beginning 1))
                        (setq depth
                              (if (equal (match-string 1) "end")
                                  (max 0 (1- depth))
                                (1+ depth)))
                        (push (cons (match-beginning 0) depth) changes)))
                    (nreverse changes))))))
  (cdr lean4-outline--depths))

(defun lean4-outline--depth ()
  "Return how many blocks enclose the line point is on.
A block opened or closed on that line itself does not count, so that an
opener sits outside its own block and an `end' inside the one it closes."
  (let ((bol (line-beginning-position))
        (depth 0))
    (dolist (change (lean4-outline--depths))
      (when (< (car change) bol)
        (setq depth (cdr change))))
    depth))

(defun lean4-outline-level ()
  "Return the level of the heading point is at.
A namespace, section or declaration is one deeper than what encloses it.
An `end' takes the level of the block it closes, which is that of the
line above it minus the one it is closing -- so that folding a section
stops there rather than at the next section.

This is the variable `outline-level' for `lean4-mode'."
  (let ((depth (lean4-outline--depth)))
    (if (lean4-outline--at-end-p)
        (max 1 depth)
      (1+ depth))))

(defun lean4-outline--match-line ()
  "Set the match data to the whole of this line and return non-nil.
Outline expects the match of a heading, and uses its bounds to highlight
one; the regexp it would have used matches a line, so this does."
  (save-excursion
    (forward-line 0)
    (looking-at ".*")))

(defun lean4-outline-search (&optional bound move backward looking-at)
  "Search for a Lean outline heading, as `outline-search-function'.
LOOKING-AT asks only whether point is at one.  Otherwise search forward,
or BACKWARD, for the next, no further than BOUND, moving point there and
setting the match data.  MOVE asks to go to the buffer\\='s end rather
than stay put when there is none.

Failure returns nil rather than signalling, whatever MOVE says: outline
calls this in a `while' loop and a signal would end more than the loop."
  (if looking-at
      (and (lean4-outline--at-heading-p) (lean4-outline--match-line))
    (let ((found nil)
          (exhausted nil))
      (save-excursion
        ;; A heading matches at the beginning of its line, so going forward
        ;; the line point is in is eligible exactly while point is still at
        ;; that beginning, as it is for `re-search-forward'.  This is not a
        ;; nicety: `outline-next-preface' emulates the leading "\n" of the
        ;; regexp it would otherwise use by stepping over that newline and
        ;; asking for a heading at point, and it hides a region up to
        ;; whatever comes back -- so a search that skipped the line would
        ;; have it hide the heading itself.  Going back the line is never
        ;; eligible, `re-search-backward' wanting a match which ends before
        ;; point, and no caller wants one either.
        (when (or backward (not (bolp)))
          (setq exhausted (not (zerop (forward-line (if backward -1 1))))))
        (while (not (or found exhausted))
          (if (and (lean4-outline--at-heading-p)
                   (or (null bound)
                       (if backward (>= (point) bound) (<= (point) bound))))
              (setq found (point))
            (setq exhausted (not (zerop (forward-line (if backward -1 1))))))))
      (cond (found (goto-char found)
                   (lean4-outline--match-line)
                   t)
            (t (when move
                 (goto-char (if backward (point-min) (point-max))))
               nil)))))

(defun lean4-outline--setup ()
  "Teach `outline-minor-mode' the structure of a Lean file.
Called from the mode body.  The mode itself is not turned on: which
buffers get an outline is the reader\\='s business, and a major mode
switching on a minor mode they did not ask for is not."
  (setq-local outline-search-function #'lean4-outline-search)
  (setq-local outline-level #'lean4-outline-level)
  ;; Read by nothing here, since `outline-search-function' takes
  ;; precedence, but `outline-minor-mode' and code around it fall back to
  ;; the regexp; better an approximation of the truth than the default,
  ;; which is a Lisp form feed.
  (setq-local outline-regexp
              (rx (* space)
                  (or "namespace" "section" "mutual" "end"
                      "theorem" "lemma" "def" "abbrev" "instance" "example"
                      "structure" "class" "inductive" "opaque" "axiom"
                      "macro" "macro_rules" "syntax" "elab" "elab_rules"
                      "notation" "declare_syntax_cat")
                  symbol-end)))

(provide 'lean4-outline)
;;; lean4-outline.el ends here
