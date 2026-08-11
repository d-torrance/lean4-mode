;;; lean4-suggest-test.el --- Tests for applying Lean's suggestions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; What is offered, in what order, and which of it is the default.  The code
;; action here is the one a real server sent for `example : 1 = 1 := by simp?'
;; -- Lean 4.32.2 titles it "Try this: simp only" -- so the shape is its shape
;; rather than a guess at it.  Asking the server, and Eglot's carrying out of
;; what gets chosen, are exercised in lean4-e2e-test.el.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defconst lean4-suggest-test--action
  '(:title "Try this: simp only" :kind "quickfix"
    :edit (:documentChanges []))
  "A code action as Lean 4.32.2 sent it for a `simp?' suggestion.")

(defconst lean4-suggest-test--hint
  '(:label " {α}"
    :position (:line 0 :character 8)
    :textEdits [(:newText " {α}"
                 :range (:start (:line 0 :character 8)
                         :end (:line 0 :character 8)))])
  "An inlay hint carrying an edit, which is what makes it offerable.")

(defmacro lean4-suggest-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT.
Point starts where a `|' in TEXT marks, and the marker is removed."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-auto-start-server nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     (goto-char (point-min))
     (when (search-forward "|" nil t) (delete-char -1))
     ,@body))

;;;; What is offered

(ert-deftest lean4-suggest-candidates-are-titled-by-the-server ()
  "A code action is offered under the title Lean gave it."
  (should (equal (mapcar #'car (lean4-suggest--candidates
                                (list lean4-suggest-test--action) nil))
                 '("Try this: simp only"))))

(ert-deftest lean4-suggest-candidates-put-the-hint-last ()
  "An action proposes something about the code; the hint only writes down
what Lean already decided, so it comes after."
  (lean4-suggest-test--with-lean "def myId (a : α) : α := a\n"
    (should (equal (mapcar #'car (lean4-suggest--candidates
                                  (list lean4-suggest-test--action)
                                  lean4-suggest-test--hint))
                   '("Try this: simp only" "Insert \" {α}\"")))))

(ert-deftest lean4-suggest-candidates-carry-what-to-do ()
  "Each candidate says which of the two kinds of thing it is."
  (lean4-suggest-test--with-lean "def myId (a : α) : α := a\n"
    (let ((candidates (lean4-suggest--candidates
                       (list lean4-suggest-test--action)
                       lean4-suggest-test--hint)))
      (should (eq (car (cdr (assoc "Try this: simp only" candidates)))
                  'action))
      (should (eq (car (cdr (assoc "Insert \" {α}\"" candidates)))
                  'hint)))))

(ert-deftest lean4-suggest-candidates-of-nothing ()
  "With nothing on offer there is nothing to choose from."
  (should-not (lean4-suggest--candidates nil nil)))

(ert-deftest lean4-suggest-candidates-tolerate-an-untitled-action ()
  "A title is required by LSP, so an action without one is a server bug;
it is still offered rather than dropped or crashed on."
  (should (equal (mapcar #'car (lean4-suggest--candidates
                               (list '(:kind "quickfix")) nil))
                 '("Untitled action"))))

;;;; Which is the default

(ert-deftest lean4-suggest-default-is-the-first ()
  "Failing an opinion from Lean, the order it sent them in stands."
  (should (equal (lean4-suggest--default
                  (lean4-suggest--candidates
                   (list lean4-suggest-test--action
                         '(:title "Import Foo.Bar"))
                   nil))
                 "Try this: simp only")))

(ert-deftest lean4-suggest-default-is-the-preferred-one ()
  "Where Lean marks one preferred, that is the one RET should take."
  (should (equal (lean4-suggest--default
                  (lean4-suggest--candidates
                   (list lean4-suggest-test--action
                         '(:title "Import Foo.Bar" :isPreferred t))
                   nil))
                 "Import Foo.Bar")))

;;;; What is asked about

(ert-deftest lean4-suggest-bounds-are-the-region-when-there-is-one ()
  "An active region is what the reader means to ask about."
  (lean4-suggest-test--with-lean "theorem t : True := by\n  trivial\n"
    (goto-char (point-min))
    ;; `use-region-p' consults `transient-mark-mode', which is not on under
    ;; --batch.
    (let ((transient-mark-mode t))
      (push-mark (point-max) t t)
      (should (equal (lean4-suggest--bounds)
                     (list (point-min) (point-max)))))))

(ert-deftest lean4-suggest-bounds-are-the-term-at-point ()
  "Failing that, the term point is in."
  (lean4-suggest-test--with-lean "theorem t : True := by\n  triv|ial\n"
    (should (equal (lean4-suggest--bounds)
                   (list (save-excursion (back-to-indentation) (point))
                         (line-end-position))))))

(ert-deftest lean4-suggest-bounds-on-a-blank-line-are-point ()
  "And failing that, point itself.
This is the case that matters: the Eglot in Emacs 29 signals an error
here, and a blank line inside a proof is somewhere a reader really does
press the key."
  (lean4-suggest-test--with-lean "theorem t : True := by\n|\n  trivial\n"
    (should (equal (lean4-suggest--bounds) (list (point) (point))))))

;;;; Refusing to act

(ert-deftest lean4-suggest-without-a-server ()
  "The command says so rather than failing obscurely."
  (lean4-suggest-test--with-lean "theorem t : True := by\n  triv|ial\n"
    (should-error (lean4-apply-suggestion) :type 'user-error)))

(provide 'lean4-suggest-test)
;;; lean4-suggest-test.el ends here
