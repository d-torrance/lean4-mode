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

(require 'cl-lib)
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

;;;; Saying the same thing as Eglot

(ert-deftest lean4-suggest-eglot-announces-the-same-key ()
  "Eglot's report of an available code action names the key that applies it.
Eglot writes the key of `eglot-code-actions', which is bound to nothing,
so without the remapping in `lean4-mode-map' its report would name that
command while ours named the key -- two ways of saying one thing."
  (lean4-suggest-test--with-lean "theorem t : True := tri|vial\n"
    (should (equal (substring-no-properties
                    (substitute-command-keys "\\[eglot-code-actions]"))
                   "C-c C-."))
    (should (equal (substring-no-properties
                    (substitute-command-keys "\\[eglot-code-actions]"))
                   (substring-no-properties
                    (substitute-command-keys
                     "\\[lean4-apply-suggestion]"))))))

(ert-deftest lean4-suggest-eglot-code-actions-are-remapped ()
  "Eglot's command reaches ours, by key and by mouse alike.
The mouse matters because Eglot puts `eglot-diagnostics-map' both on the
indicator it draws where an action is available and on the errors
themselves, so a click there would otherwise offer the code actions
without the inlay hint the same key offers."
  (lean4-suggest-test--with-lean "theorem t : True := tri|vial\n"
    (should (eq (command-remapping 'eglot-code-actions)
                #'lean4-apply-suggestion))
    (should (eq (command-remapping 'eglot-code-actions-at-mouse)
                #'lean4-apply-suggestion-at-mouse))))

(ert-deftest lean4-suggest-eglot-follows-a-rebinding ()
  "Rebinding the command moves both reports, the remapping being to it."
  (lean4-suggest-test--with-lean "theorem t : True := tri|vial\n"
    (let ((map (copy-keymap lean4-mode-map)))
      (keymap-unset map "C-c C-.")
      (keymap-set map "C-c C-y" #'lean4-apply-suggestion)
      (use-local-map map)
      (should (equal (substring-no-properties
                      (substitute-command-keys "\\[eglot-code-actions]"))
                     "C-c C-y")))))

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

;;;; Asking, or not

(defmacro lean4-suggest-test--offering (actions hint &rest body)
  "Evaluate BODY with ACTIONS and HINT on offer and a server pretended.
`completing-read' records what it was asked in `asked' and answers with
the last candidate; what gets carried out is recorded in `applied'."
  (declare (indent 2) (debug (form form body)))
  `(let (asked applied)
     (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
               ((symbol-function 'eglot--signal-textDocument/didChange)
                #'ignore)
               ((symbol-function 'lean4-suggest--code-actions)
                (lambda () ,actions))
               ((symbol-function 'lean4-suggest--hint) (lambda () ,hint))
               ((symbol-function 'lean4-suggest--execute)
                (lambda (action) (setq applied action)))
               ((symbol-function 'lean4-hints--insert)
                (lambda (hint) (setq applied hint)))
               ((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  (setq asked prompt)
                  (car (car (last collection))))))
       ,@body)))

(ert-deftest lean4-suggest-does-not-ask-about-one-suggestion ()
  "A single suggestion is applied without a prompt offering one answer."
  (lean4-suggest-test--offering (list lean4-suggest-test--action) nil
    (lean4-apply-suggestion)
    (should-not asked)
    (should (equal applied lean4-suggest-test--action))))

(ert-deftest lean4-suggest-says-what-it-applied-unasked ()
  "And says which one, there being no prompt to have shown it."
  (lean4-suggest-test--offering (list lean4-suggest-test--action) nil
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (setq said (apply #'format format arguments)))))
        (lean4-apply-suggestion))
      (should (equal said "Try this: simp only")))))

(ert-deftest lean4-suggest-asks-about-two ()
  "With a choice to make, it is made by the reader."
  (lean4-suggest-test--offering
      (list lean4-suggest-test--action '(:title "Import Foo.Bar")) nil
    (lean4-apply-suggestion)
    (should asked)
    (should (equal applied '(:title "Import Foo.Bar")))))

(ert-deftest lean4-suggest-counts-the-hint-as-a-suggestion ()
  "An action and a hint are two things, so they are asked about."
  (lean4-suggest-test--with-lean "def myId (a : α) : α := a\n"
    (lean4-suggest-test--offering (list lean4-suggest-test--action)
        lean4-suggest-test--hint
      (lean4-apply-suggestion)
      (should asked)
      (should (equal applied lean4-suggest-test--hint)))))

(ert-deftest lean4-suggest-says-so-when-there-is-nothing ()
  "With nothing on offer, nothing is applied and nothing is asked."
  (lean4-suggest-test--offering nil nil
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (setq said (apply #'format format arguments)))))
        (lean4-apply-suggestion))
      (should (equal said "Lean suggests nothing here")))
    (should-not asked)
    (should-not applied)))

;;;; Refusing to act

(ert-deftest lean4-suggest-without-a-server ()
  "The command says so rather than failing obscurely."
  (lean4-suggest-test--with-lean "theorem t : True := by\n  triv|ial\n"
    (should-error (lean4-apply-suggestion) :type 'user-error)))

(provide 'lean4-suggest-test)
;;; lean4-suggest-test.el ends here
