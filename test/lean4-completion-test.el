;;; lean4-completion-test.el --- Tests for completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Completion in a Lean buffer comes from three sources, in order: the
;; server through Eglot, the Unicode abbreviations, and Lean's keywords.
;; What makes them cooperate is that none of them is exclusive, which is
;; the property most of these tests are really about.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defmacro lean4-completion-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT, point at the end."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-auto-start-server nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     (insert ,text)
     ,@body))

;;;; Keywords

(ert-deftest lean4-completion-keyword-completes-a-prefix ()
  "A keyword prefix completes to the keyword."
  (lean4-completion-test--with-lean "exam"
    (should (equal (all-completions
                    "exam" (nth 2 (lean4-keyword-completion-at-point)))
                   '("example")))))

(ert-deftest lean4-completion-keyword-declines-a-non-keyword ()
  "A prefix matching no keyword offers nothing, rather than nothing useful."
  (lean4-completion-test--with-lean "zzzq"
    (should-not (all-completions
                 "zzzq" (nth 2 (lean4-keyword-completion-at-point))))))

(ert-deftest lean4-completion-keyword-is-never-exclusive ()
  "The keyword source always lets other sources have a turn."
  (lean4-completion-test--with-lean "exam"
    (should (eq (plist-get (nthcdr 3 (lean4-keyword-completion-at-point))
                           :exclusive)
                'no))))

(ert-deftest lean4-completion-keyword-ignores-mid-word-point ()
  "Completing from inside an existing word is noise, so it is declined."
  (lean4-completion-test--with-lean "example"
    (goto-char (+ (point-min) 3))
    (should-not (lean4-keyword-completion-at-point))))

(ert-deftest lean4-completion-keyword-completes-in-a-buffer ()
  "End to end, with no server: the word is actually completed."
  (lean4-completion-test--with-lean "exam"
    (completion-at-point)
    (should (equal (buffer-string) "example"))))

;;;; Cooperation between sources

(ert-deftest lean4-completion-falls-through-an-empty-server-reply ()
  "A server that offers nothing does not silence the other sources.

Regression test, and the reason `lean4-eglot-completion-at-point'
exists.  Eglot's own entry sets no `:exclusive', which makes it
exclusive: it claims the word, offers nothing for it, and completion
stops.  Lean returns nothing for a bare word at the top level -- its
completion is contextual -- so that is not a rare case, it is what
happens every time somebody types the start of a declaration."
  (lean4-completion-test--with-lean "exam"
    ;; Shaped exactly like Eglot's reply: covers the word, matches nothing.
    (add-hook 'completion-at-point-functions
              (lambda () (list (- (point) 4) (point) (list "no-match")))
              nil t)
    (completion-at-point)
    (should (equal (buffer-string) "exam")))
  ;; The same reply, made non-exclusive, reaches the keywords.
  (lean4-completion-test--with-lean "exam"
    (add-hook 'completion-at-point-functions
              (lambda ()
                (list (- (point) 4) (point) (list "no-match") :exclusive 'no))
              nil t)
    (completion-at-point)
    (should (equal (buffer-string) "example"))))

(ert-deftest lean4-completion-wrapper-marks-eglot-non-exclusive ()
  "The wrapper adds `:exclusive no' to whatever Eglot returned."
  (cl-letf (((symbol-function 'eglot-completion-at-point)
             (lambda () (list 1 2 nil))))
    (should (eq (plist-get (nthcdr 3 (lean4-eglot-completion-at-point))
                           :exclusive)
                'no)))
  ;; And leaves an existing setting alone.
  (cl-letf (((symbol-function 'eglot-completion-at-point)
             (lambda () (list 1 2 nil :exclusive t))))
    (should (eq (plist-get (nthcdr 3 (lean4-eglot-completion-at-point))
                           :exclusive)
                t)))
  ;; Nothing in, nothing out.
  (cl-letf (((symbol-function 'eglot-completion-at-point) #'ignore))
    (should-not (lean4-eglot-completion-at-point))))

(ert-deftest lean4-completion-abbreviations-and-keywords-coexist ()
  "A backslash prefix goes to the abbreviations, a word to the keywords."
  (lean4-completion-test--with-lean "\\alph"
    (should (lean4-input-completion-at-point))
    (should-not (lean4-keyword-completion-at-point)))
  (lean4-completion-test--with-lean "exam"
    (should-not (lean4-input-completion-at-point))
    (should (lean4-keyword-completion-at-point))))

(provide 'lean4-completion-test)
;;; lean4-completion-test.el ends here
