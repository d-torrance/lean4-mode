;;; lean4-hints-test.el --- Tests for Lean inlay hints  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Which hint a command acts on, and what ElDoc is told about it.  The hints
;; here are the ones a real server sent, captured from Lean 4.32.2 for
;;
;;     def myId (a : α) : α := a
;;
;; so the shapes are its shapes rather than a guess at them.  The request
;; itself is exercised end-to-end, in lean4-e2e-test.el.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lean4-mode)

(defconst lean4-hints-test--hint
  '(:kind 2 :label " {α}"
    :paddingLeft :json-false :paddingRight :json-false
    :position (:line 0 :character 8)
    :textEdits [(:newText " {α}"
                 :range (:start (:line 0 :character 8)
                         :end (:line 0 :character 8)))]
    :tooltip (:kind "markdown"
              :value "Automatically-inserted implicit parameters:\n\
```lean\nα : Sort u_1\n```"))
  "A hint as Lean 4.32.2 sent it for `def myId (a : α) : α := a'.")

(defmacro lean4-hints-test--with-lean (text &rest body)
  "Evaluate BODY in a `lean4-mode' buffer containing TEXT.
Point starts where a `|' in TEXT marks, and the marker is removed.  No
server is started: everything here is about the answer, not the asking."
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

(defun lean4-hints-test--at (line character)
  "Return a copy of the captured hint drawn at LINE and CHARACTER."
  (let ((hint (copy-sequence lean4-hints-test--hint)))
    (plist-put hint :position (list :line line :character character))))

;;;; Reading a hint

(ert-deftest lean4-hints-label-of-a-string ()
  "A label which is a string is itself."
  (should (equal (lean4-hints--label lean4-hints-test--hint) " {α}")))

(ert-deftest lean4-hints-label-of-parts ()
  "A label in parts is the parts joined, LSP allowing either."
  (should (equal (lean4-hints--label
                  '(:label [(:value " {") (:value "α") (:value "}")]))
                 " {α}")))

(ert-deftest lean4-hints-insertion-is-the-edits ()
  "What a hint inserts is the edits it carries."
  (should (lean4-hints--insertion lean4-hints-test--hint))
  (should-not (lean4-hints--insertion '(:label " {α}")))
  (should-not (lean4-hints--insertion '(:label " {α}" :textEdits []))))

;;;; Choosing a hint

(ert-deftest lean4-hints-nearest-ignores-other-lines ()
  "A hint on another line is not the hint at point."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\ndef f := 0|\n"
    (should-not (lean4-hints--nearest (vector (lean4-hints-test--at 0 8))
                                     (point)))))

(ert-deftest lean4-hints-nearest-finds-the-one-on-this-line ()
  "A hint on this line is found from anywhere on it."
  (lean4-hints-test--with-lean "def myId (a : α) : α := |a\n"
    (should (equal (lean4-hints--nearest (vector (lean4-hints-test--at 0 8))
                                         (point))
                   (lean4-hints-test--at 0 8)))))

(ert-deftest lean4-hints-nearest-prefers-the-closer ()
  "With two on a line, the nearer to point wins."
  (lean4-hints-test--with-lean "def f (a : α) (b : β) :| Nat := 0\n"
    (let* ((near (lean4-hints-test--at 0 22))
           (far (lean4-hints-test--at 0 5))
           (hints (vector far near)))
      (should (equal (lean4-hints--nearest hints (point)) near)))))

(ert-deftest lean4-hints-nearest-breaks-a-tie-with-the-earlier ()
  "Equally distant, the earlier hint wins, so the choice is not arbitrary."
  (lean4-hints-test--with-lean "def f (ab) : Nat := 0\n"
    (goto-char (+ (point-min) 8))
    (let* ((before (lean4-hints-test--at 0 7))
           (after (lean4-hints-test--at 0 9))
           (hints (vector before after)))
      (should (equal (lean4-hints--nearest hints (point)) before)))))

;;;; What ElDoc is told

(ert-deftest lean4-hints-documentation-reports-the-tooltip ()
  "The type Lean inferred is what the report is for."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\n"
    (let ((documentation (lean4-hints--documentation
                          lean4-hints-test--hint)))
      (should (string-search "α : Sort u_1" documentation))
      (should (string-search "Automatically-inserted implicit parameters:"
                             documentation)))))

(ert-deftest lean4-hints-documentation-names-the-key ()
  "And says how to write it down, since Emacs has no hint to double-click.
The key named is `lean4-apply-suggestion''s, that being the one bound and
the one worth teaching: it reaches everything else Lean offers as well."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\n"
    (should (string-search "C-c C-."
                           (lean4-hints--documentation
                            lean4-hints-test--hint)))
    (should (string-search "\" {α}\""
                           (lean4-hints--documentation
                            lean4-hints-test--hint)))))

(ert-deftest lean4-hints-documentation-names-the-command-without-a-key ()
  "Where no key is bound the command's own name is given instead.
`substitute-command-keys' does this, which is why the announcement stays
true whatever the reader has bound."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\n"
    (use-local-map (make-sparse-keymap))
    (should (string-search "lean4-apply-suggestion"
                           (lean4-hints--documentation
                            lean4-hints-test--hint)))))

(ert-deftest lean4-hints-documentation-without-a-tooltip ()
  "A hint with nothing to say still says how to insert itself."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\n"
    (let ((documentation (lean4-hints--documentation
                          '(:label " {α}"
                            :textEdits [(:newText " {α}"
                                         :range (:start (:line 0 :character 8)
                                                 :end (:line 0
                                                       :character 8)))]))))
      (should (string-search "C-c C-." documentation)))))

(ert-deftest lean4-hints-documentation-of-nothing-is-nothing ()
  "A hint with neither tooltip nor edit is not worth a line in the echo area."
  (lean4-hints-test--with-lean "def myId (a : α) : α := a\n"
    (should-not (lean4-hints--documentation '(:label " {α}")))))

;;;; Refusing to act

(ert-deftest lean4-hints-insert-without-a-server ()
  "The command says so rather than failing obscurely."
  (lean4-hints-test--with-lean "def myId (a : α) : α := |a\n"
    (should-error (lean4-insert-inlay-hint) :type 'user-error)))

;;;; Answering ElDoc, even with nothing

(defmacro lean4-hints-test--answering (hints &rest body)
  "Evaluate BODY with a server pretended and HINTS its answer.
What the ElDoc function reported is left in `reported', which stays
`never' if it reported nothing at all -- which is the bug this guards
against, `eldoc-documentation-compose' showing nothing until every
function has answered."
  (declare (indent 1) (debug (form body)))
  `(let ((reported 'never))
     (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
               ((symbol-function 'lean4--server-capable) (lambda (_) t))
               ;; The real one needs a file name, and these buffers visit
               ;; nothing; what is in the request does not matter here.
               ((symbol-function 'lean4-hints--params) (lambda () nil))
               ((symbol-function 'jsonrpc-async-request)
                (lambda (_server _method _params &rest args)
                  (funcall (plist-get args :success-fn) ,hints))))
       ,@body)))

(ert-deftest lean4-hints-eldoc-reports-a-hint ()
  "Where there is a hint, that is what is reported."
  (lean4-hints-test--with-lean "def myId (a : α) : α := |a\n"
    (lean4-hints-test--answering (vector lean4-hints-test--hint)
      (should (lean4-hints-eldoc-function
               (lambda (&rest args) (setq reported args))))
      (should (string-search "α : Sort u_1" (car reported))))))

(ert-deftest lean4-hints-eldoc-reports-nothing-rather-than-not-answering ()
  "Where there is no hint, ElDoc is told so rather than left waiting."
  (lean4-hints-test--with-lean "theorem t : True := tri|vial\n"
    (lean4-hints-test--answering []
      (should (lean4-hints-eldoc-function
               (lambda (&rest args) (setq reported args))))
      (should (equal reported '(nil))))))

(ert-deftest lean4-hints-eldoc-answers-when-the-request-fails ()
  "A rejected request is still an answer: the server routinely refuses a
position it is still elaborating, and ElDoc must not wait for that."
  (lean4-hints-test--with-lean "theorem t : True := tri|vial\n"
    (let ((reported 'never))
      (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'lean4--server-capable) (lambda (_) t))
                ((symbol-function 'lean4-hints--params) (lambda () nil))
                ((symbol-function 'jsonrpc-async-request)
                 (lambda (_server _method _params &rest args)
                   (funcall (plist-get args :error-fn) '(:message "busy")))))
        (should (lean4-hints-eldoc-function
                 (lambda (&rest args) (setq reported args)))))
      (should (equal reported '(nil))))))

(ert-deftest lean4-hints-eldoc-answers-when-the-request-times-out ()
  "So is a request that never comes back."
  (lean4-hints-test--with-lean "theorem t : True := tri|vial\n"
    (let ((reported 'never))
      (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'lean4--server-capable) (lambda (_) t))
                ((symbol-function 'lean4-hints--params) (lambda () nil))
                ((symbol-function 'jsonrpc-async-request)
                 (lambda (_server _method _params &rest args)
                   (funcall (plist-get args :timeout-fn)))))
        (should (lean4-hints-eldoc-function
                 (lambda (&rest args) (setq reported args)))))
      (should (equal reported '(nil))))))

(ert-deftest lean4-hints-eldoc-is-silent-without-a-server ()
  "And the ElDoc function declines, leaving the others to answer."
  (lean4-hints-test--with-lean "def myId (a : α) : α := |a\n"
    (should-not (lean4-hints-eldoc-function #'ignore))))

(provide 'lean4-hints-test)
;;; lean4-hints-test.el ends here
