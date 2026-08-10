;;; lean4-loogle-test.el --- Tests for the Loogle client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; No network: the responses below are what loogle.lean-lang.org actually
;; sent, captured from its JSON endpoint, and the drawing is checked against
;; them.  Only `lean4-loogle--fetch' talks to the service, and it is the one
;; thing not exercised here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-loogle)

;;;; Fixtures

(defconst lean4-loogle-test--found
  '(:header "Found one declaration mentioning Nat.succ_le_succ."
    :count 1
    :hits ((:name "Nat.succ_le_succ"
            :module "Init.Prelude"
            :type "{n m : ℕ} : n ≤ m → n.succ ≤ m.succ"
            :doc "")))
  "A successful name search, as Loogle answered it.")

(defconst lean4-loogle-test--suggested
  '(:error "unknown identifier 'Nat.succ_le_suc'"
    :suggestions ("\"Nat.succ_le_suc\""))
  "A misspelled name: an error and a suggestion, and no hits.")

(defmacro lean4-loogle-test--with-render (query response &rest body)
  "Draw RESPONSE to QUERY and evaluate BODY in the results buffer.
Inside BODY the buffer is current and `text' is its contents without
properties."
  (declare (indent 2) (debug (form form body)))
  `(unwind-protect
       (progn
         (lean4-loogle--render ,query ,response)
         (with-current-buffer lean4-loogle-buffer-name
           (let ((text (substring-no-properties (buffer-string))))
             ,@body)))
     (when (get-buffer lean4-loogle-buffer-name)
       (kill-buffer lean4-loogle-buffer-name))))

;;;; Building the requests

(ert-deftest lean4-loogle-query-is-encoded ()
  "A query is a URL query value, and Loogle's syntax is full of reserved
characters."
  (let ((lean4-loogle-url "https://example.invalid/json"))
    (should (equal (lean4-loogle--query-url "?a + ?b = ?b + ?a")
                   (concat "https://example.invalid/json?q="
                           "%3Fa%20%2B%20%3Fb%20%3D%20%3Fb%20%2B%20%3Fa")))))

(ert-deftest lean4-loogle-documentation-url-is-a-path-and-an-anchor ()
  "A module's dots are path separators, and the name is the anchor."
  (should (equal (lean4-loogle--documentation-url
                  '(:name "Nat.succ_le_succ" :module "Init.Prelude"))
                 (concat "https://leanprover-community.github.io/"
                         "mathlib4_docs/Init/Prelude.html#Nat.succ_le_succ")))
  ;; The slashes stay slashes.  VS Code encodes the whole path, which turns
  ;; them into %2F; its own comment admits the URL is approximate.
  (should-not (string-search "%2F" (lean4-loogle--documentation-url
                                    '(:name "f" :module "A.B.C")))))

;;;; Reading the answer

(defun lean4-loogle-test--parse (body)
  "Return what `lean4-loogle--parse' makes of an HTTP response with BODY."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n" body)
    (lean4-loogle--parse (current-buffer))))

(ert-deftest lean4-loogle-parse-skips-the-headers ()
  "The body starts after the blank line, not at the top of the buffer."
  (should (equal (plist-get (lean4-loogle-test--parse "{\"count\": 3}") :count)
                 3)))

(ert-deftest lean4-loogle-parse-rejects-what-is-not-an-answer ()
  "A proxy's error page is not JSON, and a truncated reply has no body."
  (should-error (lean4-loogle-test--parse "<html>go away</html>"))
  (should-error (with-temp-buffer
                  (insert "HTTP/1.1 500 Oops\r\nContent-Type: text/html\r\n")
                  (lean4-loogle--parse (current-buffer)))))

;;;; Drawing

(ert-deftest lean4-loogle-renders-a-hit ()
  "A result is its name, the module it is in, and its type."
  (lean4-loogle-test--with-render "Nat.succ_le_succ" lean4-loogle-test--found
    (should (string-search "Found one declaration" text))
    (should (string-search "1 result" text))
    (should-not (string-search "1 results" text))
    (should (string-search "Nat.succ_le_succ @ Init.Prelude" text))
    (should (string-search "{n m : ℕ} : n ≤ m → n.succ ≤ m.succ" text))
    (should (equal header-line-format "Loogle: Nat.succ_le_succ"))))

(ert-deftest lean4-loogle-counts-in-the-plural ()
  "More than one result is counted as results."
  (lean4-loogle-test--with-render
      "f"
      '(:hits ((:name "a" :module "M" :type "T")
               (:name "b" :module "M" :type "T")))
    (should (string-search "2 results" text))))

(ert-deftest lean4-loogle-reports-an-error-instead-of-emptiness ()
  "Loogle explains a bad query in `error'; that is not \"no results\".
Saying both would suggest the search ran and found nothing."
  (lean4-loogle-test--with-render "Nat.succ_le_suc" lean4-loogle-test--suggested
    (should (string-search "unknown identifier" text))
    (should-not (string-search "No results" text))))

(ert-deftest lean4-loogle-says-when-there-are-none ()
  "A search that ran and matched nothing says so."
  (lean4-loogle-test--with-render "Nonexistent" '(:hits nil)
    (should (string-search "No results" text))))

(ert-deftest lean4-loogle-suggestions-are-buttons ()
  "A suggestion can be searched for by pressing it."
  (lean4-loogle-test--with-render "Nat.succ_le_suc" lean4-loogle-test--suggested
    (should (string-search "Did you mean" text))
    (goto-char (point-min))
    (should (search-forward "Nat.succ_le_suc" nil t))
    ;; The button carries the query rather than depending on the text, which
    ;; Loogle returns with its own quoting.
    (let ((button (next-button (point-min))))
      (should button)
      (should (equal (button-get button 'lean4-query)
                     "\"Nat.succ_le_suc\"")))))

;;;; Acting on a result

(ert-deftest lean4-loogle-hit-at-point-walks-out-of-the-body ()
  "Point is usually on the type, not on the name."
  (lean4-loogle-test--with-render "Nat.succ_le_succ" lean4-loogle-test--found
    (ignore text)
    (goto-char (point-min))
    (search-forward "n.succ")
    (should (equal (plist-get (lean4-loogle--hit-at-point) :name)
                   "Nat.succ_le_succ"))))

(ert-deftest lean4-loogle-copy-name-fills-the-kill-ring ()
  "Copying a result takes its name, not the line it is drawn on."
  (lean4-loogle-test--with-render "Nat.succ_le_succ" lean4-loogle-test--found
    (ignore text)
    (goto-char (point-min))
    (search-forward "n.succ")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (lean4-loogle-copy-name))
      (should (equal (current-kill 0) "Nat.succ_le_succ")))))

(ert-deftest lean4-loogle-browses-the-documentation-of-the-result ()
  "Pressing a result opens its documentation, as clicking does in VS Code."
  (lean4-loogle-test--with-render "Nat.succ_le_succ" lean4-loogle-test--found
    (ignore text)
    (goto-char (point-min))
    ;; The bare name occurs first in Loogle's own header, which belongs to no
    ;; result; the heading is where the name is followed by its module.
    (search-forward "Nat.succ_le_succ @")
    (let (visited)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq visited url))))
        (lean4-loogle-browse-documentation))
      (should (equal visited
                     (concat "https://leanprover-community.github.io/"
                             "mathlib4_docs/Init/Prelude.html#Nat.succ_le_succ"))))))

(ert-deftest lean4-loogle-commands-need-a-result ()
  "Away from any result there is nothing to act on."
  (lean4-loogle-test--with-render "Nonexistent" '(:hits nil)
    (ignore text)
    (goto-char (point-min))
    (should-error (lean4-loogle-copy-name) :type 'user-error)
    (should-error (lean4-loogle-browse-documentation) :type 'user-error)))

(ert-deftest lean4-loogle-search-declines-an-empty-query ()
  "There is no point asking Loogle about nothing."
  (should-error (lean4-loogle-search "   ") :type 'user-error))

(provide 'lean4-loogle-test)
;;; lean4-loogle-test.el ends here
