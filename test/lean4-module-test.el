;;; lean4-module-test.el --- Tests for the module hierarchy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The tree is drawn from two hash tables and a root, so all of this runs
;; without a server: the tables are filled with what Lean would have sent.
;; The payload shapes are `LeanModule' and `LeanImport' as
;; `Lean/Data/Lsp/Extra.lean' declares them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pcase)
(require 'lean4-module)

;;;; Fixtures

(defun lean4-module-test--module (name)
  "Return a `LeanModule' called NAME."
  (list :name name :uri (concat "file:///pkg/" name ".lean")))

(defun lean4-module-test--import (name &rest kind)
  "Return a `LeanImport' of NAME, with KIND as its import kind."
  (list :module (lean4-module-test--module name)
        :kind (or kind (list :isPrivate :json-false :isAll :json-false
                             :metaKind "nonMeta"))))

(defmacro lean4-module-test--with-tree (root children &rest body)
  "Draw ROOT with CHILDREN and evaluate BODY in the hierarchy buffer.
CHILDREN is an alist of path to list of `LeanImport'.  The real buffer
name and the real `lean4-module--redisplay' are used, so this exercises
the drawing rather than a copy of it.  Inside BODY the buffer is current
and `text' is its contents without properties."
  (declare (indent 2) (debug (form form body)))
  `(unwind-protect
       (with-current-buffer (get-buffer-create lean4-module-buffer-name)
         (lean4-module-mode)
         (setq lean4-module--root ,root
               lean4-module--direction 'imports
               lean4-module--children (make-hash-table :test #'equal)
               lean4-module--pending (make-hash-table :test #'equal))
         (pcase-dolist (`(,path . ,imports) ,children)
           (puthash path imports lean4-module--children))
         (lean4-module--redisplay)
         (let ((text (substring-no-properties (buffer-string))))
           ,@body))
     (when (get-buffer lean4-module-buffer-name)
       (kill-buffer lean4-module-buffer-name))))

;;;; Reading what the server sends

(ert-deftest lean4-module-modifiers-are-empty-by-default ()
  "An ordinary import has nothing to say about itself.
Lean sets these flags only for modules using the `module' keyword, and a
JSON false does not reach Emacs as nil."
  (should-not (lean4-module--modifiers (lean4-module-test--import "Base")))
  (should-not (lean4-module--modifiers
               (lean4-module-test--import
                "Base" :isPrivate :json-false :isAll :json-false
                :metaKind "nonMeta"))))

(ert-deftest lean4-module-modifiers-are-reported ()
  "Each flag Lean sets is named, and `full' means both ways at once."
  (should (equal (lean4-module--modifiers
                  (lean4-module-test--import
                   "Base" :isPrivate t :isAll t :metaKind "meta"))
                 '("private" "all" "meta")))
  (should (equal (lean4-module--modifiers
                  (lean4-module-test--import
                   "Base" :isPrivate :json-false :isAll :json-false
                   :metaKind "full"))
                 '("meta and non-meta")))
  ;; An unknown kind from a newer Lean is passed over rather than shown raw.
  (should-not (lean4-module--modifiers
               (lean4-module-test--import "Base" :metaKind "somethingNew"))))

(ert-deftest lean4-module-direction-picks-the-request ()
  "Each way through the graph is a request of its own."
  (should (eq (lean4-module--children-method 'imports)
              :$/lean/moduleHierarchy/imports))
  (should (eq (lean4-module--children-method 'imported-by)
              :$/lean/moduleHierarchy/importedBy)))

;;;; Drawing

(ert-deftest lean4-module-root-is-shown-with-its-children ()
  "The root is open and its children are one level in."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top") (list (lean4-module-test--import "Base")
                                 (lean4-module-test--import "Middle"))))
    (should (equal text "Top\n  Base\n  Middle\n"))))

(ert-deftest lean4-module-a-childless-module-is-a-leaf ()
  "Known to import nothing means no body, so nothing promises to unfold.
`magit-section' draws its indicator for anything it could unfold, and
unfolding nothing reads like a tree that has broken."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top") nil))
    (should (equal text "Top\n"))))

(ert-deftest lean4-module-unfetched-children-are-pending ()
  "A node whose children have not arrived is remembered to be asked about."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top") (list (lean4-module-test--import "Base"))))
    (ignore text)
    ;; The root's children are known; Base's are not, and Base starts folded,
    ;; so its body -- and the note of what to ask for -- waits until it opens.
    (should (zerop (hash-table-count lean4-module--pending)))
    (goto-char (point-min))
    (search-forward "Base")
    (magit-section-show (magit-current-section))
    (should (equal (hash-table-keys lean4-module--pending) '(("Base" "Top"))))
    (should (string-search "..." (buffer-string)))))

(ert-deftest lean4-module-placeholder-is-indented ()
  "The placeholder sits under the node it belongs to.

`magit-section' defers the body of a folded section, by which time the
binding saying how far in we are has unwound -- so without capturing it
the placeholder came out hard left."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top") (list (lean4-module-test--import "Base"))))
    (ignore text)
    (goto-char (point-min))
    (search-forward "Base")
    (magit-section-show (magit-current-section))
    (goto-char (point-min))
    (should (re-search-forward "^ \\{4\\}\\.\\.\\.$" nil t))))

(ert-deftest lean4-module-repeated-module-is-not-expanded ()
  "A module already on the path to the root is drawn as a leaf.
Lean's import graph has diamonds, and following one would go round for
ever."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top") (list (lean4-module-test--import "Middle")))
            (cons '("Middle" "Top") (list (lean4-module-test--import "Top"))))
    (should (equal text "Top\n  Middle\n"))
    (goto-char (point-min))
    (search-forward "Middle")
    (magit-section-show (magit-current-section))
    (let ((shown (substring-no-properties (buffer-string))))
      (should (string-search "(shown above)" shown))
      ;; And nothing was queued to ask about underneath it.
      (should (zerop (hash-table-count lean4-module--pending))))))

(ert-deftest lean4-module-modifiers-appear-in-the-heading ()
  "An import's modifiers are shown beside the module it is of."
  (lean4-module-test--with-tree
      (lean4-module-test--module "Top")
      (list (cons '("Top")
                  (list (lean4-module-test--import
                         "Base" :isPrivate t :isAll :json-false
                         :metaKind "meta"))))
    (should (equal text "Top\n  Base private, meta\n"))))

;;;; Finding a module again

(ert-deftest lean4-module-module-at-path-finds-the-root ()
  "The root is not in any children table; it is the root."
  (let* ((root (lean4-module-test--module "Top"))
         (lean4-module--root root)
         (lean4-module--children (make-hash-table :test #'equal)))
    (should (equal (lean4-module--module-at-path '("Top")) root))))

(ert-deftest lean4-module-module-at-path-finds-a-child ()
  "A child is looked up among its parent's children."
  (let* ((lean4-module--root (lean4-module-test--module "Top"))
         (lean4-module--children (make-hash-table :test #'equal)))
    (puthash '("Top") (list (lean4-module-test--import "Base")
                            (lean4-module-test--import "Middle"))
             lean4-module--children)
    (should (equal (lean4-module--name
                    (lean4-module--module-at-path '("Middle" "Top")))
                   "Middle"))
    (should-not (lean4-module--module-at-path '("Absent" "Top")))))

(provide 'lean4-module-test)
;;; lean4-module-test.el ends here
