;;; lean4-mode-test.el --- Tests for the major mode's bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The keymap and the menu.  Both are the kind of thing that goes wrong
;; quietly: a command can be added to the package and never bound, and
;; easymenu drops a `:label' with no complaint when the two vector forms
;; are mixed, leaving a menu item that never says what it will do.

;;; Code:

(require 'ert)
;; For `compilation-mode-font-lock-keywords', which the mode used to clear.
(require 'compile)
(require 'lean4-mode)

(defun lean4-mode-test--menu-items (menu)
  "Return every (COMMAND . LABEL-FORM) reachable from MENU, a keymap.
easymenu turns each entry into (menu-item LABEL-FORM COMMAND . PROPS),
where LABEL-FORM is evaluated afresh every time the menu is drawn --
which is what lets an item say what the next invocation will do."
  (let (items)
    (map-keymap
     (lambda (_event binding)
       (cond ((keymapp binding)
              (setq items (append (lean4-mode-test--menu-items binding) items)))
             ((eq (car-safe binding) 'menu-item)
              (push (cons (nth 2 binding) (nth 1 binding)) items))))
     menu)
    items))

(defun lean4-mode-test--menu-label (command)
  "Return the label the menu would draw for COMMAND right now."
  (when-let* ((item (assq command (lean4-mode-test--menu-items lean4-mode-menu))))
    (eval (cdr item) t)))

(ert-deftest lean4-mode-binds-the-goal-display-commands ()
  "Every goal-display command is reachable from the keyboard.

The controls in the display are clickable, which makes it easy to add
one and never bind it, leaving a feature the mouse can reach and the
keyboard cannot."
  (dolist (binding '(("C-c C-s" . lean4-info-toggle-pin)
                     ("C-c C-SPC" . lean4-info-toggle-pause)
                     ("C-c C-o" . lean4-info-toggle-message-order)
                     ("C-c C-a" . lean4-info-toggle-all-messages-pause)))
    (should (eq (keymap-lookup lean4-mode-map (car binding)) (cdr binding)))
    ;; And in the display itself, so none of them needs the Lean buffer.
    (should (eq (keymap-lookup lean4-info-mode-map (car binding))
                (cdr binding)))))

(ert-deftest lean4-mode-menu-offers-the-goal-display-commands ()
  "The menu reaches them too."
  (let ((items (lean4-mode-test--menu-items lean4-mode-menu)))
    (dolist (command '(lean4-info-toggle-pin
                       lean4-info-toggle-pause
                       lean4-info-toggle-message-order
                       lean4-info-toggle-all-messages-pause))
      (should (assq command items)))))

(ert-deftest lean4-mode-menu-labels-are-live ()
  "The items that toggle say what the next invocation will do.

Regression test.  easymenu has two vector forms -- [NAME CMD ENABLE] and
[NAME CMD :keyword value...] -- and silently drops the keywords when a
positional ENABLE precedes them, so the `:label' went missing and the
menu read \"Pin\" whether or not the display was already pinned."
  (with-temp-buffer
    ;; The pin item reads whether *this* position is pinned, so it needs a
    ;; buffer and a pin in it rather than a flag.
    (dolist (entry (list (list 'lean4-info-toggle-pin
                               (lambda (on)
                                 (setq lean4-info--pins
                                       (and on (list (lean4-info--pin-create
                                                      :marker (point-marker))))))
                               "npin")
                         (list 'lean4-info-toggle-pause
                               (lambda (on) (setq lean4-info-paused on))
                               "npause")
                         (list 'lean4-info-toggle-all-messages-pause
                               (lambda (on)
                                 (setq lean4-info-all-messages-paused on))
                               "npause")))
      (pcase-let ((`(,command ,setter ,engaged) entry))
        ;; Drawn with the state off, then on: the two have to differ, and
        ;; the engaged one has to be the undo wording.
        (let* ((off (progn (funcall setter nil)
                           (lean4-mode-test--menu-label command)))
               (on (progn (funcall setter t)
                          (lean4-mode-test--menu-label command))))
          (funcall setter nil)
          (should off)
          (should on)
          (should-not (equal off on))
          (should (string-search engaged (downcase on))))))))

(defmacro lean4-mode-test--with-mode (&rest body)
  "Evaluate BODY in a `lean4-mode' buffer, with nothing else started."
  (declare (indent 0) (debug (body)))
  `(with-temp-buffer
     (let ((lean4-mode-hook nil)
           (lean4-info-auto-open nil))
       (lean4-mode))
     ,@body))

(ert-deftest lean4-mode-leaves-globals-alone ()
  "The mode command changes nothing outside its own buffer.

Regression test.  `set-input-method' assigns the global
`default-input-method' as well as the buffer\\='s own, so opening one Lean
file left \\`C-\\' toggling the Lean input method in every other buffer;
and `compilation-mode-font-lock-keywords' was cleared outright, which
emptied every compilation buffer for the rest of the session."
  (let ((default-input-method 'untouched)
        (compilation-mode-font-lock-keywords 'untouched))
    (lean4-mode-test--with-mode
      ;; Buffer-locally it is on, which is the whole point of doing it.
      (should (equal current-input-method "Lean")))
    (should (eq default-input-method 'untouched))
    (should (eq compilation-mode-font-lock-keywords 'untouched))))

(ert-deftest lean4-mode-inhibits-electric-indent-by-variable ()
  "Reindenting on newline is declined through the variable meant for it.
`electric-indent-local-mode' would switch off a minor mode the reader
turned on, which is not a major mode\\='s business; `electric-indent-inhibit'
says the same thing without touching the mode\\='s own state."
  (lean4-mode-test--with-mode
    (should electric-indent-inhibit)
    (should-not (local-variable-p 'electric-indent-mode))))

(ert-deftest lean4-mode-tables-are-named-for-the-mode ()
  "The syntax and abbrev tables carry the names the conventions ask for.
`(elisp) Major Mode Conventions' asks for `MODE-syntax-table' and
`MODE-abbrev-table', which is where `edit-abbrevs' and a reader of Emacs
both look."
  (should (syntax-table-p lean4-mode-syntax-table))
  (should (abbrev-table-p lean4-mode-abbrev-table))
  (lean4-mode-test--with-mode
    (should (eq (syntax-table) lean4-mode-syntax-table))
    (should (eq local-abbrev-table lean4-mode-abbrev-table))))

(ert-deftest lean4-menus-share-the-goal-display-controls ()
  "Both menus offer the display\\='s controls, from the one list.
The commands work from either buffer, so a control in one menu and not
the other would only be a gap in whichever lacked it."
  (let ((commands (mapcar (lambda (item) (aref item 1)) lean4-info-menu-items)))
    (should commands)
    (dolist (menu (list lean4-mode-menu lean4-info-mode-menu))
      (let ((items (lean4-mode-test--menu-items menu)))
        (dolist (command commands)
          (should (assq command items)))))))

(ert-deftest lean4-info-mode-menu-offers-its-own-commands ()
  "The goal display\\='s menu also reaches what only it can do."
  (let ((items (lean4-mode-test--menu-items lean4-info-mode-menu)))
    (should (assq 'lean4-info-goto-type-definition items))
    (should (assq 'lean4-toggle-info items))))

(ert-deftest lean4-menu-pin-label-reads-the-source-buffer ()
  "In the goal display the pin item reports on the Lean buffer.

`lean4-info-toggle-pin' run from the display defers to the Lean buffer,
so a label that asked only about the display -- where no pin marker ever
lives -- offered to pin even with the followed position already pinned."
  (let ((source (generate-new-buffer " *lean4-mode-test-source*"))
        (lean4-info--pins nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (let ((lean4-mode-hook nil)
                  (lean4-info-auto-open nil))
              (lean4-mode))
            (insert "example : True := trivial\n")
            (goto-char (point-min))
            (setq lean4-info--pins
                  (list (lean4-info--pin-create :marker (point-marker)))))
          (with-temp-buffer
            (let ((lean4-info-mode-hook nil))
              (lean4-info-mode))
            (setq lean4-info--source-buffer source)
            (should (equal (lean4-mode-test--menu-label 'lean4-info-toggle-pin)
                           "Unpin this position"))))
      (kill-buffer source))))

(provide 'lean4-mode-test)
;;; lean4-mode-test.el ends here
