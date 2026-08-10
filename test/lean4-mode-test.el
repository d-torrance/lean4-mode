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

(provide 'lean4-mode-test)
;;; lean4-mode-test.el ends here
