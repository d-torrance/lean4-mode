;;; lean4-info-test.el --- Tests for the goal display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the parts of the goal display that need no server.

;;; Code:

(require 'ert)
(require 'lean4-mode)

(defun lean4-info-test--indicator-count ()
  "Return the number of fold indicators in the current buffer."
  (length (seq-filter (lambda (overlay)
                        (overlay-get overlay 'magit-vis-indicator))
                      (overlays-in (point-min) (point-max)))))

(ert-deftest lean4-info-sections-show-fold-indicators-when-inserted ()
  "A freshly built goal display shows which sections fold.

Regression test.  `magit-section' updates its indicators only from
`magit-section-show' and `magit-section-hide', so nothing inserted
carries one until it has been toggled: the display looked as though
nothing folded, and folding one section made that one section -- and
only that one -- grow a chevron."
  (let ((magit-section-visibility-indicators
         ;; Character indicators go in the margin, which leaves an overlay
         ;; to count.  The fringe form used in graphical frames cannot be
         ;; exercised under --batch.
         '((magit-fringe-bitmap> . magit-fringe-bitmapv) (?> . ?v))))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (with-current-buffer lean4-info-buffer-name
          (let ((inhibit-read-only t))
            (erase-buffer)
            (magit-insert-section (magit-section 'root)
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body (insert "one goal\n")))))
          (should (= (lean4-info-test--indicator-count) 0))
          (lean4-info--add-visibility-indicators)
          (should (> (lean4-info-test--indicator-count) 0)))
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-goals-value-distinguishes-three-outcomes ()
  "No proof, a finished proof, and an open goal are told apart.

Conflating the first two either hides the reward for finishing or claims
every ordinary line is proved."
  ;; Not inside a proof at all: the server answers with nothing.
  (should-not (lean4-info--goals-value nil [] #'ignore))
  ;; Inside a proof with nothing left.
  (should (eq (lean4-info--goals-value '(:goals []) [] #'ignore)
              'accomplished))
  ;; Something left to prove.
  (should (equal (lean4-info--goals-value '(:goals ["x"]) ["x"]
                                          (lambda (goals) (elt goals 0)))
                 "x")))

(defun lean4-info-test--header-text ()
  "Return the header line's text, without properties."
  (mapconcat (lambda (part)
               (if (stringp part) (substring-no-properties part) ""))
             header-line-format ""))

(ert-deftest lean4-info-header-offers-controls ()
  "The header line carries working pin and pause controls.

VS Code puts these where the reader is already looking.  A control that
appears only once you have found the keybinding is not much of one."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((lean4-info--pin nil) (lean4-info-paused nil))
          (lean4-info--update-header)
          (should (string-search "[Pin]" (lean4-info-test--header-text)))
          (should (string-search "[Pause]" (lean4-info-test--header-text))))
        ;; The labels say what the click will do.
        (let ((lean4-info--pin (point-marker)) (lean4-info-paused nil))
          (lean4-info--update-header)
          (should (string-search "[Unpin]" (lean4-info-test--header-text)))
          (should (string-search "Pinned to" (lean4-info-test--header-text))))
        (let ((lean4-info--pin nil) (lean4-info-paused t))
          (lean4-info--update-header)
          (should (string-search "[Unpause]" (lean4-info-test--header-text)))
          (should (string-search "Paused" (lean4-info-test--header-text)))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-header-buttons-are-clickable ()
  "Clicking a control in the header line runs the command."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((lean4-info--pin nil) (lean4-info-paused nil))
          (lean4-info--update-header)
          (let* ((text (lean4-info-test--header-text))
                 (raw (mapconcat (lambda (p) (if (stringp p) p ""))
                                 header-line-format ""))
                 (index (string-search "[Pin]" text)))
            (should index)
            (should (eq (keymap-lookup (get-text-property index 'keymap raw)
                                       "<header-line> <mouse-1>")
                        'lean4-info-toggle-pin)))))
    (kill-buffer lean4-info-buffer-name)))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
