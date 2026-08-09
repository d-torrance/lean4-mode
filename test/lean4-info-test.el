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

(ert-deftest lean4-info-heading-reports-the-position ()
  "The heading says which file and position the display is reporting on."
  (with-temp-buffer
    (rename-buffer "Foo.lean" 'unique)
    (insert "theorem t : True := by\n  trivial\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 3)
    (let ((lean4-info--pin nil) (lean4-info-paused nil))
      (should (string-prefix-p "Foo.lean:2:3"
                               (substring-no-properties
                                (lean4-info--heading
                                 (lean4-info--location-string))))))))

(ert-deftest lean4-info-heading-follows-the-pin ()
  "When pinned, the heading reports the pinned position, not point."
  (with-temp-buffer
    (rename-buffer "Bar.lean" 'unique)
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((lean4-info--pin (point-marker))
          (lean4-info-paused nil))
      ;; Point moves away; the heading should not.
      (goto-char (point-max))
      (should (string-prefix-p "Bar.lean:2:0"
                               (substring-no-properties
                                (lean4-info--heading
                                 (lean4-info--location-string)))))
      (should (string-search "pinned"
                             (substring-no-properties
                              (lean4-info--heading
                               (lean4-info--location-string))))))))

(ert-deftest lean4-info-heading-carries-clickable-controls ()
  "The controls are in the heading and run their commands when clicked."
  (with-temp-buffer
    (rename-buffer "Baz.lean" 'unique)
    (let* ((lean4-info--pin nil)
           (lean4-info-paused nil)
           (heading (lean4-info--heading (lean4-info--location-string)))
           (plain (substring-no-properties heading))
           (pin (string-search (lean4-info-pin-glyph) plain))
           (pause (string-search (lean4-info-pause-glyph) plain)))
      (should pin)
      (should pause)
      (should (eq (keymap-lookup (get-text-property pin 'keymap heading)
                                 "<mouse-1>")
                  'lean4-info-toggle-pin))
      (should (eq (keymap-lookup (get-text-property pause 'keymap heading)
                                 "<mouse-1>")
                  'lean4-info-toggle-pause)))))

(ert-deftest lean4-info-controls-fall-back-to-ascii ()
  "The controls are configurable, for fonts without the glyphs.
Emacs runs in terminals and on machines with no emoji font, so the
defaults are chosen with `char-displayable-p' and can be overridden."
  (let ((lean4-info-pin-icon "P")
        (lean4-info-pause-icon "||")
        (lean4-info--pin nil)
        (lean4-info-paused nil))
    (let ((plain (substring-no-properties
                    (lean4-info--heading (lean4-info--location-string)))))
      (should (string-search "P" plain))
      (should (string-search "||" plain)))))

(ert-deftest lean4-info-pause-control-shows-what-it-will-do ()
  "The pause control becomes a play symbol while paused."
  (with-temp-buffer
    (rename-buffer "Icons.lean" 'unique)
    (let ((lean4-info--pin nil) (lean4-info-paused nil))
      (should (string-search (lean4-info-pause-glyph)
                             (substring-no-properties
                              (lean4-info--heading
                               (lean4-info--location-string))))))
    (let ((lean4-info--pin nil) (lean4-info-paused t))
      (let ((plain (substring-no-properties
                    (lean4-info--heading (lean4-info--location-string)))))
        (should (string-search (lean4-info-resume-glyph) plain))
        (should-not (string-search (lean4-info-pause-glyph) plain))))))

(ert-deftest lean4-info-pin-control-shows-its-state-by-face ()
  "The pin control is faced differently when engaged.

Unicode has no rotated-pin pair, and the nearest alternatives differ in
width, so the state is carried by the face rather than by a second
glyph."
  (with-temp-buffer
    (rename-buffer "Faces.lean" 'unique)
    (cl-flet ((pin-face ()
                (let* ((heading (lean4-info--heading
                                 (lean4-info--location-string)))
                       (index (string-search
                               (lean4-info-pin-glyph)
                               (substring-no-properties heading))))
                  (get-text-property index 'face heading))))
      (let ((lean4-info--pin nil) (lean4-info-paused nil))
        (should (eq (pin-face) 'lean4-info-button)))
      (let ((lean4-info--pin (point-marker)) (lean4-info-paused nil))
        (should (eq (pin-face) 'lean4-info-button-active))))))

(ert-deftest lean4-info-glyphs-suit-the-frame ()
  "The controls are chosen for the frame they are drawn in.

One Emacs can serve a graphical frame and a terminal at once, so
deciding at load time would give both whichever came up first."
  (let ((lean4-info-pin-icon nil)
        (lean4-info-pause-icon nil)
        (lean4-info-resume-icon nil))
    ;; A terminal with the media controls but no emoji.
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (character)
                 (or (< character 128) (<= #x2300 character #x23FF)))))
      (should (equal (lean4-info-pin-glyph) "⌖"))
      (should (equal (lean4-info-pause-glyph) "⏸")))
    ;; A terminal that can show nothing but ASCII.
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (character) (< character 128))))
      (should (equal (lean4-info-pin-glyph) "P"))
      (should (equal (lean4-info-pause-glyph) "||"))
      (should (equal (lean4-info-resume-glyph) ">")))))

(ert-deftest lean4-info-configured-glyph-wins ()
  "An explicitly configured control is used whatever the frame can show."
  (let ((lean4-info-pin-icon "PIN"))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (equal (lean4-info-pin-glyph) "PIN")))))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
