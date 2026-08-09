;;; lean4-info-test.el --- Tests for the goal display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the parts of the goal display that need no server.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lean4-mode)

(defmacro lean4-info-test--with-fringe-indicators (&rest body)
  "Evaluate BODY with `magit-section' drawing fold indicators in the fringe.

The variable behind this was renamed and reshaped in magit-section 4:
3.3.0, which Debian and Ubuntu ship, has
`magit-section-visibility-indicator' holding a single cons, and later
versions have `magit-section-visibility-indicators' holding an alist
keyed by frame type.  Bind whichever exists.

The fringe form is the one to ask for either way.  3.3.0 draws its other
form, an ellipsis, only on a section that is already hidden, so it would
show nothing here; and the fringe form makes a real overlay even under
--batch, where the bitmap itself has nowhere to render."
  (declare (indent 0) (debug t))
  (let ((fringe '(magit-fringe-bitmap> . magit-fringe-bitmapv)))
    `(cl-progv
         (list (if (boundp 'magit-section-visibility-indicators)
                   'magit-section-visibility-indicators
                 'magit-section-visibility-indicator))
         (list (if (boundp 'magit-section-visibility-indicators)
                   '((,fringe) (?> . ?v))
                 ',fringe))
       ,@body)))

(defun lean4-info-test--indicator-count ()
  "Return the number of fold indicators in the current buffer."
  (length (seq-filter (lambda (overlay)
                        (overlay-get overlay 'magit-vis-indicator))
                      (overlays-in (point-min) (point-max)))))

(defun lean4-info-test--foldable-count ()
  "Return the number of sections in this buffer that have a body to fold."
  (let ((n 0))
    (letrec ((walk (lambda (section)
                     (when (oref section content) (cl-incf n))
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))
    n))

(ert-deftest lean4-info-sections-show-fold-indicators-when-inserted ()
  "A freshly built goal display shows which sections fold.

Regression test.  magit-section 4 updates its indicators only from
`magit-section-show' and `magit-section-hide', so nothing inserted
carries one until it has been toggled: the display looked as though
nothing folded, and folding one section made that one section -- and
only that one -- grow a chevron.  3.3.0 draws them as it inserts and
never had the bug, which is why this asserts the end state rather than
that we were the ones to bring it about."
  (lean4-info-test--with-fringe-indicators
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (with-current-buffer lean4-info-buffer-name
          (let ((inhibit-read-only t))
            (erase-buffer)
            (magit-insert-section (magit-section 'root)
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body (insert "one goal\n")))))
          (lean4-info--add-visibility-indicators)
          (should (> (lean4-info-test--foldable-count) 0))
          (should (= (lean4-info-test--indicator-count)
                     (lean4-info-test--foldable-count))))
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-controls-run-in-the-lean-buffer ()
  "A clicked control acts on the Lean buffer, not on the info buffer.

Regression test.  `mouse-1' runs the command with the info buffer
current, where pinning is meaningless -- `lean4-info-toggle-pin' needs
point in a Lean buffer, so clicking the pin only raised an error."
  (let ((source (get-buffer-create "*lean4-info-test-source*")))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (progn
          (with-current-buffer source
            (let ((lean4-mode-hook nil)
                  (lean4-auto-start-server nil)
                  (lean4-info-auto-open nil))
              (lean4-mode))
            (insert "example : True := trivial\n"))
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source)
            (lean4-info--run-control #'lean4-info-toggle-pin)
            (should lean4-info--pin)
            (should (eq (marker-buffer lean4-info--pin) source))
            (lean4-info--run-control #'lean4-info-toggle-pin)
            (should-not lean4-info--pin)))
      (when lean4-info--pin
        (set-marker lean4-info--pin nil)
        (setq lean4-info--pin nil))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-says-so-when-there-is-nothing-to-report ()
  "An empty display says \"No info found.\" rather than showing a bare heading.
An empty buffer reads like one that has stopped working."
  (let ((source (get-buffer-create "*lean4-info-test-source*")))
    (unwind-protect
        (with-current-buffer source
          (let ((lean4-mode-hook nil)
                (lean4-auto-start-server nil)
                (lean4-info-auto-open nil))
            (lean4-mode))
          (setq lean4-goals nil lean4-term-goal nil lean4-info--diagnostics nil)
          (setq lean4-info--rendered nil)
          (lean4-ensure-info-buffer lean4-info-buffer-name)
          (lean4-info-buffer-redisplay 'force)
          (with-current-buffer lean4-info-buffer-name
            (should (string-search "No info found." (buffer-string)))))
      (kill-buffer source)
      (when (get-buffer lean4-info-buffer-name)
        (kill-buffer lean4-info-buffer-name)))))

(ert-deftest lean4-info-heading-reports-both-states-at-once ()
  "Pinned and paused are different things and can hold together.

Regression test.  Reporting only one of them left the other looking as
though the command had not taken effect."
  (with-temp-buffer
    (rename-buffer "Both.lean" 'unique)
    (cl-flet ((state ()
                (substring-no-properties
                 (lean4-info--heading (lean4-info--location-string)))))
      (let ((lean4-info--pin nil) (lean4-info-paused nil))
        (should-not (string-search "pinned" (state)))
        (should-not (string-search "paused" (state))))
      (let ((lean4-info--pin (point-marker)) (lean4-info-paused nil))
        (should (string-search "pinned" (state)))
        (should-not (string-search "paused" (state))))
      (let ((lean4-info--pin nil) (lean4-info-paused t))
        (should (string-search "paused" (state)))
        (should-not (string-search "pinned" (state))))
      (let ((lean4-info--pin (point-marker)) (lean4-info-paused t))
        (should (string-search "pinned and paused" (state)))))))

(ert-deftest lean4-info-rebuilding-does-not-scroll-the-display ()
  "A rebuild leaves the reader where they were.

Regression test.  Erasing and reinserting sends point and the window
start to the end of the new text, so clicking a control -- which
rebuilds in order to redraw the control -- threw the display to the
bottom, as did every refresh while reading a long goal."
  (with-temp-buffer
    (dotimes (i 200) (insert (format "line %d\n" i)))
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (goto-char (point-min))
      (forward-line 100)
      (set-window-start window (point))
      (let ((start (window-start window))
            (spot (point)))
        (lean4-info--keeping-position
          (erase-buffer)
          (dotimes (i 200) (insert (format "line %d\n" i))))
        (should (= (point) spot))
        (should (= (window-start window) start))
        (should (= (window-point window) spot))))))

(ert-deftest lean4-info-keeping-position-survives-a-shorter-rebuild ()
  "Restoring is clamped: the new text can be shorter than the old."
  (with-temp-buffer
    (dotimes (i 200) (insert (format "line %d\n" i)))
    (set-window-buffer (selected-window) (current-buffer))
    (goto-char (point-max))
    (lean4-info--keeping-position
      (erase-buffer)
      (insert "No info found.\n"))
    (should (<= (point) (point-max)))
    (should (<= (window-start (selected-window)) (point-max)))))

(ert-deftest lean4-info-message-badge-counts-by-severity ()
  "The heading says what kinds of message a file has, as VS Code does."
  (cl-letf (((symbol-function 'lean4-info--displayable-p) (lambda (&rest _) t)))
    (should (equal (lean4-info--severity-badge
                    '((:severity 1) (:severity 3) (:severity 3)
                      (:severity 3) (:severity 3) (:severity 3)))
                   "1 ⊗  5 ⓘ"))
    ;; Most severe first, and severities nobody has are left out rather
    ;; than shown as zero.
    (should (equal (lean4-info--severity-badge
                    '((:severity 4) (:severity 2) (:severity 1)))
                   "1 ⊗  1 ⚠  1 ⓗ"))
    (should (equal (lean4-info--severity-badge '((:severity 2))) "1 ⚠"))
    ;; A diagnostic with no severity is an error, as everywhere else.
    (should (equal (lean4-info--severity-badge '((:message "x"))) "1 ⊗")))
  (should-not (lean4-info--severity-badge nil)))

(ert-deftest lean4-info-message-badge-falls-back-to-letters ()
  "A frame with no glyphs gets letters, not a row of boxes."
  (cl-letf (((symbol-function 'lean4-info--displayable-p) #'ignore))
    (should (equal (lean4-info--severity-badge
                    '((:severity 1) (:severity 3)))
                   "1 E  1 I"))))

(ert-deftest lean4-info-all-messages-caption-carries-the-badge ()
  "The caption names the section and counts what is in it."
  (cl-letf (((symbol-function 'lean4-info--displayable-p) (lambda (&rest _) t)))
    (should (equal (lean4-info--all-messages-caption '((:severity 1)))
                   "All messages (1 ⊗):")))
  ;; Nothing to count is not a state the section is inserted in, but the
  ;; caption should still read as a caption.
  (should (equal (lean4-info--all-messages-caption nil) "All messages:")))

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
      ;; The binding is a wrapper that re-runs the command in the Lean
      ;; buffer, so check what it was built to invoke, and that what is
      ;; bound is something `mouse-1' can actually run.
      (should (eq (get-text-property pin 'lean4-info-command heading)
                  'lean4-info-toggle-pin))
      (should (eq (get-text-property pause 'lean4-info-command heading)
                  'lean4-info-toggle-pause))
      (should (commandp (keymap-lookup
                         (get-text-property pin 'keymap heading) "<mouse-1>")))
      (should (commandp (keymap-lookup
                         (get-text-property pause 'keymap heading)
                         "<mouse-1>"))))))

(ert-deftest lean4-info-pin-control-shows-which-way-it-will-go ()
  "The pin control differs pinned from unpinned, as pause and resume do.
Without this the only sign of the state was the control's face, which is
not much to read a mode off."
  (should-not (equal (lean4-info-pin-glyph) (lean4-info-unpin-glyph)))
  ;; Including when the frame can display none of the candidates.
  (cl-letf (((symbol-function 'lean4-info--displayable-p) #'ignore))
    (should-not (equal (lean4-info-pin-glyph) (lean4-info-unpin-glyph))))
  ;; And when it can display all of them, so the pair is the emoji pair
  ;; rather than two tiers of the fallback chain.
  (cl-letf (((symbol-function 'lean4-info--displayable-p)
             (lambda (&rest _) t)))
    (should (equal (lean4-info-pin-glyph) "📌"))
    (should (equal (lean4-info-unpin-glyph) "📍")))
  ;; A configured glyph wins, as for the others.
  (let ((lean4-info-unpin-icon "!"))
    (should (equal (lean4-info-unpin-glyph) "!"))))

(ert-deftest lean4-info-heading-shows-the-unpin-control-when-pinned ()
  "The heading picks up the swapped glyph."
  (with-temp-buffer
    (rename-buffer "Qux.lean" 'unique)
    (let* ((lean4-info-paused nil)
           (lean4-info--pin (copy-marker (point-min)))
           (heading (substring-no-properties
                     (lean4-info--heading (lean4-info--location-string)))))
      (should (string-search (lean4-info-unpin-glyph) heading))
      (should-not (string-search (lean4-info-pin-glyph) heading))
      (set-marker lean4-info--pin nil))))

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
A second channel alongside the swapped glyph, and the one that survives
a frame with no glyph to swap to."
  (with-temp-buffer
    (rename-buffer "Faces.lean" 'unique)
    (cl-flet ((pin-face (glyph)
                (let* ((heading (lean4-info--heading
                                 (lean4-info--location-string)))
                       (index (string-search
                               glyph (substring-no-properties heading))))
                  (should index)
                  (get-text-property index 'face heading))))
      (let ((lean4-info--pin nil) (lean4-info-paused nil))
        (should (eq (pin-face (lean4-info-pin-glyph)) 'lean4-info-button)))
      (let ((lean4-info--pin (point-marker)) (lean4-info-paused nil))
        (should (eq (pin-face (lean4-info-unpin-glyph))
                    'lean4-info-button-active))))))

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

(ert-deftest lean4-info-does-not-rebuild-when-nothing-changed ()
  "An unchanged display is left alone.

Regression test.  The buffer is erased and re-inserted from scratch, so
rebuilding it is visible as a flicker.  The server republishes
diagnostics repeatedly -- some projects do so continuously -- and each
one rebuilt the buffer identically, which reads as the display blinking
at the reader for as long as it is on screen."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (let ((rebuilds 0)
            (lean4-info--rendered nil))
        (advice-add 'erase-buffer :before
                    (lambda (&rest _)
                      (when (eq (current-buffer)
                                (get-buffer lean4-info-buffer-name))
                        (cl-incf rebuilds)))
                    '((name . lean4-info-test-count)))
        (unwind-protect
            (with-temp-buffer
              (rename-buffer "Flicker.lean" 'unique)
              (setq-local lean4-goals "⊢ True")
              ;; Displayed and selected, as `lean4-info-buffer-active' wants.
              (set-window-buffer (selected-window) (current-buffer))
              (save-window-excursion
                (display-buffer lean4-info-buffer-name)
                (lean4-info-buffer-redisplay 'force)
                (should (= rebuilds 1))
                ;; Nothing has changed, so nothing should happen.
                (dotimes (_ 5) (lean4-info-buffer-redisplay 'force))
                (should (= rebuilds 1))
                ;; A real change is drawn.
                (setq-local lean4-goals "⊢ False")
                (lean4-info-buffer-redisplay 'force)
                (should (= rebuilds 2))))
          (advice-remove 'erase-buffer 'lean4-info-test-count)))
    (kill-buffer lean4-info-buffer-name)))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
