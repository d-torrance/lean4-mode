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
  "Return the number of fold indicators drawn in this buffer."
  (length (seq-filter (lambda (overlay)
                        (overlay-get overlay 'magit-vis-indicator))
                      (overlays-in (point-min) (point-max)))))

(ert-deftest lean4-info-sections-show-fold-indicators-when-inserted ()
  "A freshly built display shows which sections fold.

Regression test.  `magit-section' updates its indicators only from
`magit-section-show' and `magit-section-hide', so nothing inserted
carried one until it had been toggled: the display looked as though
nothing folded, and folding one section made that one -- and only that
one -- grow an indicator."
  (lean4-info-test--with-fringe-indicators
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (with-current-buffer lean4-info-buffer-name
          (let ((inhibit-read-only t))
            (erase-buffer)
            (magit-insert-section (lean4-info-section 'root)
              (magit-insert-section (lean4-info-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body (insert "one goal\n")))))
          (lean4-info--add-visibility-indicators)
          (should (> (lean4-info-test--foldable-count) 0))
          (should (= (lean4-info-test--indicator-count)
                     (lean4-info-test--foldable-count))))
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-indentation-is-real-text ()
  "Sections are set in with spaces, as `magit-section' buffers are.

Drawn text rather than a `line-prefix' or a `display' property: those
are drawn only where a display line begins, or drawn even where the
character is invisible, and folding a section puts the heading below it
on the wrong side of both rules."
  (with-temp-buffer
    (lean4-info--insert "top\n")
    (lean4-info--indented
      (lean4-info--insert "one\n")
      (lean4-info--indented (lean4-info--insert "two\n")))
    (should (equal (buffer-string) "top\n  one\n    two\n"))
    ;; Nothing out of band is left behind.
    (should-not (text-property-not-all (point-min) (point-max)
                                       'line-prefix nil))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'display nil))))

(ert-deftest lean4-info-indents-only-where-a-line-begins ()
  "A message arrives in parts, and only the parts that start a line indent.

Regression test.  A message is text, terms and traces inserted in turn;
indenting the start of every part set text in halfway along its own
line, and left the indentation stranded there."
  (with-temp-buffer
    (lean4-info--indented
      (lean4-info--insert "Type mismatch\n")
      ;; A term, mid-line, then the rest of the sentence.
      (lean4-info--insert "  \"not a number\"")
      (lean4-info--insert "\nhas type\n"))
    (should (equal (buffer-string)
                   "  Type mismatch\n    \"not a number\"\n  has type\n"))))

(ert-deftest lean4-info-indentation-keeps-the-text-properties ()
  "Indenting a goal does not cost it what it carries.

The goal text is propertized character by character, and that is what
`lean4-render', ElDoc and xref read back out of it."
  (let ((goal (concat (propertize "⊢ n" 'lean4-info 'handle)
                      "\n"
                      (propertize "  + 1" 'lean4-info 'handle))))
    (with-temp-buffer
      (lean4-info--indented (lean4-info--insert goal))
      (goto-char (point-min))
      (should (search-forward "⊢" nil t))
      (should (eq (get-text-property (1- (point)) 'lean4-info) 'handle))
      (goto-char (point-max))
      (should (eq (get-text-property (1- (point)) 'lean4-info) 'handle))
      ;; And the indentation itself carries nothing.
      (should-not (get-text-property (point-min) 'lean4-info)))))

(defun lean4-info-test--foldable-count ()
  "Return the number of sections in this buffer that have a body to fold."
  (let ((n 0))
    (letrec ((walk (lambda (section)
                     (when (oref section content) (cl-incf n))
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))
    n))



(defun lean4-info-test--insert-message (diagnostic buffer)
  "Insert DIAGNOSTIC for BUFFER into a fresh section tree, and return it."
  (with-current-buffer (get-buffer-create lean4-info-buffer-name)
    (let ((inhibit-read-only t))
      (unless (derived-mode-p 'magit-section-mode) (magit-section-mode))
      (erase-buffer)
      (magit-insert-section (magit-section 'root)
        (magit-insert-heading "root")
        (lean4-info--insert-message diagnostic buffer))
      )
    (buffer-string)))

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
            (should lean4-info--pins)
            (should (eq (marker-buffer
                         (lean4-info-pin-marker (car lean4-info--pins)))
                        source))
            (lean4-info--run-control #'lean4-info-toggle-pin)
            (should-not lean4-info--pins)))
      (lean4-info-unpin-all)
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

(ert-deftest lean4-info-message-caption-carries-the-badge ()
  "The caption names the section and counts what is in it.
Both message sections are captioned the same way -- the one for the
position and the one for the file."
  (cl-letf (((symbol-function 'lean4-info--displayable-p) (lambda (&rest _) t)))
    (should (equal (lean4-info--messages-caption "All messages"
                                                 '((:severity 1)))
                   "All messages (1 ⊗)"))
    (should (equal (lean4-info--messages-caption "Messages" '((:severity 2)))
                   "Messages (1 ⚠)")))
  ;; Nothing to count is not a state either section is inserted in, but
  ;; the caption should still read as a caption.
  (should (equal (lean4-info--messages-caption "Messages" nil) "Messages"))
  ;; No trailing colon: `magit-section' turns one into a child count,
  ;; which would follow the badge with a second count of the same thing.
  (should-not (string-suffix-p ":" (lean4-info--messages-caption
                                    "All messages" '((:severity 1))))))

(ert-deftest lean4-info-message-heading-names-its-file ()
  "A message says which file it is in, not just a bare pair of numbers."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (let ((text (lean4-info-test--insert-message
                     '(:range (:start (:line 6 :character 8)) :message "boom")
                     source)))
          ;; Line counted from one, column from zero, as LSP reports
          ;; them, and no trailing colon.
          (should (string-search "Named.lean:7:8" text))
          (should-not (string-search "Named.lean:7:8:" text))
          (should (string-search "boom" text))
          ;; The place is a label, not a control: clicking a heading
          ;; folds it, so the one thing that goes to the position has to
          ;; be the one control that says so.
          (let ((index (string-search "Named.lean:7:8" text)))
            (should-not (get-text-property index 'lean4-info-command text))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-a-click-does-what-return-does ()
  "`mouse-1' and RET ask the same question of the same spot.

That is how `mouse-1' behaves throughout Emacs -- on a button or a link
it acts, anywhere else it sets point -- and it is why folding is not
bound to it.  The goals are trees of subterms, and clicking one is how
the reader puts it under ElDoc; folding on a click took that away and
folded the subterm out of sight as well."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (insert "line one\nline two\n"))
          (lean4-info-test--insert-message
           '(:range (:start (:line 1 :character 0)) :message "boom") source)
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source)
            ;; A message heading: both go to the position it reports.
            (goto-char (point-min))
            (should (search-forward "Named.lean:2:0" nil t))
            (should (lean4-info--act-at (match-beginning 0)))
            ;; Nothing to act on: RET says so, and a click just moves.
            (goto-char (point-min))
            (should-not (lean4-info--act-at (point)))
            (should-error (lean4-info-return) :type 'user-error))
          ;; Folding is not among the things a click does, and not
          ;; something this binds at all: it is left to whatever
          ;; `magit-section' offers, so the display folds the way the
          ;; reader's Magit does.
          (should (eq (keymap-lookup lean4-info-section-map "<mouse-1>")
                      'lean4-info-mouse-1))
          (dolist (key '("<double-mouse-1>" "<left-fringe> <mouse-1>"
                         "<left-margin> <mouse-1>"))
            ;; `keymap-lookup' answers with a number, not nil, when the
            ;; sequence runs past what the keymap defines.
            (should-not (commandp (keymap-lookup lean4-info-section-map
                                                 key)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))


(ert-deftest lean4-info-sections-are-told-apart-across-a-rebuild ()
  "Folding one section does not fold its siblings when the display rebuilds.

Regression test.  `magit-section' carries visibility across a rebuild by
matching each new section against the old one with the same ident, and
an ident is the section's type and value.  Every message section shared
one value, so folding a message and then doing anything that rebuilds --
pinning, pausing, moving point -- folded every message in the display."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (cl-flet ((build ()
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (magit-insert-section (lean4-info-section 'root)
                        (dolist (place '("F.lean:1:0" "F.lean:2:0"
                                         "F.lean:3:0"))
                          (magit-insert-section
                              (lean4-info-section (list 'message place))
                            (magit-insert-heading place)
                            (magit-insert-section-body
                              (insert "body\n")))))))
                  (hidden ()
                    (mapcar (lambda (s) (and (oref s hidden) t))
                            (oref magit-root-section children))))
          (build)
          (magit-section-hide (nth 1 (oref magit-root-section children)))
          (should (equal (hidden) '(nil t nil)))
          (build)
          (should (equal (hidden) '(nil t nil)))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-each-message-is-a-section-of-its-own ()
  "A message folds by itself, so one long trace can be put away.

Regression test.  Messages used to be plain text inside one section, so
there was nothing to fold and no chevron to say there might be."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (lean4-info-test--insert-message
           '(:range (:start (:line 0 :character 0)) :message "boom") source)
          (with-current-buffer lean4-info-buffer-name
            (let ((message (car (last (oref magit-root-section children)))))
              ;; Keyed by where it is, so that `magit-section' can tell
              ;; one message from another when the display is rebuilt.
              (should (eq (car (oref message value)) 'message))
              (should (equal (cadr (oref message value)) "Named.lean:1:0"))
              ;; Content is what makes a section foldable, and what the
              ;; indicator is drawn from.
              (should (oref message content)))
            (lean4-info-test--with-fringe-indicators
              (lean4-info--add-visibility-indicators)
              (should (> (lean4-info-test--indicator-count) 0)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-message-heading-offers-a-way-there ()
  "The control beside a message sends point to it."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "line one\nline two\nline three\n"))
          (let* ((text (lean4-info-test--insert-message
                        '(:range (:start (:line 1 :character 5)))
                        source))
                 (index (string-search (lean4-info-goto-glyph) text)))
            (should index)
            (lean4-info--run-control
             (get-text-property index 'lean4-info-command text))
            (with-current-buffer source
              (should (= (line-number-at-pos) 2))
              (should (= (current-column) 5)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))


(ert-deftest lean4-info-goto-control-moves-point-in-its-window ()
  "Clicking the control really does leave point where it points.

Regression test.  With the Lean buffer actually on screen, moving point
with `set-buffer' moves the buffer's point and not the window's, and
selecting the window afterwards restores the window's own point -- so
the jump was made and then silently undone.  Without a window, or with
the Lean window already selected, the same code appeared to work."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (insert "line one\nline two\nline three\n")
            (goto-char (point-min)))
          (lean4-info-test--insert-message
           '(:range (:start (:line 1 :character 5))) source)
          ;; The arrangement a click really happens in: the Lean buffer
          ;; in one window, the info buffer in another, and the click
          ;; selecting the info window.
          (delete-other-windows)
          (set-window-buffer (selected-window) source)
          (select-window (split-window))
          (set-window-buffer (selected-window) lean4-info-buffer-name)
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source)
            (let* ((text (buffer-string))
                   (index (string-search (lean4-info-goto-glyph) text)))
              (should index)
              (lean4-info--run-control
               (get-text-property index 'lean4-info-command text))))
          (let ((window (get-buffer-window source)))
            (should window)
            ;; The window's point, not merely the buffer's: that is what
            ;; the reader is shown, and what selecting the window uses.
            (should (= (window-point window)
                       (with-current-buffer source (point))))
            (with-current-buffer source
              (should (= (line-number-at-pos) 2))
              (should (= (current-column) 5)))))
      (delete-other-windows)
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-sections-carry-the-display-keymap ()
  "Every section answers to `mouse-1', heading and body alike.

`magit-section' puts its own keymap over a section, and over a heading
line again whenever the section is folded, so a keymap on the text does
not survive.  Its `keymap' slot does, which is why every section here is
a `lean4-info-section'."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (lean4-info-section 'root)
            (magit-insert-section (lean4-info-section 'goals)
              (magit-insert-heading "Goals:")
              (magit-insert-section-body (insert "body\n")))))
        (let ((section (car (oref magit-root-section children))))
          (dolist (position (list (oref section start)
                                  (1- (oref section end))))
            (should (eq (keymap-lookup (get-text-property position 'keymap)
                                       "<mouse-1>")
                        'lean4-info-mouse-1)))
          ;; And again after folding, which is when `magit-section' would
          ;; have overwritten a keymap put on the text.
          (magit-section-hide section)
          (should (eq (keymap-lookup
                       (get-text-property (oref section start) 'keymap)
                       "<mouse-1>")
                      'lean4-info-mouse-1))))
    (kill-buffer lean4-info-buffer-name)))



(ert-deftest lean4-info-return-goes-to-the-thing-at-point ()
  "RET goes to a message's position, and falls through elsewhere.

TAB folds and RET goes to the thing at point, which is the division
`magit-section' uses.  It also restores by keyboard what the go-to
control offers by mouse, the file name being a label now."
  (let ((source (get-buffer-create "Named.lean"))
        (jumped nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (insert "line one\nline two\nline three\n"))
          (lean4-info-test--insert-message
           '(:range (:start (:line 1 :character 5))) source)
          (with-current-buffer lean4-info-buffer-name
            ;; Anywhere on the heading, not only on the control.
            (goto-char (point-min))
            (should (search-forward "Named.lean:2:5" nil t))
            (goto-char (match-beginning 0))
            (should (get-text-property (point) 'lean4-info-position))
            (lean4-info-return)
            (with-current-buffer source
              (should (= (line-number-at-pos) 2))
              (should (= (current-column) 5))))
          (with-current-buffer lean4-info-buffer-name
            ;; On a subterm it is xref's business.
            (goto-char (point-min))
            (let ((inhibit-read-only t))
              (put-text-property (point-min) (1+ (point-min))
                                 'lean4-info '(:dummy t)))
            (cl-letf (((symbol-function 'xref-find-definitions)
                       (lambda (&rest _) (interactive) (setq jumped t))))
              (lean4-info-return))
            (should jumped)
            ;; On neither, nothing -- handing this to xref sent it looking
            ;; for a tags table, which is not what was asked for.
            (let ((inhibit-read-only t))
              (remove-text-properties (point-min) (1+ (point-min))
                                      '(lean4-info nil)))
            (should-error (lean4-info-return) :type 'user-error)))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-return-presses-the-control-at-point ()
  "RET does what clicking a control would, so none of them needs a mouse."
  (with-temp-buffer
    (rename-buffer "Keys.lean" 'unique)
    (let* ((lean4-info--pin nil)
           (lean4-info-paused nil)
           (heading (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))
           (plain (substring-no-properties heading))
           (index (string-search (lean4-info-pause-glyph) plain)))
      (should index)
      (insert heading)
      (goto-char (+ (point-min) index))
      (should (eq (get-text-property (point) 'lean4-info-command)
                  'lean4-info-toggle-pause))
      (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
                ((symbol-function 'lean4-info-buffer-refresh) #'ignore))
        (lean4-info-return)
        (should lean4-info-paused)
        (lean4-info-return)
        (should-not lean4-info-paused)))))

(ert-deftest lean4-info-refresh-control-appears-only-while-paused ()
  "Nothing else leaves the display out of date, so nothing else offers this."
  (with-temp-buffer
    (rename-buffer "Refresh.lean" 'unique)
    (let ((lean4-info--pin nil))
      (let* ((lean4-info-paused nil)
             (heading (substring-no-properties
                       (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))))
        (should-not (string-search (lean4-info-refresh-glyph) heading)))
      (let* ((lean4-info-paused t)
             (heading (substring-no-properties
                       (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))))
        (should (string-search (lean4-info-refresh-glyph) heading))
        ;; Before the pause control it belongs to, and before the pin.
        (should (< (string-search (lean4-info-refresh-glyph) heading)
                   (string-search (lean4-info-pin-glyph) heading)))))))

(ert-deftest lean4-info-messages-are-ordered-by-where-they-start ()
  "Two messages about one declaration come out as they are written.

Ordering by where a message ends put the completed-proof report after
the trace it belongs with; VS Code has them the other way, which is the
order they start in."
  (let ((a '(:range (:start (:line 40 :character 0) :end (:line 44 :character 0))))
        (b '(:range (:start (:line 41 :character 0) :end (:line 42 :character 0))))
        (lean4-info-message-order 'location))
    (should (equal (lean4-info--sort-messages (list b a) 0) (list a b)))))

(ert-deftest lean4-info-messages-can-be-ordered-by-nearness-to-point ()
  "VS Code's \"sort by proximity to text cursor\", under Emacs's name for it."
  (let* ((near '(:range (:start (:line 40 :character 0))))
         (far '(:range (:start (:line 4 :character 0))))
         (both (list far near)))
    (let ((lean4-info-message-order 'location))
      (should (equal (lean4-info--sort-messages both 40) (list far near))))
    (let ((lean4-info-message-order 'point))
      (should (equal (lean4-info--sort-messages both 40) (list near far))))))

(ert-deftest lean4-info-sort-control-keeps-one-glyph ()
  "One glyph for both orders, as in VS Code; the face carries the state."
  (let ((by-file (let ((lean4-info-message-order 'location))
                   (lean4-info-sort-glyph)))
        (by-point (let ((lean4-info-message-order 'point))
                    (lean4-info-sort-glyph))))
    (should (equal by-file by-point)))
  (let ((lean4-info-sort-icon "%"))
    (should (equal (lean4-info-sort-glyph) "%"))))

(ert-deftest lean4-info-caption-can-carry-controls ()
  "The file's message heading has room for its own controls."
  (let ((caption (lean4-info--messages-caption
                  "All messages" '((:severity 1)) "XY")))
    (should (string-prefix-p "All messages (" (substring-no-properties caption)))
    (should (string-suffix-p "XY" (substring-no-properties caption)))))

(ert-deftest lean4-info-goto-position-clamps-a-stale-column ()
  "A column past the end of its line does not signal.
The file can have been edited since the message was made."
  (with-temp-buffer
    (insert "ab\ncd\n")
    (lean4-info--goto-position 1 99)
    (should (= (point) 3))))

(ert-deftest lean4-info-only-a-pinned-section-offers-a-way-back ()
  "The way back belongs to a pinned section.
The followed section names where point already is, so the control would
lead where point is and only take up room."
  (with-temp-buffer
    (rename-buffer "Back.lean" 'unique)
    (insert "one
two
")
    (let ((lean4-info-paused nil)
          (pin (lean4-info--pin-create :marker (copy-marker (point-min)))))
      (should-not (string-search
                   (lean4-info-goto-glyph)
                   (substring-no-properties
                    (lean4-info--heading (lean4-info--location-string)
                                         (lean4-info--controls)
                                         (lean4-info--point-state)))))
      (should (string-search
               (lean4-info-goto-glyph)
               (substring-no-properties
                (lean4-info--heading "x" (lean4-info--pin-controls pin)
                                     "pinned"))))
      (set-marker (lean4-info-pin-marker pin) nil))))



(ert-deftest lean4-info-each-pin-pauses-on-its-own ()
  "A pinned section freezes by itself, and says so.

Pinning a position and freezing it are different things: a pinned goal
follows its declaration through an edit unless told otherwise.  VS Code
gives every pinned section its own pause, and so does this."
  (with-temp-buffer
    (rename-buffer "Pins.lean" 'unique)
    (let* ((one (lean4-info--pin-create :marker (copy-marker (point-min))))
           (two (lean4-info--pin-create :marker (copy-marker (point-min))))
           (lean4-info--pins (list one two)))
      (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
                ((symbol-function 'lean4-info--refresh-pin) #'ignore))
        (lean4-info-toggle-pin-pause one)
        (should (lean4-info-pin-paused one))
        (should-not (lean4-info-pin-paused two))
        (lean4-info-toggle-pin-pause one)
        (should-not (lean4-info-pin-paused one)))
      ;; The heading says which, and offers a refresh only when paused.
      (setf (lean4-info-pin-paused one) t)
      (let ((heading (substring-no-properties
                      (lean4-info--heading
                       "x" (lean4-info--pin-controls one)
                       "pinned and paused"))))
        (should (string-search "pinned and paused" heading))
        (should (string-search (lean4-info-refresh-glyph) heading))
        (should (string-search (lean4-info-resume-glyph) heading)))
      (setf (lean4-info-pin-paused one) nil)
      (let ((heading (substring-no-properties
                      (lean4-info--heading "x" (lean4-info--pin-controls one)
                                           "pinned"))))
        (should-not (string-search (lean4-info-refresh-glyph) heading))
        (should (string-search (lean4-info-pause-glyph) heading)))
      (set-marker (lean4-info-pin-marker one) nil)
      (set-marker (lean4-info-pin-marker two) nil))))

(ert-deftest lean4-info-a-paused-pin-is-not-refetched ()
  "Pausing a pin is what stops it being fetched again."
  (with-temp-buffer
    (let ((pin (lean4-info--pin-create :marker (point-marker)))
          (fetched nil))
      (cl-letf (((symbol-function 'lean4-info--refresh-pin-interactive)
                 (lambda (&rest _) (setq fetched t)))
                ((symbol-function 'eglot-current-server) (lambda () t)))
        (let ((lean4-info-interactive t))
          (lean4-info--refresh-pin pin)
          (should fetched)
          (setq fetched nil)
          (setf (lean4-info-pin-paused pin) t)
          (lean4-info--refresh-pin pin)
          (should-not fetched)
          ;; Asked for outright, it is fetched even so.
          (lean4-info-refresh-pin pin)
          (should fetched)))
      (set-marker (lean4-info-pin-marker pin) nil))))

(ert-deftest lean4-info-each-section-reports-its-own-state ()
  "Pinned and paused are different things and belong to different sections.
Pinned says a section is not following point; paused says the display is
not updating at all."
  (with-temp-buffer
    (rename-buffer "Both.lean" 'unique)
    (let ((pin (lean4-info--pin-create :marker (copy-marker (point-min)))))
      (let ((lean4-info-paused nil))
        (should-not (lean4-info--point-state))
        (should (string-search
                 "pinned"
                 (substring-no-properties
                  (lean4-info--heading "x" (lean4-info--pin-controls pin)
                                       "pinned")))))
      (let ((lean4-info-paused t))
        (should (equal (lean4-info--point-state) "paused"))
        (should (string-search
                 "paused"
                 (substring-no-properties
                  (lean4-info--heading (lean4-info--location-string)
                                       (lean4-info--controls)
                                       (lean4-info--point-state))))))
      (set-marker (lean4-info-pin-marker pin) nil))))


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
                                (lean4-info--heading (lean4-info--location-string)
                                 (lean4-info--controls)
                                 (lean4-info--point-state))))))))

(ert-deftest lean4-info-pinned-section-reports-its-own-position ()
  "A pinned section names where it was pinned, whatever point does."
  (with-temp-buffer
    (rename-buffer "Bar.lean" 'unique)
    (insert "one
two
three
")
    (goto-char (point-min))
    (forward-line 1)
    (let ((pin (lean4-info--pin-create :marker (point-marker))))
      ;; Point moves away; the pinned section does not.
      (goto-char (point-max))
      (let ((heading (substring-no-properties
                      (lean4-info--heading
                       (lean4-info--marker-location-string
                        (lean4-info-pin-marker pin))
                       (lean4-info--pin-controls pin)
                       "pinned"))))
        (should (string-prefix-p "Bar.lean:2:0" heading))
        (should (string-search "pinned" heading)))
      ;; While the followed section reports point.
      (should (string-prefix-p
               "Bar.lean:4:0"
               (substring-no-properties
                (lean4-info--heading (lean4-info--location-string)
                                     (lean4-info--controls)
                                     (lean4-info--point-state)))))
      (set-marker (lean4-info-pin-marker pin) nil))))


(ert-deftest lean4-info-heading-carries-clickable-controls ()
  "The controls are in the heading and run their commands when clicked."
  (with-temp-buffer
    (rename-buffer "Baz.lean" 'unique)
    (let* ((lean4-info--pin nil)
           (lean4-info-paused nil)
           (heading (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))
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

(ert-deftest lean4-info-sections-carry-the-control-that-fits-them ()
  "A pinned section unpins; the followed one pins."
  (with-temp-buffer
    (rename-buffer "Qux.lean" 'unique)
    (let* ((lean4-info-paused nil)
           (pin (lean4-info--pin-create :marker (copy-marker (point-min))))
           (pinned (substring-no-properties
                    (lean4-info--heading "x" (lean4-info--pin-controls pin)
                                         "pinned")))
           (followed (substring-no-properties
                      (lean4-info--heading (lean4-info--location-string)
                                           (lean4-info--controls)
                                           (lean4-info--point-state)))))
      (should (string-search (lean4-info-unpin-glyph) pinned))
      (should-not (string-search (lean4-info-pin-glyph) pinned))
      (should (string-search (lean4-info-pin-glyph) followed))
      (should-not (string-search (lean4-info-unpin-glyph) followed))
      (set-marker (lean4-info-pin-marker pin) nil))))


(ert-deftest lean4-info-controls-fall-back-to-ascii ()
  "The controls are configurable, for fonts without the glyphs.
Emacs runs in terminals and on machines with no emoji font, so the
defaults are chosen with `char-displayable-p' and can be overridden."
  (let ((lean4-info-pin-icon "P")
        (lean4-info-pause-icon "||")
        (lean4-info--pin nil)
        (lean4-info-paused nil))
    (let ((plain (substring-no-properties
                    (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))))
      (should (string-search "P" plain))
      (should (string-search "||" plain)))))

(ert-deftest lean4-info-pause-control-shows-what-it-will-do ()
  "The pause control becomes a play symbol while paused."
  (with-temp-buffer
    (rename-buffer "Icons.lean" 'unique)
    (let ((lean4-info-paused nil))
      (should (string-search (lean4-info-pause-glyph)
                             (substring-no-properties
                              (lean4-info--heading
                               (lean4-info--location-string)
                               (lean4-info--controls)
                               (lean4-info--point-state))))))
    (let ((lean4-info-paused t))
      (let ((plain (substring-no-properties
                    (lean4-info--heading (lean4-info--location-string)
                                   (lean4-info--controls)
                                   (lean4-info--point-state)))))
        (should (string-search (lean4-info-resume-glyph) plain))
        (should-not (string-search (lean4-info-pause-glyph) plain))))))

(ert-deftest lean4-info-engaged-controls-are-faced-apart ()
  "A control whose state is engaged is faced differently.
The channel that survives a frame with no glyph to swap to."
  (with-temp-buffer
    (rename-buffer "Faces.lean" 'unique)
    (let ((pin (lean4-info--pin-create :marker (copy-marker (point-min))))
          (lean4-info-paused nil))
      (cl-flet ((face-of (heading glyph)
                  (let ((index (string-search
                                glyph (substring-no-properties heading))))
                    (should index)
                    (get-text-property index 'face heading))))
        (should (eq (face-of (lean4-info--heading
                              (lean4-info--location-string)
                              (lean4-info--controls)
                              (lean4-info--point-state))
                             (lean4-info-pin-glyph))
                    'lean4-info-button))
        (should (eq (face-of (lean4-info--heading
                              "x" (lean4-info--pin-controls pin) "pinned")
                             (lean4-info-unpin-glyph))
                    'lean4-info-button-active)))
      (set-marker (lean4-info-pin-marker pin) nil))))


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
