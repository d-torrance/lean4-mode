;;; lean4-info-test.el --- Tests for the goal display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the parts of the goal display that need no server.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lean4-mode)

(defun lean4-info-test--chevrons ()
  "Return the indentation and fold indicator drawn on each heading.

Both live in the `display' of the heading\='s first character, which the
character itself follows; drop that to leave what is drawn in front of
it."
  (let (found)
    (letrec ((walk (lambda (section)
                     (when (lean4-info--foldable-p section)
                       (let ((shown (get-text-property (oref section start)
                                                       'display)))
                         (push (and shown (substring shown 0 -1)) found)))
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))
    (nreverse found)))

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

Regression test.  magit-section updates its own indicators only from
`magit-section-show' and `magit-section-hide', so nothing inserted
carried one until it had been toggled: the display looked as though
nothing folded, and folding one section made that one section -- and
only that one -- grow a chevron."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-heading "Root:")
            (magit-insert-section (magit-section 'goals)
              (magit-insert-heading "Goals:")
              (magit-insert-section-body (insert "one goal\n")))))
        (lean4-info--paint-chevrons)
        (should (> (lean4-info-test--foldable-count) 0))
        (should (= (length (lean4-info-test--chevrons))
                   (lean4-info-test--foldable-count)))
        (should (seq-every-p #'identity (lean4-info-test--chevrons))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-chevrons-are-indented-with-their-heading ()
  "The indicator sits with the text it is about, not at the frame edge.

Regression test.  magit-section draws in the fringe, which cannot be
indented: a section set in from its parent had its text move right while
its indicator stayed put, so the two drifted apart."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-heading "Root:")
            (lean4-info--indented
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body (insert "one goal\n"))))))
        (lean4-info--paint-chevrons)
        (pcase-let ((`(,open . ,_) (lean4-info-chevron-pair))
                    (`(,root ,goals) (lean4-info-test--chevrons)))
          ;; The outer heading is not set in; the inner one is, and its
          ;; indicator is set in with it rather than left behind.
          (should (equal root open))
          (should (equal goals (concat "  " open)))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-chevrons-follow-folding ()
  "Folding a section turns its indicator round, without being told."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-section (magit-section 'goals)
              (magit-insert-heading "Goals:")
              (magit-insert-section-body (insert "body\n")))))
        (lean4-info--paint-chevrons)
        (setq lean4-info--folds (lean4-info--fold-state))
        (let ((section (car (oref magit-root-section children))))
          (pcase-let ((`(,open . ,closed) (lean4-info-chevron-pair)))
            (should (equal (car (lean4-info-test--chevrons)) open))
            (magit-section-hide section)
            ;; No hook runs when magit folds; the repaint notices instead.
            (lean4-info--repaint-chevrons)
            (should (equal (car (lean4-info-test--chevrons)) closed))
            (magit-section-show section)
            (lean4-info--repaint-chevrons)
            (should (equal (car (lean4-info-test--chevrons)) open)))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-heading-below-a-folded-one-keeps-its-chevron ()
  "Folding one section does not take the indicator off the next.

Regression test.  The indicator was drawn in the line\='s `line-prefix',
and folding a section makes `magit-section' cover its body including the
newline before the next heading -- which leaves that heading at the edge
of an invisible run, where a `line-prefix' stops being drawn.  The
heading below a folded one lost its chevron, and its indentation with
it."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (lean4-info--indented
              (magit-insert-section (magit-section 'a)
                (magit-insert-heading "A heading")
                (magit-insert-section-body (insert "body of A\n")))
              (magit-insert-section (magit-section 'b)
                (magit-insert-heading "B heading")
                (magit-insert-section-body (insert "body of B\n"))))))
        (lean4-info--paint-chevrons)
        (pcase-let ((`(,open . ,closed) (lean4-info-chevron-pair))
                    (a (nth 0 (oref magit-root-section children))))
          (should (equal (lean4-info-test--chevrons)
                         (list (concat "  " open) (concat "  " open))))
          (magit-section-hide a)
          (lean4-info--paint-chevrons)
          ;; The folded one turns round; the one below keeps both its
          ;; indicator and its indentation.
          (should (equal (lean4-info-test--chevrons)
                         (list (concat "  " closed) (concat "  " open))))
          ;; And is indented once, not twice.  A heading following a
          ;; folded section continues that section's display line, and a
          ;; continued line is drawn with its `wrap-prefix' -- so a
          ;; heading carrying its indentation in both places got it
          ;; twice.
          (dolist (section (list a (nth 1 (oref magit-root-section
                                                children))))
            (let ((start (oref section start)))
              (should (equal (get-text-property start 'line-prefix) ""))
              (should (equal (get-text-property start 'wrap-prefix) ""))))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-folding-leaves-no-chevron-hanging ()
  "Folding a section leaves no indicator behind under it.

Regression test.  A folded section\='s children are still in the buffer,
merely invisible, and a `line-prefix' at the edge of an invisible run is
drawn anyway -- which showed as a chevron hanging under a section that
had just been folded shut."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-section (magit-section 'parent)
              (magit-insert-heading "Parent:")
              (magit-insert-section-body
                (lean4-info--indented
                  (magit-insert-section (magit-section 'child)
                    (magit-insert-heading "Child:")
                    (magit-insert-section-body (insert "body\n"))))))))
        (lean4-info--paint-chevrons)
        (setq lean4-info--folds (lean4-info--fold-state))
        (pcase-let ((`(,open . ,closed) (lean4-info-chevron-pair))
                    (parent (car (oref magit-root-section children))))
          (should (equal (lean4-info-test--chevrons)
                         (list open (concat "  " open))))
          (magit-section-hide parent)
          (lean4-info--repaint-chevrons)
          ;; The parent turns round; the child is drawn with nothing at
          ;; all.  Emacs draws a `display' string even on an invisible
          ;; character, so anything left on the hidden heading -- its
          ;; indentation, its first letter -- would appear on screen with
          ;; the rest of the heading gone.
          (should (equal (lean4-info-test--chevrons) (list closed nil)))
          (magit-section-show parent)
          (lean4-info--repaint-chevrons)
          (should (equal (lean4-info-test--chevrons)
                         (list open (concat "  " open))))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-indentation-accumulates ()
  "Nested levels add up rather than replacing one another.

The outer level is applied last, so it has to add itself in front of
what the inner ones left."
  (with-temp-buffer
    (lean4-info--indented
      (insert "one\n")
      (lean4-info--indented (insert "two\n")))
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'lean4-info-indent) "  "))
    (forward-line 1)
    (should (equal (get-text-property (point) 'lean4-info-indent) "    "))
    ;; And still no spaces in the text itself.
    (should (equal (buffer-string) "one\ntwo\n"))))

(ert-deftest lean4-info-chevron-pair-suits-the-frame ()
  "The pair is chosen for the frame, and is configurable."
  (cl-letf (((symbol-function 'lean4-info--displayable-p) (lambda (&rest _) t)))
    (should (equal (lean4-info-chevron-pair) '("▾ " . "▸ "))))
  (cl-letf (((symbol-function 'lean4-info--displayable-p) #'ignore))
    (should (equal (lean4-info-chevron-pair) '("- " . "+ "))))
  (let ((lean4-info-chevrons '("D " . "R ")))
    (should (equal (lean4-info-chevron-pair) '("D " . "R ")))))

(defun lean4-info-test--insert-message (diagnostic buffer)
  "Insert DIAGNOSTIC for BUFFER into a fresh section tree, and return it."
  (with-current-buffer (get-buffer-create lean4-info-buffer-name)
    (let ((inhibit-read-only t))
      (unless (derived-mode-p 'magit-section-mode) (magit-section-mode))
      (erase-buffer)
      (magit-insert-section (magit-section 'root)
        (magit-insert-heading "root")
        (lean4-info--insert-message diagnostic buffer))
      ;; As the real build does, and for the same reason: `magit-section'
      ;; replaces the keymap on a heading line, controls included.
      (lean4-info--restore-control-keymaps))
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
              (should (eq (oref message value) 'message))
              ;; Content is what makes a section foldable, and what the
              ;; indicator is drawn from.
              (should (oref message content)))
            (lean4-info--paint-chevrons)
            (should (member (car (lean4-info-chevron-pair))
                            (lean4-info-test--chevrons)))))
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
            (funcall (keymap-lookup (get-text-property index 'keymap text)
                                    "<mouse-1>"))
            (with-current-buffer source
              (should (= (line-number-at-pos) 2))
              (should (= (current-column) 5)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-controls-in-a-heading-stay-clickable ()
  "A control on a heading line keeps its binding.

Regression test.  `magit-section-maybe-add-heading-map' puts its own
keymap over the whole of a heading line, which replaced the one each
control carries: the control could be seen, described and hovered, but
clicking it did nothing.  The other properties survive, which is what
makes the repair possible."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (let* ((text (lean4-info-test--insert-message
                      '(:range (:start (:line 0 :character 0))) source))
               (index (string-search (lean4-info-goto-glyph) text))
               (map (get-text-property index 'keymap text)))
          (should map)
          (should (commandp (keymap-lookup map "<mouse-1>")))
          ;; Composed over magit's rather than replacing it, so folding a
          ;; section by mouse still works.
          (should (eq (keymap-lookup map "<double-mouse-1>")
                      'magit-mouse-toggle-section)))
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
              (funcall (keymap-lookup (get-text-property index 'keymap text)
                                      "<mouse-1>"))))
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

(ert-deftest lean4-info-clicking-a-heading-folds-it ()
  "A single click anywhere on a heading folds the section it heads."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-section (magit-section 'goals)
              (magit-insert-heading "Goals:")
              (magit-insert-section-body (insert "body\n")))))
        (lean4-info--add-heading-clicks)
        (let ((section (car (oref magit-root-section children))))
          (should-not (oref section hidden))
          (goto-char (oref section start))
          ;; Everywhere on the heading, not merely at its start: the
          ;; chevron is drawn in the line's `line-prefix', where a click
          ;; resolves to the line's first character.
          (dolist (pos (list (oref section start)
                             (+ 3 (oref section start))))
            (should (eq (keymap-lookup (get-text-property pos 'keymap)
                                       "<mouse-1>")
                        'lean4-info-mouse-toggle-section)))
          (magit-section-toggle section)
          (should (oref section hidden))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-no-chevron-on-a-section-that-cannot-fold ()
  "A heading with an empty body gets no indicator.
Folding it would do nothing, so saying it can be folded is a lie."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (magit-insert-section (magit-section 'empty)
              (magit-insert-heading "Empty:"))
            (magit-insert-section (magit-section 'full)
              (magit-insert-heading "Full:")
              (magit-insert-section-body (insert "body\n")))))
        (lean4-info--paint-chevrons)
        (should (equal (lean4-info-test--chevrons)
                       (list (car (lean4-info-chevron-pair))))))
    (kill-buffer lean4-info-buffer-name)))

(ert-deftest lean4-info-controls-survive-a-fold ()
  "Folding leaves the heading and its controls still clickable.

Regression test.  `magit-section' re-applies its own keymap over a
heading line whenever the section is folded, which took away both the
click that folds it and any control sitting on that line."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (lean4-info-test--insert-message
           '(:range (:start (:line 0 :character 0)) :message "boom") source)
          (with-current-buffer lean4-info-buffer-name
            (lean4-info--paint-chevrons)
            (lean4-info--add-heading-clicks)
            (lean4-info--restore-control-keymaps)
            (setq lean4-info--folds (lean4-info--fold-state))
            (let ((section (car (last (oref magit-root-section children)))))
              (magit-section-hide section)
              (lean4-info--repaint-chevrons)
              ;; The heading still folds, and the control on it still
              ;; does its own thing rather than folding.
              (should (eq (keymap-lookup
                           (get-text-property (oref section start) 'keymap)
                           "<mouse-1>")
                          'lean4-info-mouse-toggle-section))
              (let ((index (string-search (lean4-info-goto-glyph)
                                          (buffer-string))))
                (should index)
                (should (commandp
                         (keymap-lookup
                          (get-text-property index 'keymap (buffer-string))
                          "<mouse-1>")))
                (should-not
                 (eq (keymap-lookup
                      (get-text-property index 'keymap (buffer-string))
                      "<mouse-1>")
                     'lean4-info-mouse-toggle-section))))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

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


(ert-deftest lean4-info-indents-without-touching-the-text ()
  "Indentation is a display property, not spaces in the buffer.

The goal text carries the positions `lean4-render', ElDoc and xref read
back out of it; inserting characters into it would move every one."
  (with-temp-buffer
    (lean4-info--indented (insert "goal\nmore\n"))
    (should (equal (buffer-string) "goal\nmore\n"))
    (should (equal (get-text-property (point-min) 'line-prefix) "  "))
    (should (equal (get-text-property (point-min) 'wrap-prefix) "  "))))

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
