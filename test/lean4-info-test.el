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

(ert-deftest lean4-info-buffer-has-a-mode-of-its-own ()
  "The goal display is a major mode, not a buffer configured by hand.

That is what makes \\[describe-mode] list its bindings, gives the reader
`lean4-info-mode-hook', and lets `display-buffer-alist' match on the mode
rather than on the buffer's name."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (should (derived-mode-p 'lean4-info-mode))
        ;; Sections, folding and read-only come from the parent.
        (should (derived-mode-p 'magit-section-mode))
        (should buffer-read-only)
        (should (eq (keymap-lookup nil "TAB") 'magit-section-toggle))
        ;; Its own bindings win over the parent's.
        (should (eq (keymap-lookup nil "RET") 'lean4-info-return))
        (should (eq (syntax-table) lean4-mode-syntax-table))
        ;; `g' is `revert-buffer' here, from `special-mode', and reverting
        ;; a buffer with no file behind it fails unless it is told what to
        ;; do instead.
        (should (eq (keymap-lookup nil "g") 'revert-buffer))
        (let ((called nil))
          (cl-letf (((symbol-function 'lean4-info-refresh-paused)
                     (lambda () (setq called t))))
            (revert-buffer))
          (should called)))
    (kill-buffer lean4-info-buffer-name)))

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

Regression test.  A control pressed by mouse runs its command with the
info buffer current, where pinning is meaningless -- `lean4-info-toggle-pin' needs
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
  (cl-letf (((symbol-function 'lean4--displayable-p) (lambda (&rest _) t)))
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
  (cl-letf (((symbol-function 'lean4--displayable-p) #'ignore))
    (should (equal (lean4-info--severity-badge
                    '((:severity 1) (:severity 3)))
                   "1 E  1 I"))))

(ert-deftest lean4-info-message-caption-carries-the-badge ()
  "The caption names the section and counts what is in it.
Both message sections are captioned the same way -- the one for the
position and the one for the file."
  (cl-letf (((symbol-function 'lean4--displayable-p) (lambda (&rest _) t)))
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

(ert-deftest lean4-info-the-display-answers-to-the-mouse-as-buttons-do ()
  "The display puts back the `button.el' bindings `magit-section' shadows.

A button carries `button-map' on its `keymap' property, which
`magit-section' overwrites, so the section keymap holds the two parts of
it a control needs: `mouse-2' to press one, and the `follow-link'
binding that lets `mouse-1-click-follows-link' reach `mouse-2'.  Folding
stays with whatever `magit-section' offers."
  (should (eq (keymap-lookup lean4-info-section-map "<mouse-2>")
              'push-button))
  (should (eq (keymap-lookup lean4-info-section-map "<follow-link>")
              'mouse-face))
  (dolist (key '("<mouse-1>" "<double-mouse-1>" "<left-fringe> <mouse-1>"
                 "<left-margin> <mouse-1>"))
    ;; `keymap-lookup' answers with a number, not nil, when the sequence
    ;; runs past what the keymap defines.
    (should-not (commandp (keymap-lookup lean4-info-section-map key)))))

(ert-deftest lean4-info-a-control-is-the-only-thing-a-click-presses ()
  "The controls highlight under the pointer, and nothing else does.

That highlight is what the `follow-link' binding answers with, so it is
what decides where `mouse-1' presses rather than moves point: the go-to
control is a link, and the position it names, the message under it and a
subterm are all ordinary text."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (insert "line one\nline two\n"))
          (lean4-info-test--insert-message
           '(:range (:start (:line 1 :character 0)) :message "boom") source)
          (with-current-buffer lean4-info-buffer-name
            (goto-char (point-min))
            (should (search-forward (lean4-info-goto-glyph) nil t))
            (should (get-text-property (match-beginning 0) 'mouse-face))
            (goto-char (point-min))
            (should (search-forward "Named.lean:2:0" nil t))
            (should-not (get-text-property (match-beginning 0) 'mouse-face))
            (goto-char (point-min))
            (should (search-forward "boom" nil t))
            (should-not (get-text-property (match-beginning 0) 'mouse-face))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-pressing-a-control-goes-and-hands-the-focus-back ()
  "`push-button' at a control runs it, which is how the mouse arrives.

`mouse-2' presses the button, and `mouse-1' on a link is translated into
`mouse-2' before it gets here, so both end up in `push-button'."
  (let ((source (get-buffer-create "Named.lean")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (insert "line one\nline two\n")
            (goto-char (point-min)))
          (lean4-info-test--insert-message
           '(:range (:start (:line 1 :character 0)) :message "boom") source)
          ;; The arrangement a click really happens in: the Lean buffer in
          ;; one window and the info buffer in another.
          (delete-other-windows)
          (set-window-buffer (selected-window) source)
          (select-window (split-window))
          (set-window-buffer (selected-window) lean4-info-buffer-name)
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source)
            (goto-char (point-min))
            (should (search-forward (lean4-info-goto-glyph) nil t))
            (should (push-button (match-beginning 0))))
          (should (eq (selected-window) (get-buffer-window source)))
          (with-current-buffer source
            (should (= (line-number-at-pos) 2))))
      (delete-other-windows)
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-a-subterm-is-left-to-eldoc-and-to-xref ()
  "A subterm is ordinary text: a click sets point on it, under ElDoc.

Its definition is `\\[xref-find-definitions]', which the mode offers as
an xref backend, and which VS Code spells Ctrl-click.  RET leaves it
alone as well."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (lean4-info-section 'root)
            (insert (propertize "Nat.succ" 'lean4-info '(:p 1)) "\n")))
        ;; Not a link, so `mouse-1' stays `mouse-set-point' here.
        (should-not (get-text-property (point-min) 'mouse-face))
        (should-not (button-at (point-min)))
        (let ((jumps 0))
          (cl-letf (((symbol-function 'xref-find-definitions)
                     (lambda (&rest _) (interactive) (cl-incf jumps))))
            (goto-char (point-min))
            (should-not (lean4-info-return))
            (should (= jumps 0)))))
    (kill-buffer lean4-info-buffer-name)))


(ert-deftest lean4-info-a-childless-trace-does-not-look-foldable ()
  "A trace node with no children gets no body, and so no indicator.

Lean says which is which: a leaf arrives as `strict' with an empty
array, and only a node that really has children arrives as `lazy'.
Giving a leaf a body would promise something to unfold and then unfold
nothing."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (lean4-info-section 'root)
            ;; Collapsed, which is how most leaves arrive: that is the
            ;; case that used to get an indicator, because a section
            ;; starting folded has its body put aside as a washer, and a
            ;; washer is something `magit-section' says it can unfold.
            (lean4-info--insert-trace
             '(:children (:strict []) :cls "Leaf" :collapsed t
               :msg (:text "a leaf"))
             '(0))
            (lean4-info--insert-trace
             '(:children (:lazy (:__rpcref "1")) :cls "Lazy" :collapsed t
               :msg (:text "has children"))
             '(1))
            (lean4-info--insert-trace
             '(:children (:strict [(:text "kid")]) :cls "Strict" :collapsed t
               :msg (:text "has children too"))
             '(2))))
        (pcase-let ((`(,leaf ,lazy ,strict)
                     (oref magit-root-section children)))
          ;; A leaf is its heading and nothing else, so `magit-section'
          ;; has nothing to hide and draws no indicator.
          (should (= (oref leaf content) (oref leaf end)))
          (should-not (oref leaf washer))
          ;; A node whose children Lean has not sent starts folded, so
          ;; `magit-section' keeps its body as a washer and runs it when
          ;; the reader opens the section -- which is what it counts as
          ;; foldable, and draws the indicator for.
          (should (oref lazy hidden))
          (should (oref lazy washer))
          ;; And one whose children came with the message, folded shut.
          (should (oref strict washer))))
    (kill-buffer lean4-info-buffer-name)))

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

(ert-deftest lean4-info-follows-point-moved-from-outside-the-lean-buffer ()
  "Point moving in the Lean buffer updates the display, whoever moved it.

Regression test.  The watch for point moving was a hook local to the
Lean buffer, so it only ever saw commands run there: pressing a go-to
control in the display moved point in the file from the display's own
buffer, and the display went on reporting on the position point had just
left until the Lean buffer was given a command of its own."
  (let ((source (get-buffer-create "*lean4-info-test-source*"))
        scheduled)
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (cl-letf (((symbol-function 'lean4-info-buffer-redisplay-debounced)
                   (lambda () (push (current-buffer) scheduled))))
          (with-current-buffer source
            (let ((lean4-mode-hook nil)
                  (lean4-auto-start-server nil)
                  (lean4-info-auto-open nil))
              (lean4-mode))
            (insert "example : True := trivial\n")
            (goto-char (point-min)))
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source)
            ;; Bring the display up to date with the file as it stands.
            (lean4-info--follow-point)
            (should (equal scheduled (list source)))
            ;; Nothing has moved, so there is nothing to do: the hook runs
            ;; after every command in every buffer, and an update rebuilds
            ;; the display and asks the server for goals afresh.
            (setq scheduled nil)
            (lean4-info--follow-point)
            (should-not scheduled)
            ;; A control in the display sends point elsewhere in the file
            ;; without ever leaving the display's own buffer.
            (with-current-buffer source (goto-char (point-max)))
            (lean4-info--follow-point)
            (should (equal scheduled (list source)))
            ;; So does an edit that leaves point where it is.
            (setq scheduled nil)
            (with-current-buffer source
              (save-excursion (insert "example : True := trivial\n")))
            (lean4-info--follow-point)
            (should (equal scheduled (list source)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-follows-a-buffer-it-has-been-away-from ()
  "Returning to a Lean buffer brings its goals back, point or no point.

The display shows one buffer at a time, so coming back to a file it has
left has to be reported even though point in that file has not moved
since -- which is what happens whenever the reader looks at one file and
returns to another."
  (let ((source (get-buffer-create "*lean4-info-test-source*"))
        (other (get-buffer-create "*lean4-info-test-other*"))
        scheduled)
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (cl-letf (((symbol-function 'lean4-info-buffer-redisplay-debounced)
                   (lambda () (push (current-buffer) scheduled))))
          (dolist (buffer (list source other))
            (with-current-buffer buffer
              (let ((lean4-mode-hook nil)
                    (lean4-auto-start-server nil)
                    (lean4-info-auto-open nil))
                (lean4-mode))
              (insert "example : True := trivial\n")))
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source))
          (with-current-buffer source
            (lean4-info--follow-point)
            (should (equal scheduled (list source))))
          ;; The reader looks at the other file, which the display takes up.
          (setq scheduled nil)
          (with-current-buffer other
            (lean4-info--follow-point)
            (should (equal scheduled (list other))))
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer other))
          ;; And comes back.  Point in the first file is where it was.
          (setq scheduled nil)
          (with-current-buffer source
            (lean4-info--follow-point)
            (should (equal scheduled (list source)))))
      (kill-buffer source)
      (kill-buffer other)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-updates-the-buffer-it-shows-from-any-window ()
  "The display is due an update while the reader is reading the display.

Its own window being selected is what a go-to control leaves behind, and
what reading a long goal means.  The buffer whose goals are shown is
still the one to report on; another Lean buffer, which the display is
not following, is not."
  (let ((source (get-buffer-create "*lean4-info-test-source*"))
        (other (get-buffer-create "*lean4-info-test-other*")))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (unwind-protect
        (progn
          (with-current-buffer lean4-info-buffer-name
            (setq lean4-info--source-buffer source))
          (delete-other-windows)
          (set-window-buffer (selected-window) lean4-info-buffer-name)
          (with-current-buffer source
            (should (lean4-info-buffer-active lean4-info-buffer-name)))
          (with-current-buffer other
            (should-not (lean4-info-buffer-active lean4-info-buffer-name))))
      (delete-other-windows)
      (kill-buffer source)
      (kill-buffer other)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-info-sections-carry-the-display-keymap ()
  "Every section carries the button bindings, heading and body alike.

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
                                       "<mouse-2>")
                        'push-button)))
          ;; And again after folding, which is when `magit-section' would
          ;; have overwritten a keymap put on the text.
          (magit-section-hide section)
          (should (eq (keymap-lookup
                       (get-text-property (oref section start) 'keymap)
                       "<mouse-2>")
                      'push-button))))
    (kill-buffer lean4-info-buffer-name)))



(ert-deftest lean4-info-return-goes-to-the-thing-at-point ()
  "RET goes to a message's position, and elsewhere leaves point alone.

It is how the keyboard reaches what the go-to control offers by mouse,
the file name beside it being a label."
  (let ((source (get-buffer-create "Named.lean")))
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
            ;; On a subterm, and on text that is neither, RET does
            ;; nothing: a goal is read where it stands.
            (goto-char (point-min))
            (let ((inhibit-read-only t))
              (put-text-property (point-min) (1+ (point-min))
                                 'lean4-info '(:dummy t)))
            (should-not (lean4-info-return))
            (let ((inhibit-read-only t))
              (remove-text-properties (point-min) (1+ (point-min))
                                      '(lean4-info nil)))
            (should-not (lean4-info-return))))
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
                  "All messages" '((:severity 1)) '("XY"))))
    (should (string-prefix-p "All messages (" (substring-no-properties caption)))
    (should (string-suffix-p "XY" (substring-no-properties caption)))))

(ert-deftest lean4-info-all-messages-controls-answer-to-the-mouse ()
  "The controls on the file's message heading can be clicked.

Regression test.  Every section of the display is a `lean4-info-section',
which is what carries the button bindings, but this one was left a
plain `magit-section' -- so `magit-section' put its own heading keymap
there instead and clicking the sort control did nothing."
  (let ((source (get-buffer-create "Sorted.lean")))
    (unwind-protect
        (with-current-buffer (get-buffer-create lean4-info-buffer-name)
          (let ((inhibit-read-only t))
            (unless (derived-mode-p 'magit-section-mode) (magit-section-mode))
            (erase-buffer)
            (magit-insert-section (lean4-info-section 'root)
              (lean4-info--mk-message-section
               'all-messages
               (lean4-info--messages-caption
                "All messages" nil (lean4-info--all-messages-controls))
               '((:range (:start (:line 0 :character 0)) :message "boom"))
               source)))
          (goto-char (point-min))
          (let ((sort (string-search (lean4-info-sort-glyph) (buffer-string))))
            (should sort)
            ;; `string-search' counts from zero and buffer positions from one.
            (let ((position (1+ sort)))
              (should (eq (get-text-property position 'lean4-info-command)
                          'lean4-info-toggle-message-order))
              (should (eq (keymap-lookup (get-text-property position 'keymap)
                                         "<mouse-2>")
                          'push-button)))))
      (kill-buffer source)
      (kill-buffer lean4-info-buffer-name))))

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

(ert-deftest lean4-info-a-position-just-pinned-is-shown-once ()
  "Pinning at point does not put the same position on display twice.

Regression test.  `lean4-info--following-point-p' asks about point in
the current buffer, and the sections are inserted with the info buffer
current -- where the answer is always yes.  So the position just pinned
got a pinned section and a followed section, one above the other, and
pausing from the file pointed at whichever of the two the reader had not
meant: what should have read \"pinned and paused\" in one section came
out as a pinned one and a paused one."
  (with-temp-buffer
    (rename-buffer "Twice.lean" 'unique)
    (insert "theorem t : True := by\n  trivial\n")
    (goto-char (point-min))
    (let* ((source (current-buffer))
           (pin (lean4-info--pin-create :marker (copy-marker (point))
                                        :goals "⊢ True"
                                        :id 1))
           (lean4-info--pins (list pin))
           ;; What `lean4-info-toggle-pin' leaves behind: pinned where
           ;; point still is.
           (lean4-info--pinned-at (cons source (point)))
           (following (lean4-info--following-point-p)))
      (should-not following)
      (with-temp-buffer
        (lean4-info-mode)
        (let ((inhibit-read-only t))
          (lean4-info--insert-display "Twice.lean:1:0" "⊢ True" nil nil nil nil
                                      following source))
        (goto-char (point-min))
        (should (= 1 (how-many "^Twice\\.lean:"))))
      ;; Once point has moved off it, both are shown -- one pinned, one
      ;; followed, as VS Code has it.
      (goto-char (point-max))
      (let ((following (lean4-info--following-point-p)))
        (should following)
        (with-temp-buffer
          (lean4-info-mode)
          (let ((inhibit-read-only t))
            (lean4-info--insert-display "Twice.lean:3:0" "⊢ True" nil nil nil
                                        nil following source))
          (goto-char (point-min))
          (should (= 2 (how-many "^Twice\\.lean:")))))
      (set-marker (lean4-info-pin-marker pin) nil))))

(ert-deftest lean4-info-redisplay-decides-following-in-the-lean-buffer ()
  "Whether to show the followed position is settled where point means something.

Regression test, guarding the same fault from the other side: the whole
render runs with the info buffer current, so anything that reads point
has to be worked out before that."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-temp-buffer
        (rename-buffer "Decide.lean" 'unique)
        (insert "theorem t : True := by\n  trivial\n")
        (goto-char (point-min))
        (let* ((asked nil)
               (source (current-buffer))
               (pin (lean4-info--pin-create :marker (copy-marker (point))
                                            :goals "⊢ True"
                                            :id 1))
               (lean4-info--pins (list pin))
               (lean4-info--pinned-at (cons source (point)))
               (lean4-info--rendered nil)
               (lean4-goals "⊢ True"))
          (cl-letf* ((following (symbol-function
                                 'lean4-info--following-point-p))
                     ((symbol-function 'lean4-info--following-point-p)
                      (lambda ()
                        (push (current-buffer) asked)
                        (funcall following))))
            (lean4-info-buffer-redisplay 'force))
          (should asked)
          (should (seq-every-p (lambda (buffer) (eq buffer source)) asked))
          (with-current-buffer lean4-info-buffer-name
            (goto-char (point-min))
            (should (= 1 (how-many "^Decide\\.lean:"))))
          (set-marker (lean4-info-pin-marker pin) nil)))
    (kill-buffer lean4-info-buffer-name)))


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

(defun lean4-info-test--draw-display (goal-lines)
  "Insert a display whose goal runs to GOAL-LINES lines.
The messages below it are the same every time, so that what moves is
only how much stands above them."
  (magit-insert-section (lean4-info-section 'root)
    (magit-insert-section (lean4-info-section 'goals)
      (magit-insert-heading "Tactic state")
      (dotimes (i goal-lines)
        (insert (format "goal line %d\n" i))))
    (magit-insert-section (lean4-info-section 'all-messages)
      (magit-insert-heading "All messages")
      (insert "first message\nsecond message\nthird message\n"))))

(ert-deftest lean4-info-rebuilding-keeps-the-reader-in-the-same-section ()
  "The reader stays in the section they were reading, not at an offset.

Regression test.  Where the reader was is kept across a rebuild, but it
was kept as a character position -- and the buffer is written afresh
from whatever the server last said, so the same offset is a different
place every time.  Moving through the Lean file changed how much goal
stood above the messages, and the position in the goal display wandered
off to wherever the new text happened to reach that far."
  (lean4-ensure-info-buffer lean4-info-buffer-name)
  (unwind-protect
      (with-current-buffer lean4-info-buffer-name
        (let ((inhibit-read-only t)
              (window (selected-window)))
          (set-window-buffer window (current-buffer))
          (lean4-info-test--draw-display 1)
          (goto-char (point-min))
          (search-forward "second message")
          (set-window-point window (point))
          (let ((spot (point))
                (rows (count-screen-lines (window-start window) (point)
                                          nil window)))
            (lean4-info--keeping-position
              (erase-buffer)
              (lean4-info-test--draw-display 40))
            ;; Not where it was: thirty-nine lines of goal have appeared
            ;; above, and the reader has not moved.
            (should (> (point) spot))
            (should (equal (buffer-substring-no-properties
                            (line-beginning-position) (point))
                           "second message"))
            (should (equal (magit-section-ident (magit-current-section))
                           '((lean4-info-section . all-messages)
                             (lean4-info-section . root))))
            (should (= (window-point window) (point)))
            ;; And it is still being read on the row it was read on.
            (should (= (count-screen-lines (window-start window) (point)
                                           nil window)
                       rows)))))
    (kill-buffer lean4-info-buffer-name)))

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
  (should-not (lean4-info--goals-value nil []))
  ;; Inside a proof with nothing left.
  (should (eq (lean4-info--goals-value '(:goals []) []) 'accomplished))
  ;; Something left to prove, kept as a list to be rendered on the way in.
  (should (equal (lean4-info--goals-value '(:goals ["x"]) ["x"]) '("x"))))

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
      ;; buffer, so check what it was built to invoke.
      (should (eq (get-text-property pin 'lean4-info-command heading)
                  'lean4-info-toggle-pin))
      (should (eq (get-text-property pause 'lean4-info-command heading)
                  'lean4-info-toggle-pause))
      ;; Each is a real button, so `button-at' and `forward-button' find
      ;; it once it is on display.  What the mouse is bound to belongs
      ;; to the section rather than to these characters, and is checked
      ;; where a section exists -- see
      ;; `lean4-info-sections-carry-the-display-keymap'.
      (erase-buffer)
      (insert heading)
      (dolist (offset (list pin pause))
        (let ((button (button-at (+ (point-min) offset))))
          (should button)
          (should (eq (button-type button) 'lean4-info-control))))
      ;; And they are reachable in order from the start of the line.
      (goto-char (point-min))
      (forward-button 1)
      (should (= (point) (+ (point-min) pin))))))

(ert-deftest lean4-info-pin-control-shows-which-way-it-will-go ()
  "The pin control differs pinned from unpinned, as pause and resume do.
Without this the only sign of the state was the control's face, which is
not much to read a mode off."
  (should-not (equal (lean4-info-pin-glyph) (lean4-info-unpin-glyph)))
  ;; Including when the frame can display none of the candidates.
  (cl-letf (((symbol-function 'lean4--displayable-p) #'ignore))
    (should-not (equal (lean4-info-pin-glyph) (lean4-info-unpin-glyph))))
  ;; And when it can display all of them, so the pair is the emoji pair
  ;; rather than two tiers of the fallback chain.
  (cl-letf (((symbol-function 'lean4--displayable-p)
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

(defun lean4-info-test--control-column (row glyph)
  "Return the column ROW puts GLYPH in, counted in from the window's edge.

Each control is preceded by a stretch space that names the column the
control begins in, so the space before the glyph says where the glyph
lands -- whatever anything else in the row is drawn as wide as."
  (let ((at (string-search glyph row)))
    (should at)
    (should (> at 0))
    (pcase (get-text-property (1- at) 'display row)
      (`(space :align-to (- right ,column)) column))))

(ert-deftest lean4-info-pinning-does-not-move-the-controls ()
  "Pinning changes the pin glyph in place; it does not shuffle the row.

Regression test, twice over.  The controls are set hard right, so what
stands to the right of one decides the column it lands in.  The pinned
section once listed its pause before its unpin where the followed one
lists its pause after its pin, and the two traded places outright.  Then
the row was measured by the width of its own text, which holds only
while every glyph is drawn as wide as `string-width' says: the go-to
control the pinned section adds on the left nudged the pin and pause
glyphs out from under the reader's pointer."
  (with-temp-buffer
    (rename-buffer "Order.lean" 'unique)
    ;; Force the emoji: the terminal fallbacks are several characters
    ;; wide, and the point of the exercise is the ones that are not.
    (cl-letf (((symbol-function 'lean4--displayable-p) (lambda (&rest _) t)))
      (let* ((lean4-info-paused nil)
             (pin (lean4-info--pin-create :marker (copy-marker (point-min))))
             (pinned (lean4-info--heading "x" (lean4-info--pin-controls pin)
                                          "pinned"))
             (followed (lean4-info--heading "x" (lean4-info--controls))))
        ;; Pause keeps the rightmost column in both rows, and unpin takes
        ;; the column pin was in.
        (should (equal (lean4-info-test--control-column
                        followed (lean4-info-pause-glyph))
                       (lean4-info-test--control-column
                        pinned (lean4-info-pause-glyph))))
        (should (equal (lean4-info-test--control-column
                        followed (lean4-info-pin-glyph))
                       (lean4-info-test--control-column
                        pinned (lean4-info-unpin-glyph))))
        (set-marker (lean4-info-pin-marker pin) nil)))))

(ert-deftest lean4-info-pausing-does-not-move-the-controls ()
  "The refresh control appears without pushing the others along.

It is there only while the section is paused, and the controls to its
right keep their columns because the columns are struck from the right
edge inwards.  The go-to control on its left does move along, as VS
Code's does: an absent control is not left holding a column open."
  (with-temp-buffer
    (rename-buffer "Paused.lean" 'unique)
    (cl-letf (((symbol-function 'lean4--displayable-p) (lambda (&rest _) t)))
      (let* ((pin (lean4-info--pin-create :marker (copy-marker (point-min))))
             (running (let ((lean4-info-paused nil))
                        (lean4-info--heading "x" (lean4-info--controls))))
             (paused (let ((lean4-info-paused t))
                       (lean4-info--heading "x" (lean4-info--controls)
                                            "paused")))
             (pinned (lean4-info--heading "x" (lean4-info--pin-controls pin)
                                          "pinned"))
             (held (progn (setf (lean4-info-pin-paused pin) t)
                          (lean4-info--heading "x"
                                               (lean4-info--pin-controls pin)
                                               "pinned and paused"))))
        (should (string-search (lean4-info-refresh-glyph) paused))
        (should-not (string-search (lean4-info-refresh-glyph) running))
        (dolist (rows (list (list running paused) (list pinned held)))
          (pcase-let ((`(,before ,after) rows))
            ;; Pause and resume swap glyphs, so ask each row for its own.
            (should (equal (lean4-info-test--control-column
                            before (lean4-info-pause-glyph))
                           (lean4-info-test--control-column
                            after (lean4-info-resume-glyph))))))
        (should (equal (lean4-info-test--control-column
                        running (lean4-info-pin-glyph))
                       (lean4-info-test--control-column
                        paused (lean4-info-pin-glyph))))
        (should (equal (lean4-info-test--control-column
                        pinned (lean4-info-unpin-glyph))
                       (lean4-info-test--control-column
                        held (lean4-info-unpin-glyph))))
        ;; And the go-to control takes the column the refresh control
        ;; vacates, rather than the row keeping a gap where it was.
        (should (equal (lean4-info-test--control-column
                        pinned (lean4-info-goto-glyph))
                       (lean4-info-test--control-column
                        held (lean4-info-refresh-glyph))))
        (set-marker (lean4-info-pin-marker pin) nil)))))


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

;;;; Copying what is on display

(ert-deftest lean4-info-copies-the-state-unfiltered ()
  "The whole state is copied, whatever the display is set to show.
Text pasted into an issue is no use with half the hypotheses missing."
  (with-temp-buffer
    (setq-local lean4-goals
                '((:goalPrefix "⊢ " :type (:text "True")
                   :hyps [(:names ["inst"] :type (:text "Monoid α")
                           :isInstance t)])))
    (let ((lean4-info-hide-instance-assumptions t))
      (should (equal (lean4-info--state-text)
                     "inst : Monoid α\n⊢ True")))))

(ert-deftest lean4-info-copies-leans-own-goal-prefix ()
  "A `conv' goal is not introduced by an unadorned turnstile."
  (with-temp-buffer
    (setq-local lean4-goals '((:goalPrefix "| " :hyps [] :type (:text "a + b"))))
    (should (equal (lean4-info--state-text) "| a + b"))))

(ert-deftest lean4-info-copies-plain-and-accomplished-states ()
  "Text stays text, and a finished proof says so."
  (with-temp-buffer
    (setq-local lean4-goals '("⊢ A" "⊢ B"))
    (should (equal (lean4-info--state-text) "⊢ A\n\n⊢ B")))
  (with-temp-buffer
    (setq-local lean4-goals 'accomplished)
    (should (equal (lean4-info--state-text) "goals accomplished")))
  (with-temp-buffer
    (setq-local lean4-goals nil)
    (should-not (lean4-info--state-text))))

(ert-deftest lean4-info-copy-state-fills-the-kill-ring ()
  "The command puts the state where `yank' will find it."
  (with-temp-buffer
    (setq-local lean4-goals '((:goalPrefix "⊢ " :hyps [] :type (:text "True"))))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (lean4-info-copy-state))
      (should (equal (current-kill 0) "⊢ True")))))

(ert-deftest lean4-info-copy-state-declines-when-there-is-none ()
  "Nothing to copy is a user error, not an empty kill."
  (with-temp-buffer
    (setq-local lean4-goals nil)
    (should-error (lean4-info-copy-state) :type 'user-error)))

(ert-deftest lean4-info-copies-the-state-into-a-comment ()
  "The state goes in as a block comment above the line, indented with it."
  (with-temp-buffer
    (setq-local lean4-goals
                '((:goalPrefix "⊢ " :hyps [(:names ["h"] :type (:text "P"))]
                   :type (:text "True"))))
    (insert "theorem t : True := by\n  sorry\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (cl-letf (((symbol-function 'message) #'ignore))
      (lean4-info-copy-to-comment))
    (should (equal (buffer-string)
                   (concat "theorem t : True := by\n"
                           "  /-\n"
                           "  h : P\n"
                           "  ⊢ True\n"
                           "  -/\n"
                           "  sorry\n")))))

(ert-deftest lean4-info-copy-to-comment-leaves-point-alone ()
  "Inserting above the line does not move point off it."
  (with-temp-buffer
    (setq-local lean4-goals '((:goalPrefix "⊢ " :hyps [] :type (:text "True"))))
    (insert "  sorry\n")
    (goto-char (point-min))
    (end-of-line)
    (let ((column (current-column)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (lean4-info-copy-to-comment))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     "  sorry"))
      (should (= (current-column) column)))))

(ert-deftest lean4-info-copy-message-needs-the-display ()
  "The message to copy is the one point is in, so it needs that buffer."
  (with-temp-buffer
    (should-error (lean4-info-copy-message) :type 'user-error)))

(ert-deftest lean4-info-copy-message-takes-the-one-point-is-in ()
  "Point anywhere inside a message copies that message and not its neighbour.
Messages are sections, so this walks out to the enclosing one -- point is
usually on a term deep inside the body."
  (with-temp-buffer
    (lean4-info-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (lean4-info-test-root)
        (dolist (name '("first" "second"))
          (magit-insert-section (lean4-info-section (list 'message name))
            (magit-insert-heading (format "Fixture.lean:1:0 %s\n" name))
            (magit-insert-section-body
              (insert (format "the body of the %s message\n" name)))))))
    (goto-char (point-min))
    (search-forward "the body of the second")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (lean4-info-copy-message))
      (should (string-search "second" (current-kill 0)))
      (should-not (string-search "first" (current-kill 0))))))

;;;; How much of a goal to show

(ert-deftest lean4-info-goal-settings-invert-show-goal-names ()
  "The renderer is told what to leave out, so this option arrives inverted."
  (let ((lean4-info-show-goal-names t))
    (should-not (plist-get (lean4-info--goal-settings) :hide-goal-names)))
  (let ((lean4-info-show-goal-names nil))
    (should (plist-get (lean4-info--goal-settings) :hide-goal-names))))

(ert-deftest lean4-info-goal-settings-default-to-vs-codes ()
  "Out of the box every filter is off, as in VS Code."
  (let ((settings (lean4-info--goal-settings)))
    (dolist (key '(:hide-goal-names :emphasize-first-goal :target-first
                   :hide-type-assumptions :hide-instance-assumptions
                   :hide-inaccessible-assumptions :hide-let-values))
      (should-not (plist-get settings key)))))

(ert-deftest lean4-info-goals-text-renders-a-tree ()
  "Interactive goals are held as trees and rendered on the way in."
  (should (equal (substring-no-properties
                  (lean4-info--goals-text
                   '((:goalPrefix "⊢ " :hyps [] :type (:text "True")))))
                 "⊢ True")))

(ert-deftest lean4-info-goal-count-agrees-in-number ()
  "One goal is \"1 goal\"; more are \"goals\".
Counted even at one, which is what VS Code does -- so the line is there
on every proof, not only a branching one."
  (should (equal (substring-no-properties
                  (lean4-info--goal-count '((:type (:text "A")))))
                 "1 goal"))
  (should (equal (substring-no-properties
                  (lean4-info--goal-count '((:type (:text "A"))
                                            (:type (:text "B")))))
                 "2 goals")))

(ert-deftest lean4-info-goal-count-counts-plain-goals-too ()
  "Goals from a server too old for the RPC are still counted.
They are kept as the list of strings Lean sent, which is countable even
though it cannot be filtered."
  (should (equal (substring-no-properties
                  (lean4-info--goal-count '("⊢ A" "⊢ B")))
                 "2 goals")))

(ert-deftest lean4-info-goal-count-declines-what-it-cannot-count ()
  "Pre-rendered text and a finished proof get no count.
VS Code heads a finished proof \"No goals\" rather than counting to zero;
this reports Lean\\='s own wording there instead."
  (should-not (lean4-info--goal-count "⊢ already text"))
  (should-not (lean4-info--goal-count 'accomplished)))

(ert-deftest lean4-info-tactic-state-is-headed-and-counted ()
  "The section reads \"Tactic state\" with the count on the line below."
  (with-temp-buffer
    (lean4-info-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (lean4-info-test-root)
        (lean4-info--insert-position
         '((:goalPrefix "⊢ " :hyps [] :type (:text "A"))
           (:goalPrefix "⊢ " :hyps [] :type (:text "B")))
         nil nil (current-buffer))))
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-search "Tactic state" text))
      (should-not (string-search "Goals:" text))
      ;; The count sits between the heading and the first goal.
      (should (string-match-p "Tactic state.*\n *2 goals\n *⊢ A" text)))))

(ert-deftest lean4-info-accomplished-is-not-counted ()
  "A finished proof keeps Lean's wording and gains no count line."
  (with-temp-buffer
    (lean4-info-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (lean4-info-test-root)
        (lean4-info--insert-position 'accomplished nil nil (current-buffer))))
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-search "goals accomplished" text))
      (should-not (string-search "0 goals" text)))))

(ert-deftest lean4-info-goals-text-passes-plain-text-through ()
  "Goals that arrived as text cannot be filtered, and are not touched.
A server too old for the interactive RPC sends them already rendered."
  (should (equal (lean4-info--goals-text "⊢ already text") "⊢ already text"))
  (should (equal (lean4-info--term-goal-text "⊢ so is this") "⊢ so is this")))

(ert-deftest lean4-info-goals-text-honours-the-settings ()
  "A filter set here reaches the renderer."
  (let ((goals '((:goalPrefix "⊢ " :type (:text "True")
                  :hyps [(:names ["inst"] :type (:text "Monoid α")
                          :isInstance t)]))))
    (let ((lean4-info-hide-instance-assumptions nil))
      (should (string-search "inst" (lean4-info--goals-text goals))))
    (let ((lean4-info-hide-instance-assumptions t))
      (should-not (string-search "inst" (lean4-info--goals-text goals))))))

(ert-deftest lean4-info-render-key-notices-a-changed-setting ()
  "A toggle has to force a rebuild, or nothing on screen would change."
  (let (before after)
    (let ((lean4-info-hide-let-values nil))
      (setq before (lean4-info--render-key nil nil nil nil nil nil)))
    (let ((lean4-info-hide-let-values t))
      (setq after (lean4-info--render-key nil nil nil nil nil nil)))
    (should-not (equal before after))))

(ert-deftest lean4-info-render-key-notices-expected-type-visibility ()
  "Folding or hiding the expected type is a rebuild too."
  (let (expanded collapsed hidden)
    (let ((lean4-info-expected-type-visibility 'expanded))
      (setq expanded (lean4-info--render-key nil nil nil nil nil nil)))
    (let ((lean4-info-expected-type-visibility 'collapsed))
      (setq collapsed (lean4-info--render-key nil nil nil nil nil nil)))
    (let ((lean4-info-expected-type-visibility 'hidden))
      (setq hidden (lean4-info--render-key nil nil nil nil nil nil)))
    (should-not (equal expanded collapsed))
    (should-not (equal collapsed hidden))))

(ert-deftest lean4-info-expected-type-can-be-hidden ()
  "With the expected type hidden its section is absent altogether."
  (let ((lean4-info-expected-type-visibility 'hidden))
    (with-temp-buffer
      (lean4-info-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (lean4-info-test-root)
          (lean4-info--insert-position nil "⊢ Expected" nil (current-buffer))))
      (should-not (string-search "Expected type" (buffer-string)))))
  (let ((lean4-info-expected-type-visibility 'expanded))
    (with-temp-buffer
      (lean4-info-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (lean4-info-test-root)
          (lean4-info--insert-position nil "⊢ Expected" nil (current-buffer))))
      (should (string-search "Expected type" (buffer-string)))
      ;; VS Code's wording, and no colon, as with "Tactic state".
      (should-not (string-search "Expected type:" (buffer-string))))))

(defmacro lean4-info-test--recording-setter (symbol counter &rest body)
  "Evaluate BODY with SYMBOL's custom setter counting calls in COUNTER.
The real setter still runs, so whatever it maintains is maintained; this
only notices that the custom machinery was reached at all.  Restored
afterwards, since a `custom-set' property left behind would follow the
option into every other test."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((original (get ,symbol 'custom-set)))
     (unwind-protect
         (progn
           (put ,symbol 'custom-set
                (lambda (name value)
                  (setq ,counter (1+ ,counter))
                  (if original
                      (funcall original name value)
                    (set-default name value))))
           ,@body)
       (if original
           (put ,symbol 'custom-set original)
         ;; `put' of nil is not the same as never having had the property,
         ;; but it is what `custom-set' being absent means to `setopt'.
         (put ,symbol 'custom-set nil)))))

(ert-deftest lean4-info-toggle-goes-through-the-custom-setter ()
  "A toggle sets its option the way Customize would, not with `setq'.

None of the seven has a `:set' today, so nothing observable depends on
it yet -- which is exactly why this is worth pinning.  Written with
`setq', adding a `:set' to any of them later would have it silently
ignored, and now that the seven share one definition it would be ignored
at all seven at once."
  (let ((ran 0))
    (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
              ((symbol-function 'message) #'ignore))
      (lean4-info-test--recording-setter 'lean4-info-show-goal-names ran
        (let ((lean4-info-show-goal-names t))
          (lean4-info-toggle-goal-names)
          (should (= ran 1))
          ;; And it still does what it always did.
          (should-not lean4-info-show-goal-names))))))

(ert-deftest lean4-info-cycle-expected-type-lets-its-setter-do-the-work ()
  "Cycling reaches the option's `:set' rather than repeating its body.

`lean4-info-expected-type-visibility' has a `:set' that raises the
pending flag, without which a change to the setting loses to the fold
state `magit-section' remembers.  The command used to call that by hand
after a `setq', so the rule was written both in the `defcustom' and in
the command."
  (let ((ran 0))
    (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
              ((symbol-function 'message) #'ignore))
      (lean4-info-test--recording-setter
          'lean4-info-expected-type-visibility ran
        (let ((lean4-info-expected-type-visibility 'expanded)
              (lean4-info--expected-type-pending nil))
          (lean4-info-cycle-expected-type)
          (should (= ran 1))
          (should (eq lean4-info-expected-type-visibility 'collapsed))
          ;; What the setter is for, reached without a call of its own.
          (should lean4-info--expected-type-pending))))))

(ert-deftest lean4-info-toggles-report-and-redisplay ()
  "Each toggle flips its option and asks for a redraw."
  (let ((redisplays 0))
    (cl-letf (((symbol-function 'lean4-info--redisplay-source)
               (lambda () (cl-incf redisplays)))
              ((symbol-function 'message) #'ignore))
      (let ((lean4-info-hide-let-values nil))
        (lean4-info-toggle-let-values)
        (should lean4-info-hide-let-values))
      (let ((lean4-info-show-goal-names t))
        (lean4-info-toggle-goal-names)
        (should-not lean4-info-show-goal-names))
      (let ((lean4-info-target-first nil))
        (lean4-info-toggle-target-first)
        (should lean4-info-target-first))
      (should (= redisplays 3)))))

(ert-deftest lean4-info-expected-type-visibility-applies-once ()
  "The setting wins over a remembered fold, but only when it changes.

`magit-section' ignores the HIDE argument on a rebuild in order to keep
folds where the reader put them, so a change has to come through the
visibility hook -- and has to stop coming, or every redisplay would undo
a fold made by hand."
  ;; `value' is an initarg in magit-section 3.3.0 but not in 4.x, so it is set
  ;; afterwards, which works in both.
  (let ((section (magit-section :type 'lean4-info-section))
        (other (magit-section :type 'lean4-info-section)))
    (oset section value 'term-goal)
    (oset other value 'goals)
    ;; Nothing pending: the section is left to `magit-section'.
    (let ((lean4-info--expected-type-pending nil)
          (lean4-info-expected-type-visibility 'collapsed))
      (should-not (lean4-info--expected-type-visibility section)))
    ;; A change is applied to the expected type, once.
    (let ((lean4-info--expected-type-pending t)
          (lean4-info-expected-type-visibility 'collapsed))
      (should (eq (lean4-info--expected-type-visibility section) 'hide))
      (should-not (lean4-info--expected-type-visibility section)))
    (let ((lean4-info--expected-type-pending t)
          (lean4-info-expected-type-visibility 'expanded))
      (should (eq (lean4-info--expected-type-visibility section) 'show)))
    ;; And to no other section, whose fold is none of its business.
    (let ((lean4-info--expected-type-pending t)
          (lean4-info-expected-type-visibility 'collapsed))
      (should-not (lean4-info--expected-type-visibility other)))))

(ert-deftest lean4-info-expected-type-cycle-marks-it-pending ()
  "Cycling the setting arranges for it to be applied."
  (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
            ((symbol-function 'message) #'ignore))
    (let ((lean4-info-expected-type-visibility 'expanded)
          (lean4-info--expected-type-pending nil))
      (lean4-info-cycle-expected-type)
      (should lean4-info--expected-type-pending))))

(ert-deftest lean4-info-mode-installs-the-visibility-hooks ()
  "Both are local to the display and ahead of the cached visibility.

Ahead is the whole of it: the buffer-local value ends with the `t' which
stands for the global value, `magit-section-cached-visibility' is what the
global value holds, and it answers about any section it has seen before.
A function added with APPEND lands after that `t' and never gets its say --
which is how the message list came to ignore its own setting."
  (with-temp-buffer
    (lean4-info-mode)
    (let ((global (seq-position magit-section-set-visibility-hook t)))
      (should global)
      (dolist (function '(lean4-info--expected-type-visibility))
        (should (memq function magit-section-set-visibility-hook))
        (should (< (seq-position magit-section-set-visibility-hook function)
                   global))))))

(ert-deftest lean4-info-expected-type-cycles-through-three ()
  "The cycle returns to where it started after three presses."
  (cl-letf (((symbol-function 'lean4-info--redisplay-source) #'ignore)
            ((symbol-function 'message) #'ignore))
    (let ((lean4-info-expected-type-visibility 'expanded))
      (lean4-info-cycle-expected-type)
      (should (eq lean4-info-expected-type-visibility 'collapsed))
      (lean4-info-cycle-expected-type)
      (should (eq lean4-info-expected-type-visibility 'hidden))
      (lean4-info-cycle-expected-type)
      (should (eq lean4-info-expected-type-visibility 'expanded)))))

;;;; Which messages a line shows

(defconst lean4-info-test--line-messages
  (list '(:range (:start (:line 3 :character 2) :end (:line 3 :character 6))
          :message "left of point")
        '(:range (:start (:line 3 :character 20) :end (:line 3 :character 24))
          :message "right of point")
        '(:range (:start (:line 1 :character 0) :end (:line 5 :character 0))
          :message "spanning the line from above"))
  "Three raw diagnostics touching line 3: before, after, and across it.")

(ert-deftest lean4-info-every-message-on-the-line-by-default ()
  "All three cover the line, and by default all three are listed."
  (let ((lean4-info-all-errors-on-line t))
    (should (= (length (lean4-info--diagnostics-at-line
                        lean4-info-test--line-messages 3 10))
               3))))

(ert-deftest lean4-info-only-the-messages-after-point ()
  "With the option off, only what begins at or after the column given.
The one spanning the line from further up covers it from the left rather
than standing to the right of anything on it, so it goes too."
  (let ((lean4-info-all-errors-on-line nil))
    (should (equal (mapcar (lambda (diagnostic)
                             (plist-get diagnostic :message))
                           (lean4-info--diagnostics-at-line
                            lean4-info-test--line-messages 3 10))
                   '("right of point")))))

(ert-deftest lean4-info-without-a-column-every-message-stands ()
  "Asked without a column -- as a pinned position is -- the option cannot
apply, and does not."
  (let ((lean4-info-all-errors-on-line nil))
    (should (= (length (lean4-info--diagnostics-at-line
                        lean4-info-test--line-messages 3))
               3))))

;;;; The controls on a message's heading

(ert-deftest lean4-info-a-trace-message-gets-a-search-control ()
  "Where VS Code puts its search icon: on the messages with trace output,
and on the others none."
  (let ((lean4-info--search nil))
    (should (= (length (lean4-info--message-controls
                        lean4-info-test--trace-message
                        "Foo.lean:3:0" (current-buffer) 3 0))
               2))
    (should (= (length (lean4-info--message-controls
                        "type mismatch" "Foo.lean:3:0" (current-buffer) 3 0))
               1))))

(ert-deftest lean4-info-the-search-control-runs-in-the-display ()
  "It acts on the message its heading belongs to, so it cannot be run in
the Lean buffer as the other controls are."
  (let* ((lean4-info--search nil)
         (control (car (lean4-info--message-controls
                        lean4-info-test--trace-message
                        "Foo.lean:3:0" (current-buffer) 3 0))))
    (should (get-text-property 0 'lean4-info-here control))
    (should (eq (get-text-property 0 'lean4-info-command control)
                #'lean4-info-search-trace))))

(ert-deftest lean4-info-the-search-control-shows-a-search-in-force ()
  "Engaged, as the pin and the pause are, so that the display says which
message is being searched."
  (let* ((place "Foo.lean:3:0")
         (lean4-info--search (list :place place :query "x"))
         (control (car (lean4-info--message-controls
                        lean4-info-test--trace-message
                        place (current-buffer) 3 0))))
    (should (eq (get-text-property 0 'face control)
                'lean4-info-button-active))
    (should (string-search "clear" (get-text-property 0 'help-echo control)))))

;;;; Stale imports, and the control they call for

(ert-deftest lean4-info-notices-imports-which-must-be-rebuilt ()
  "The error Lean puts in the header of a file whose imports are stale."
  (should (lean4-info--imports-stale-p
           '((:message "Imports are out of date and must be rebuilt; use \
the \"Restart File\" command in your editor.")))))

(ert-deftest lean4-info-notices-imports-which-should-be-rebuilt ()
  "And the note it sends when one goes stale under an open file."
  (should (lean4-info--imports-stale-p
           '((:message "Imports of Foo.Bar are out of date and should be \
rebuilt.")))))

(ert-deftest lean4-info-notices-nothing-in-an-ordinary-message ()
  "An ordinary error is not one of those."
  (should-not (lean4-info--imports-stale-p
               '((:message "type mismatch")
                 (:message "unsolved goals")))))

(ert-deftest lean4-info-notices-it-in-an-interactive-message ()
  "With the interactive RPC a message is a tree rather than a string, which
is the shape it arrives in by default.  The tree here is one the server
sent."
  (should (lean4-info--imports-stale-p
           '((:message (:append [(:text "Imports are out of date")
                                 (:tag [(:expr (:text " and must be rebuilt"))
                                        (:text "")])]))))))

(ert-deftest lean4-info-reads-the-words-of-any-message ()
  "Every shape a message comes in answers with its words."
  (should (equal (lean4-info--message-words "plain") "plain"))
  (should (equal (lean4-info--message-words '(:text "leaf")) "leaf"))
  (should (equal (lean4-info--message-words
                  '(:append [(:text "one ") (:text "two")]))
                 "one two"))
  (should (equal (lean4-info--message-words
                  '(:tag [(:expr (:text "Nat")) (:text " is a type")]))
                 "Nat is a type"))
  (should (equal (lean4-info--message-words nil) "")))

;;;; How the message list starts out

(ert-deftest lean4-info-all-messages-starts-folded ()
  "Folded, which is VS Code's default said forwards: its
`autoOpenShowsGoal' is on, and means `All Messages' opens collapsed."
  (let ((lean4-info--all-messages-pending t)
        (lean4-info-all-messages-visibility 'collapsed)
        (folded nil))
    (cl-letf (((symbol-function 'lean4-info--section-with-value)
               (lambda (_) 'the-section))
              ((symbol-function 'magit-section-hide)
               (lambda (_) (setq folded t)))
              ((symbol-function 'magit-section-show)
               (lambda (_) (setq folded 'shown))))
      (lean4-info--apply-all-messages-visibility))
    (should (eq folded t))
    (should-not lean4-info--all-messages-pending)))

(ert-deftest lean4-info-all-messages-can-start-open ()
  "And `expanded' says so."
  (let ((lean4-info--all-messages-pending t)
        (lean4-info-all-messages-visibility 'expanded)
        (shown nil))
    (cl-letf (((symbol-function 'lean4-info--section-with-value)
               (lambda (_) 'the-section))
              ((symbol-function 'magit-section-hide) #'ignore)
              ((symbol-function 'magit-section-show)
               (lambda (_) (setq shown t))))
      (lean4-info--apply-all-messages-visibility))
    (should shown)))

(ert-deftest lean4-info-all-messages-visibility-is-only-a-default ()
  "Applied once, so that a rebuild does not undo a fold made by hand."
  (let ((lean4-info--all-messages-pending t)
        (lean4-info-all-messages-visibility 'collapsed)
        (folds 0))
    (cl-letf (((symbol-function 'lean4-info--section-with-value)
               (lambda (_) 'the-section))
              ((symbol-function 'magit-section-hide)
               (lambda (_) (setq folds (1+ folds)))))
      (lean4-info--apply-all-messages-visibility)
      (lean4-info--apply-all-messages-visibility)
      (lean4-info--apply-all-messages-visibility))
    (should (= folds 1))))

(ert-deftest lean4-info-all-messages-visibility-waits-for-the-section ()
  "A display with no messages in it has nothing to fold, and the setting
keeps until there is."
  (let ((lean4-info--all-messages-pending t)
        (lean4-info-all-messages-visibility 'collapsed))
    (cl-letf (((symbol-function 'lean4-info--section-with-value)
               (lambda (_) nil)))
      (lean4-info--apply-all-messages-visibility))
    (should lean4-info--all-messages-pending)))

(ert-deftest lean4-info-changing-it-applies-again ()
  "Setting it through Customize arranges for one more application, so that
the change is visible without rebuilding the display by hand."
  (let ((lean4-info--all-messages-pending nil))
    (lean4-info--all-messages-changed)
    (should lean4-info--all-messages-pending)))

;;;; Searching a trace

(defconst lean4-info-test--trace-message
  '(:append [(:text "Trying to prove: ")
             (:tag [(:trace (:indent 0 :cls "Meta.synthInstance"
                             :msg (:text "result")
                             :collapsed t
                             :children (:lazy (:p "ref"))))
                    (:text "")])])
  "A message with a trace in it, shaped as the server sends one.")

(ert-deftest lean4-info-a-trace-is-what-makes-a-message-searchable ()
  "VS Code offers its search on the messages with trace output and no
others, so knowing which those are is where a search begins."
  (should (lean4-info--has-trace-p lean4-info-test--trace-message))
  (should-not (lean4-info--has-trace-p "type mismatch"))
  (should-not (lean4-info--has-trace-p
               '(:append [(:text "no trace ") (:text "in here")])))
  (should-not (lean4-info--has-trace-p nil)))

(ert-deftest lean4-info-a-searched-message-is-shown-in-its-place ()
  "What a search does to the display is show the answer where the message
was: the answer is that message with its matches marked and the traces
holding them opened."
  (let ((lean4-info--search '(:place "Foo.lean:3:0" :query "instance"
                              :message (:text "the answer"))))
    (should (equal (lean4-info--searched '(:text "the message") "Foo.lean:3:0")
                   '(:text "the answer")))
    ;; Another message, and the one it was asked about once it has moved.
    (should (equal (lean4-info--searched '(:text "the message") "Foo.lean:9:0")
                   '(:text "the message")))))

(ert-deftest lean4-info-without-a-search-the-message-stands ()
  "And with no search in force, every message is itself."
  (let ((lean4-info--search nil))
    (should (equal (lean4-info--searched '(:text "the message") "Foo.lean:3:0")
                   '(:text "the message")))))

(ert-deftest lean4-info-a-message-is-found-by-where-it-was-shown ()
  "A search asks for the message at a place, which the display records as
it draws: the display runs in its own buffer, and the diagnostics belong
to the Lean one."
  (let ((lean4-info--messages (make-hash-table :test #'equal)))
    (puthash "Foo.lean:3:0" '(:text "here it is") lean4-info--messages)
    (should (equal (lean4-info--message-at-place "Foo.lean:3:0")
                   '(:text "here it is")))
    (should-not (lean4-info--message-at-place "Foo.lean:9:0"))))

(ert-deftest lean4-info-a-place-names-a-message-the-way-a-section-does ()
  "A search is keyed by the place, so the two readings of it must agree:
one heads the section, the other finds the message again."
  (with-temp-buffer
    (rename-buffer "Fixture.lean" t)
    (should (equal (lean4-info--diagnostic-place
                    '(:range (:start (:line 6 :character 2))) (current-buffer))
                   (format "%s:7:2" (buffer-name))))))

(ert-deftest lean4-info-a-search-is-part-of-what-the-display-is-built-from ()
  "Otherwise the redisplay a search asks for would find nothing changed
and do nothing."
  (let ((without (let ((lean4-info--search nil))
                   (lean4-info--render-key nil nil "here" nil nil t)))
        (with (let ((lean4-info--search '(:place "Foo.lean:3:0"
                                          :query "instance")))
                (lean4-info--render-key nil nil "here" nil nil t))))
    (should-not (equal without with))))

(ert-deftest lean4-info-clearing-a-search-when-there-is-none ()
  "Says so rather than redrawing the display for nothing."
  (let ((lean4-info--search nil)
        (redisplays 0))
    (cl-letf (((symbol-function 'lean4-info-buffer-redisplay)
               (lambda (&rest _) (setq redisplays (1+ redisplays)))))
      (lean4-info-clear-trace-search))
    (should (= redisplays 0))))

(ert-deftest lean4-info-clearing-a-search-redraws ()
  "And clears it, so the message goes back to how Lean sent it."
  (let ((lean4-info--search '(:place "Foo.lean:3:0" :query "x"))
        (redisplays 0))
    (cl-letf (((symbol-function 'lean4-info-buffer-redisplay)
               (lambda (&rest _) (setq redisplays (1+ redisplays)))))
      (lean4-info-clear-trace-search))
    (should-not lean4-info--search)
    (should (= redisplays 1))))

;;;; Keeping the settings

(ert-deftest lean4-info-saves-every-goal-setting ()
  "`lean4-info-save-settings' offers the custom machinery all eight, and
writes the file once rather than once apiece."
  (let ((offered nil)
        (saves 0))
    (cl-letf (((symbol-function 'customize-set-variable)
               (lambda (option _value &rest _) (push option offered)))
              ((symbol-function 'custom-save-all)
               (lambda () (setq saves (1+ saves)))))
      (lean4-info-save-settings))
    (should (equal (nreverse offered) lean4-info--goal-options))
    (should (= saves 1))))

(ert-deftest lean4-info-the-saved-settings-are-the-eight ()
  "And they are the eight the toggles and the menu act on, which is what
makes saving them mean what the menu entry says."
  (should (= (length lean4-info--goal-options) 8))
  (dolist (option lean4-info--goal-options)
    (should (custom-variable-p option))))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
