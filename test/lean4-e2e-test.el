;;; lean4-e2e-test.el --- End-to-end tests against a real Lean server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; These start a real `lake serve' against test/fixture and are therefore
;; slow and dependent on a Lean toolchain.  They are tagged `:e2e' and are
;; not part of `make test'; run them with `make e2e'.
;;
;; Nothing here sleeps for a fixed duration.  Waiting is done by pumping the
;; process filter until a stated condition holds or a deadline passes, so the
;; tests are as fast as the machine allows and fail with a description of
;; what never became true rather than with a mystery assertion failure.

;;; Code:

(require 'ert)
(require 'flymake)
(require 'lean4-mode)
(require 'lean4-rpc)
(require 'lean4-render)
(require 'lean4-diagnostics)

(defconst lean4-e2e--fixture-directory
  (expand-file-name
   "fixture/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the Lean project the end-to-end tests run against.")

(defconst lean4-e2e--fixture-file
  (expand-file-name "Fixture.lean" lean4-e2e--fixture-directory))

;; Line numbers in Fixture.lean, zero-based, as the protocol counts them.
(defconst lean4-e2e--sorry-line 7
  "Line of the `sorry' in the fixture.
The goal stands *at* the token: just past it Lean reports \"no goals\",
so these tests place point at the start of the line's indentation.")
(defconst lean4-e2e--error-line 9 "Line of the type error in the fixture.")
(defconst lean4-e2e--trace-line 14
  "Line of the declaration producing a nested trace in the fixture.")
(defconst lean4-e2e--filters-line 20
  "Line of the `sorry' whose goal has a hypothesis of every kind.
A type, a typeclass instance and an ordinary one, which is what the
goal display\\='s filters tell apart.")
(defconst lean4-e2e--hint-line 25
  "Line of the declaration Lean offers an inlay hint on in the fixture.
Its type mentions an `α' never bound, so Lean binds it and says so.")
(defconst lean4-e2e--suggestion-line 30
  "Line of the `simp?' in the fixture, whose \"Try this\" is a code action.")

(defconst lean4-e2e--timeout 180
  "Seconds to allow for the server to start and elaborate the fixture.

Generous because a cold start compiles the toolchain's index, and because
elan downloads a whole toolchain the first time one is asked for -- which
is what a file outside any project asks for, its version being elan's
default rather than a pinned one.

Bound to `eglot-connect-timeout' as well as used by
`lean4-e2e--wait-until'.  Eglot's own default is 30 seconds and applies to
the connection rather than to anything after it, so a cold toolchain
failed the connection long before this allowance was ever consulted.")

(defun lean4-e2e--wait-until (description predicate)
  "Pump process output until PREDICATE returns non-nil.
Signals an error naming DESCRIPTION if `lean4-e2e--timeout' passes
first.  Returns the value PREDICATE finally returned."
  (let ((deadline (+ (float-time) lean4-e2e--timeout))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      ;; A short timeout keeps this responsive without busy-waiting; the
      ;; work happens in the process filter this call runs.
      (accept-process-output nil 0.1))
    (unless value
      (error "Timed out after %ss waiting for %s"
             lean4-e2e--timeout description))
    value))

(defmacro lean4-e2e--with-fixture (&rest body)
  "Open the fixture with a running Lean server and evaluate BODY.
Point starts at `point-min'.  The server is shut down afterwards."
  (declare (indent 0) (debug (body)))
  `(let* ((lean4-info-auto-open nil)
          ;; Pins outlive a buffer -- that is what they are for -- so a
          ;; test that leaves one behind hands the next test a marker into
          ;; a killed buffer.
          (lean4-info--pins nil)
          (lean4-info--pinned-at nil)
          (buffer (find-file-noselect lean4-e2e--fixture-file)))
     (unwind-protect
         (with-current-buffer buffer
           (should (derived-mode-p 'lean4-mode))
           ;; Not `eglot-ensure': it defers the connection to
           ;; `post-command-hook', which never runs under --batch.
           (let ((eglot-sync-connect t)
                 (eglot-connect-timeout lean4-e2e--timeout))
             (apply #'eglot--connect (eglot--guess-contact)))
           (should (eglot-current-server))
           ;; Diagnostics arrive incrementally while Lean elaborates, and
           ;; the type error is on the last line, so waiting for it is a
           ;; reliable signal that the whole file has been processed.
           (lean4-e2e--wait-until
            "the fixture to be fully elaborated"
            (lambda ()
              (flymake-start)
              (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
           (goto-char (point-min))
           ,@body)
       ;; Let Eglot take its own management off the buffer rather than
       ;; preserving it and then killing the buffer ourselves.  The
       ;; shutdown has to happen *in* the managed buffer: it is a request
       ;; like any other, so Eglot first flushes the buffer's pending
       ;; edits, and on Emacs 30 that means `track-changes-fetch' on the
       ;; buffer-local tracker.  Called anywhere else the tracker is nil
       ;; and track-changes asserts that it is one of its own.
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when-let* ((server (eglot-current-server)))
             (eglot-shutdown server nil nil))
           (set-buffer-modified-p nil))
         (kill-buffer buffer)))))

(defun lean4-e2e--error-p (diagnostic)
  "Return non-nil if Flymake DIAGNOSTIC is an error.
Eglot labels its diagnostics `eglot-error' and friends rather than with
Flymake's own `:error', so compare severities instead of symbols."
  (= (flymake--severity (flymake-diagnostic-type diagnostic))
     (flymake--severity :error)))

(defun lean4-e2e--goto-line (line)
  "Move point to zero-based LINE."
  (goto-char (point-min))
  (forward-line line))

(ert-deftest lean4-e2e-server-starts ()
  "The server starts and adopts the buffer."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (should (eglot-managed-p))
    (should (cl-typep (eglot-current-server) 'lean4-eglot-lsp-server))))

(ert-deftest lean4-e2e-diagnostics-reach-flymake ()
  "Lean's errors and warnings arrive as Flymake diagnostics.
The fixture has exactly one of each: a `sorry' warning and a type
mismatch."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let* ((diagnostics (flymake-diagnostics))
           (errors (seq-filter #'lean4-e2e--error-p diagnostics)))
      (should (= (length errors) 1))
      (should (string-match-p
               "[Tt]ype mismatch"
               (lean4-diagnostic-message (car errors))))
      (should (seq-find
               (lambda (d) (string-search "sorry" (lean4-diagnostic-message d)))
               diagnostics)))))

(ert-deftest lean4-e2e-silent-diagnostics-are-not-problems ()
  "Lean's silent diagnostics never reach Flymake.

Asking for `silentDiagnosticSupport' makes Lean send them; not showing
them is then the client's job.  \"Goals accomplished!\" is one, and
without the filter it appears against every completed proof as a note --
a report that nothing is wrong, filed as something wrong.

The second half matters as much: filtering must not take the ordinary
diagnostics with it."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let ((messages (mapcar #'lean4-diagnostic-message (flymake-diagnostics))))
      (should-not (seq-find (lambda (m) (string-search "Goals accomplished" m))
                            messages))
      ;; Note-severity output, such as traces, is kept out too.
      (should-not (seq-find (lambda (m) (string-search "trace" m)) messages))
      ;; And the real ones are all still here.
      (should (seq-find (lambda (m) (string-search "sorry" m)) messages))
      (should (seq-find (lambda (m) (string-search "Type mismatch" m))
                        messages)))))

(ert-deftest lean4-e2e-diagnostics-carry-lean-full-range ()
  "The raw LSP diagnostic is reachable, and Lean's `fullRange' with it.
This is what lets the info buffer show a message while point is anywhere
inside the declaration it is about, not just on the underlined line."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let ((diagnostic (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
      (should (lean4-diagnostic-lsp-data diagnostic))
      (should (lean4-diagnostic-full-range diagnostic))
      (should (= (lean4-diagnostic-full-start-line diagnostic)
                 lean4-e2e--error-line)))))

(ert-deftest lean4-e2e-plain-goal-at-sorry ()
  "`$/lean/plainGoal' returns the goal standing at the `sorry'."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let (result done)
      (jsonrpc-async-request
       (eglot-current-server) :$/lean/plainGoal
       (eglot--TextDocumentPositionParams)
       :success-fn (lambda (r) (setq result r done t))
       :error-fn (lambda (e) (setq result e done t)))
      (lean4-e2e--wait-until "the goal request to return" (lambda () done))
      (let ((goals (plist-get result :goals)))
        (should (> (length goals) 0))
        ;; `string-search', not `string-match-p': "+" is a regexp
        ;; metacharacter, so goal text is not its own regexp.
        (should (string-search "⊢" (elt goals 0)))
        (should (string-search "2 + 2 = 4" (elt goals 0)))))))

(ert-deftest lean4-e2e-file-progress-clears ()
  "File-progress overlays are gone once elaboration finishes.
A leftover overlay means the fringe never learned the file was done,
which is what happens when the URI-to-buffer lookup silently matches
nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--wait-until
     "the progress overlays to clear"
     (lambda ()
       (lean4-fringe-update-progress-overlays)
       (not (seq-find (lambda (ov) (eq (overlay-get ov 'face) 'lean4-fringe-face))
                      (overlays-in (point-min) (point-max))))))
    (should (seq-empty-p lean4-fringe-data))))

(ert-deftest lean4-e2e-info-buffer-shows-the-goal ()
  "The info buffer renders the goal at point.

`lean4-info-buffer-active' refreshes only when the info buffer is on
display *and* the Lean buffer is the one in the selected window, so both
windows have to exist for real.  Under --batch there is a single window,
and plain `display-buffer' would put the info buffer into it, leaving no
window showing the source."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (delete-other-windows)
    (set-window-buffer (selected-window) (current-buffer))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (set-window-buffer (split-window) lean4-info-buffer-name)
    (unwind-protect
        (progn
          (should (get-buffer-window lean4-info-buffer-name t))
          (should (lean4-info-buffer-active lean4-info-buffer-name))
          (lean4-e2e--goto-line lean4-e2e--sorry-line)
          (back-to-indentation)
          (lean4-info-buffer-refresh)
          (lean4-e2e--wait-until
           "the goal to reach the info buffer"
           (lambda ()
             (with-current-buffer lean4-info-buffer-name
               (string-search "2 + 2 = 4" (buffer-string)))))
          (with-current-buffer lean4-info-buffer-name
            ;; VS Code's own heading, with the number of goals below it.
            ;; Counted even at one, which is what VS Code does.
            (should (string-search "Tactic state" (buffer-string)))
            (should (string-search "1 goal" (buffer-string)))
            (should-not (string-search "1 goals" (buffer-string)))
            ;; The server's own wording, not Eglot's "Lean 4: " prefixed
            ;; version, which is useful in the echo area but noise here.
            (should-not (string-search "Lean 4: " (buffer-string)))))
      (delete-other-windows))))

(ert-deftest lean4-e2e-goal-filters-apply-to-goals-already-fetched ()
  "A display setting takes effect on goals that have already arrived.

The trees are kept as Lean sent them and rendered on the way into the
buffer, so changing how much of a goal to show is a redisplay -- which
fetches nothing -- rather than another round trip.  That is what lets a
setting take effect on a pinned or paused section too.

Uses a real goal because the point is the whole chain: Lean's own
`isType' and `isInstance' flags, read off the wire, reaching the filter."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (delete-other-windows)
    (set-window-buffer (selected-window) (current-buffer))
    (lean4-ensure-info-buffer lean4-info-buffer-name)
    (set-window-buffer (split-window) lean4-info-buffer-name)
    (unwind-protect
        (let ((lean4-info-hide-type-assumptions nil)
              (lean4-info-hide-instance-assumptions nil))
          (lean4-e2e--goto-line lean4-e2e--filters-line)
          (back-to-indentation)
          (lean4-info-buffer-refresh)
          (lean4-e2e--wait-until
           "the goal's hypotheses to reach the info buffer"
           (lambda ()
             (with-current-buffer lean4-info-buffer-name
               (string-search "α : Type" (buffer-string)))))
          (with-current-buffer lean4-info-buffer-name
            (should (string-search "Inhabited α" (buffer-string)))
            (should (string-search "h : α" (buffer-string))))
          ;; No refresh from here on, only a redisplay.
          (setq lean4-info-hide-type-assumptions t
                lean4-info-hide-instance-assumptions t)
          (lean4-info--redisplay-source)
          (with-current-buffer lean4-info-buffer-name
            (should-not (string-search "α : Type" (buffer-string)))
            (should-not (string-search "Inhabited α" (buffer-string)))
            ;; The ordinary hypothesis and the target are left alone.
            (should (string-search "h : α" (buffer-string)))
            (should (string-search "True" (buffer-string)))))
      (delete-other-windows))))

;;;; Module hierarchy

(ert-deftest lean4-e2e-module-hierarchy-is-offered-and-answers ()
  "The server offers a module hierarchy and says which module a file is.

Only the plumbing is checked here.  The tree itself comes from the
watchdog's reference data, which it builds from the `.ilean' files a
build leaves behind -- and this fixture deliberately does not compile, so
there are none to read.  `lean4-module-test' draws the tree from payloads
instead."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (should (lean4-module-supported-p))
    (let ((module (jsonrpc-request
                   (eglot-current-server)
                   :$/lean/prepareModuleHierarchy
                   (list :textDocument (lean4-text-document-identifier)))))
      (should (equal (lean4-module--name module) "Fixture"))
      (should (string-suffix-p "Fixture.lean" (lean4-module--uri module))))))

;;;; Interactive RPC

(defun lean4-e2e--rpc (call)
  "Run CALL, a function of (SUCCESS FAILURE), and return its result.
Signals if the call fails or never answers."
  (let (result failure done)
    (funcall call
             (lambda (value) (setq result value done t))
             (lambda (error) (setq failure error done t)))
    (lean4-e2e--wait-until "the RPC call to return" (lambda () done))
    (when failure
      (error "RPC call failed: %S" failure))
    result))

(ert-deftest lean4-e2e-rpc-negotiates-wire-format-v1 ()
  "The server agrees to the v1 reference encoding we ask for.
Version 0 names the field \"p\", which can collide with user data."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (should (eq (lean4-rpc-handle-ref-key (lean4-rpc-open)) :__rpcref))))

(ert-deftest lean4-e2e-rpc-interactive-goals-render ()
  "Interactive goals come back and render to the expected proposition."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (result (lean4-e2e--rpc
                    (lambda (success failure)
                      (lean4-rpc-get-interactive-goals handle success failure))))
           (rendered (lean4-render-goals (plist-get result :goals)))
           ;; The goal prefix and any hypothesis names are Lean4-Mode's own
           ;; text, not part of the term, so only the type is expected to
           ;; be covered by subterms.
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get result :goals) 0) :type))))
      (should rendered)
      (should (string-search "2 + 2 = 4" (substring-no-properties rendered)))
      (should (equal (substring-no-properties type) "2 + 2 = 4"))
      ;; Every character of the term must carry the subterm covering it,
      ;; or hovering and jumping cannot work.
      (dotimes (index (length type))
        (should (get-text-property index 'lean4-subexpr-pos type))
        (should (get-text-property index 'lean4-info type))))))

(ert-deftest lean4-e2e-rpc-subterm-hover ()
  "A subterm reference expands into its type.
This is what the goal buffer shows when point is on a subterm."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (goals (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-get-interactive-goals handle success failure))))
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get goals :goals) 0) :type)))
           ;; The first character of "2 + 2 = 4" is a numeral.
           (info (get-text-property 0 'lean4-info type))
           (popup (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-info-to-interactive handle info
                                                    success failure)))))
      (should info)
      (should popup)
      ;; The popup reports the numeral's type.
      (should (string-search
               "Nat"
               (substring-no-properties
                (lean4-render-tagged-text (plist-get popup :type))))))))

(ert-deftest lean4-e2e-rpc-go-to-location ()
  "A subterm can say where it is defined.
This is what backs jumping from the goal buffer to a definition."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (goals (lean4-e2e--rpc
                   (lambda (success failure)
                     (lean4-rpc-get-interactive-goals handle success failure))))
           (type (lean4-render-tagged-text
                  (plist-get (elt (plist-get goals :goals) 0) :type)))
           ;; Index 2 is the "+", whose head symbol is a real definition.
           (info (get-text-property 2 'lean4-info type))
           (locations (lean4-e2e--rpc
                       (lambda (success failure)
                         (lean4-rpc-get-go-to-location handle "definition" info
                                                       success failure)))))
      (should (> (length locations) 0))
      (should (plist-get (elt locations 0) :targetUri)))))

(ert-deftest lean4-e2e-rpc-survives-a-restart ()
  "A call after the file worker restarts reconnects instead of failing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let ((handle (lean4-rpc-open)))
      ;; Prime the session.
      (lean4-e2e--rpc (lambda (success failure)
                        (lean4-rpc-get-interactive-goals handle success failure)))
      ;; Closing and reopening the document restarts the file worker, which
      ;; invalidates the session exactly as editing an import would.
      (lean4-refresh-file-dependencies)
      (lean4-e2e--wait-until
       "the file to be elaborated again"
       (lambda ()
         (flymake-start)
         (seq-find #'lean4-e2e--error-p (flymake-diagnostics))))
      (let ((result (lean4-e2e--rpc
                     (lambda (success failure)
                       (lean4-rpc-get-interactive-goals handle
                                                        success failure)))))
        (should (lean4-render-goals (plist-get result :goals)))))))

;;;; The interactive info buffer

(defmacro lean4-e2e--with-info-window (&rest body)
  "Show the info buffer beside the Lean buffer and evaluate BODY.
Both windows must exist for `lean4-info-buffer-active' to be satisfied;
see `lean4-e2e-info-buffer-shows-the-goal'."
  (declare (indent 0) (debug (body)))
  `(progn
     (delete-other-windows)
     (set-window-buffer (selected-window) (current-buffer))
     (lean4-ensure-info-buffer lean4-info-buffer-name)
     ;; The buffer outlives each test, and every test connects a fresh
     ;; server.  Left as it was, its contents would satisfy the next test's
     ;; wait immediately, and its handle would point at a dead connection.
     (with-current-buffer lean4-info-buffer-name
       (let ((inhibit-read-only t)) (erase-buffer))
       ;; `magit-section' remembers what is folded, by ident, both in the
       ;; old tree and in a cache of its own that outlives the buffer --
       ;; which is what makes a fold survive a rebuild, and what would
       ;; hand the next test a section another test had opened.
       (setq magit-section-visibility-cache nil
             magit-root-section nil
             lean4-info--handle nil
             lean4-info--source-buffer nil))
     (set-window-buffer (split-window) lean4-info-buffer-name)
     (unwind-protect (progn ,@body)
       (delete-other-windows))))

(defun lean4-e2e--show-goal-at (line)
  "Put point on zero-based LINE and wait for the info buffer to catch up."
  (lean4-e2e--goto-line line)
  (back-to-indentation)
  (lean4-info-buffer-refresh)
  (lean4-e2e--wait-until
   "the goal to reach the info buffer"
   (lambda ()
     (with-current-buffer lean4-info-buffer-name
       (string-search "⊢" (buffer-string))))))

(ert-deftest lean4-e2e-info-buffer-separates-the-messages-at-point ()
  "Messages covering point are listed apart from the rest of the file.

Which messages are \"here\" comes from Lean\'s `fullRange\', not from the
range it underlines: line 6 is the name of the declaration the `sorry\'
is in, and that is what the message is anchored to."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--goto-line 6)
      (lean4-info-buffer-refresh)
      (should (lean4-e2e--wait-until
               "the message at point to be listed on its own"
               (lambda ()
                 (with-current-buffer lean4-info-buffer-name
                   (string-search "Messages (" (buffer-string))))))
      (with-current-buffer lean4-info-buffer-name
        ;; And it is still counted among the file's messages.
        (should (string-search "All messages (" (buffer-string)))

        (should (string-search "declaration uses" (buffer-string)))
        ;; Each message says which file it is in, and offers a control
        ;; that goes there.
        (should (string-search "Fixture.lean:7:8" (buffer-string)))
        (should-not (string-search "Fixture.lean:7:8:" (buffer-string)))
        (should (string-search (lean4-info-goto-glyph) (buffer-string)))
        ;; What the position has to say is set in from its heading; the
        ;; file's own messages are not.
        ;; What the position has to say is set in from its heading; the
        ;; file's own messages are not.  Indented with real spaces, as a
        ;; `magit-section' buffer is.
        (goto-char (point-min))
        (should (re-search-forward "^  Messages (" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^All messages (" nil t))))))

(ert-deftest lean4-e2e-silent-messages-belong-to-the-position ()
  "The completed-proof report shows at the proof, not in the file's list.

`isSilent' marks a message as being for the goal display rather than the
editor.  VS Code shows such a message against the position it belongs to
and leaves it out of All Messages, and so does this."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      ;; Line 3 is the theorem that is actually proved.
      (lean4-e2e--goto-line 3)
      (lean4-info-buffer-refresh)
      (should (lean4-e2e--wait-until
               "the completed-proof report"
               (lambda ()
                 (with-current-buffer lean4-info-buffer-name
                   (string-search "Goals accomplished" (buffer-string))))))
      (with-current-buffer lean4-info-buffer-name
        (let* ((text (buffer-string))
               (report (string-search "Goals accomplished" text))
               (all (string-search "All messages" text)))
          (should report)
          (should all)
          ;; Above the file's list, so it is the position's copy, and the
          ;; only copy.
          (should (< report all))
          (should-not (string-search "Goals accomplished"
                                     (substring text all))))))))

(ert-deftest lean4-e2e-info-buffer-says-when-there-is-nothing ()
  "Outside a proof the display says so rather than going blank.

Line 0 is a comment: no goal, no expected type, no message of its own.
The fixture does have errors further down, so this also pins that the
notice is about the position being reported on rather than about the
buffer being empty -- it appears alongside the messages from below."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      ;; Warm the display on a real goal first, so that finding the notice
      ;; afterwards means it replaced something rather than never having
      ;; had anything to show.
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (lean4-e2e--goto-line 0)
      (lean4-info-buffer-refresh)
      (should (lean4-e2e--wait-until
               "the display to report having nothing"
               (lambda ()
                 (with-current-buffer lean4-info-buffer-name
                   (string-search "No info found." (buffer-string))))))
      (with-current-buffer lean4-info-buffer-name
        (should (string-search "All messages (" (buffer-string)))))))

(ert-deftest lean4-e2e-info-buffer-goals-are-interactive ()
  "Goals in the info buffer carry per-subterm information.
Without this, nothing in the buffer can be hovered or jumped from."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'lean4-info))
        (should (get-text-property (point) 'lean4-subexpr-pos))))))

(ert-deftest lean4-e2e-info-buffer-subterm-bounds ()
  "The subterm under point spans the whole of the term it belongs to.
Point on the `+' should select `2 + 2', not just the operator."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        ;; Move onto the "+".
        (goto-char (+ (match-beginning 0) 2))
        (let ((bounds (lean4-info-subterm-bounds)))
          (should bounds)
          (should (equal (buffer-substring-no-properties
                          (car bounds) (cdr bounds))
                         "2 + 2")))))))

(ert-deftest lean4-e2e-info-buffer-eldoc-reports-a-type ()
  "ElDoc reports the type of the subterm under point."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        (goto-char (match-beginning 0))
        (let (reported)
          (should (lean4-info-eldoc-function
                   (lambda (text &rest _) (setq reported text))))
          (lean4-e2e--wait-until "ElDoc to report a type"
                                 (lambda () reported))
          (should (string-search "Nat" reported)))))))

(ert-deftest lean4-e2e-info-buffer-xref-finds-a-definition ()
  "xref resolves the subterm under point to a source location.
This is what makes \\[xref-find-definitions] work in the goal buffer."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "2 + 2 = 4" nil t))
        ;; The "+" resolves to the definition of addition.
        (goto-char (+ (match-beginning 0) 2))
        (should (eq (lean4-info-xref-backend) 'lean4-info))
        (let ((items (xref-backend-definitions 'lean4-info nil)))
          (should items)
          (should (xref-item-location (car items))))))))

(ert-deftest lean4-e2e-info-buffer-reports-accomplished-goals ()
  "A finished proof says so rather than showing nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      ;; Just past the `rfl' that closes the first theorem.
      (lean4-e2e--goto-line 4)
      (end-of-line)
      (lean4-info-buffer-refresh)
      (lean4-e2e--wait-until
       "the info buffer to report the proof is done"
       (lambda ()
         (with-current-buffer lean4-info-buffer-name
           (string-search "goals accomplished" (buffer-string))))))))

;;;; Opening the goal display

(ert-deftest lean4-e2e-goal-display-opens-by-itself ()
  "Visiting a Lean file shows the goal display without being asked.

VS Code opens its InfoView on `lean4.infoview.autoOpen', which defaults
to true, and the proof state is the point of the exercise.  The window
must not be selected, though: opening a file should leave point in the
file."
  :tags '(:e2e)
  (let (buffer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (when-let* ((window (get-buffer-window lean4-info-buffer-name t)))
            (quit-window nil window))
          (let ((lean4-info-auto-open t))
            (setq buffer (find-file-noselect lean4-e2e--fixture-file))
            (set-window-buffer (selected-window) buffer)
            ;; The open is deferred to an idle moment, so let timers run.
            (lean4-e2e--wait-until
             "the goal display to appear"
             (lambda ()
               (accept-process-output nil 0.05)
               (get-buffer-window lean4-info-buffer-name t))))
          (should (get-buffer-window lean4-info-buffer-name t))
          ;; Shown, not selected.
          (should (eq (window-buffer (selected-window)) buffer)))
      (when buffer (kill-buffer buffer))
      (delete-other-windows))))

(ert-deftest lean4-e2e-goal-display-can-be-left-closed ()
  "With `lean4-info-auto-open' nil, visiting a file opens nothing."
  :tags '(:e2e)
  (let (buffer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (when-let* ((window (get-buffer-window lean4-info-buffer-name t)))
            (quit-window nil window))
          (let ((lean4-info-auto-open nil))
            (setq buffer (find-file-noselect lean4-e2e--fixture-file))
            (set-window-buffer (selected-window) buffer)
            (dotimes (_ 20) (accept-process-output nil 0.05)))
          (should-not (get-buffer-window lean4-info-buffer-name t)))
      (when buffer (kill-buffer buffer))
      (delete-other-windows))))

;;;; Files outside a project

(ert-deftest lean4-e2e-loose-file-gets-a-server ()
  "A Lean file outside any Lake package is still served.

Regression test.  Starting the server only when a workspace root was
found meant a scratch .lean file got none at all, even though
`lean4--server-command' already falls back to `lean --server' with
elan's default toolchain for exactly that case.  VS Code serves such
files, and so should this."
  :tags '(:e2e)
  (let* ((directory (make-temp-file "lean4-loose" 'directory))
         (file (expand-file-name "Loose.lean" directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "def loose : Nat := 1\n" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (derived-mode-p 'lean4-mode))
            ;; No lean-toolchain anywhere above it.
            (should-not (lean4--workspace-root))
            (should (equal (lean4--server-command nil)
                           (list lean4-executable-name "--server")))
            ;; A loose file is served by whatever elan's default toolchain
            ;; resolves to, which goes stale on every Lean release -- and
            ;; the first use of a version that is not installed downloads
            ;; it.  Eglot's 30-second default is not enough for that.
            (let ((eglot-sync-connect t)
                  (eglot-connect-timeout lean4-e2e--timeout))
              (apply #'eglot--connect (eglot--guess-contact)))
            (should (eglot-managed-p))
            ;; And it answers Lean's own requests, not just standard LSP.
            (goto-char (point-min))
            (end-of-line)
            (let (answered)
              (jsonrpc-async-request
               (eglot-current-server) :$/lean/plainTermGoal
               (eglot--TextDocumentPositionParams)
               :success-fn (lambda (_) (setq answered t))
               :error-fn (lambda (_) (setq answered t)))
              (lean4-e2e--wait-until "the server to answer"
                                     (lambda () answered)))))
      (when (buffer-live-p buffer)
        ;; In the buffer, as in `lean4-e2e--with-fixture': shutting a
        ;; server down flushes the buffer's changes first, and the
        ;; tracker that does so on Emacs 30 is buffer-local.
        (with-current-buffer buffer
          (when-let* ((server (eglot-current-server)))
            (eglot-shutdown server nil nil 'preserve-buffers)))
        (kill-buffer buffer))
      (delete-directory directory 'recursive))))

;;;; Interactive diagnostics

(ert-deftest lean4-e2e-interactive-diagnostics-carry-lean-fields ()
  "`isSilent' and `leanTags' arrive on the interactive diagnostics.

They are on no pushed diagnostic: Lean does not send silent ones over
`textDocument/publishDiagnostics' at all.  Reading them from the wrong
source is why the goals-accomplished marker first did nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (diagnostics
            (lean4-e2e--rpc
             (lambda (success failure)
               (lean4-rpc-get-interactive-diagnostics handle success failure))))
           (accomplished
            (seq-find #'lean4-diagnostics-goals-accomplished-p
                      (append diagnostics nil))))
      ;; The fixture's first theorem is proved outright.
      (should accomplished)
      (should (lean4-diagnostics-silent-p accomplished)))))

(ert-deftest lean4-e2e-has-widgets-yields-structured-messages ()
  "With `hasWidgets', messages arrive as trees rather than flat text.

Without it Lean pre-renders everything: no `MsgEmbed' tags at all, so a
trace cannot be folded and a term inside an error message cannot be
hovered.  This asserts both embeds the fixture produces."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--trace-line)
    (back-to-indentation)
    (let* ((handle (lean4-rpc-open))
           (diagnostics
            (lean4-e2e--rpc
             (lambda (success failure)
               (lean4-rpc-get-interactive-diagnostics handle success failure))))
           embeds)
      (letrec ((walk
                (lambda (node)
                  (cond
                   ((null node))
                   ((plist-member node :append)
                    (seq-doseq (child (plist-get node :append))
                      (funcall walk child)))
                   ((plist-member node :tag)
                    (let ((embed (elt (plist-get node :tag) 0)))
                      (dolist (key '(:expr :trace :goal))
                        (when (plist-member embed key) (push key embeds))))
                    (funcall walk (elt (plist-get node :tag) 1)))))))
        (seq-doseq (diagnostic diagnostics)
          (funcall walk (plist-get diagnostic :message))))
      ;; A term inside the type-mismatch message, and the synthInstance
      ;; trace at the end of the fixture.
      (should (memq :expr embeds))
      (should (memq :trace embeds)))))

;;;; Collapsible traces

(ert-deftest lean4-e2e-trace-folds-and-unfolds ()
  "A trace in the info buffer starts folded and fetches its children when opened.

The children are not sent with the message -- a `simp' trace on a real
proof can be enormous -- so opening one has to fetch them.  A trace is a
`magit-section' like everything else in the display, so opening it is
opening a section; what this checks is that doing so asks Lean.  Against
a real `Meta.synthInstance' trace rather than a synthetic one."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (clrhash lean4-info--trace-children)
            (clrhash lean4-info--trace-lazy)
            (lean4-e2e--goto-line lean4-e2e--trace-line)
            (back-to-indentation)
            (lean4-info-buffer-refresh)
            (lean4-e2e--wait-until
             "the trace to reach the info buffer"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (string-search "Meta.synthInstance" (buffer-string)))))
            (with-current-buffer lean4-info-buffer-name
              (let ((trace (lean4-e2e--trace-section)))
                (should trace)
                ;; Folded to start with: the server marks it collapsed,
                ;; and its children would cost a request.
                (should (oref trace hidden))
                ;; Opening it is opening a section, however that is done.
                (magit-section-show trace)
                (lean4-info--fetch-open-traces)))
            ;; The children arrive asynchronously and the buffer is rebuilt.
            (lean4-e2e--wait-until
             "the trace children to be fetched and shown"
             (lambda ()
               (> (hash-table-count lean4-info--trace-children) 0)))
            (with-current-buffer lean4-info-buffer-name
              (let ((trace (lean4-e2e--trace-section)))
                (should trace)
                (should-not (oref trace hidden))
                ;; And what came back is under it.
                (should (> (- (oref trace end) (oref trace content)) 1)))))
        (clrhash lean4-info--trace-children)
        (clrhash lean4-info--trace-lazy)))))

(ert-deftest lean4-e2e-nested-trace-children-are-indented-under-it ()
  "A trace's children sit to the right of the node they hang under.

Regression test.  `magit-section' puts the body of a section that starts
folded aside and runs it when the reader opens it, and how far in we
were is a dynamic binding that has unwound by then -- so children opened
that way came out at the outermost level, to the left of their own
parent."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (lean4-e2e--goto-line lean4-e2e--trace-line)
            (back-to-indentation)
            (lean4-info-buffer-refresh)
            (lean4-e2e--wait-until
             "the trace to reach the info buffer"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (string-search "Meta.synthInstance" (buffer-string)))))
            (with-current-buffer lean4-info-buffer-name
              (magit-section-show (lean4-e2e--trace-section))
              (lean4-info--fetch-open-traces))
            (lean4-e2e--wait-until
             "its children to arrive"
             (lambda () (> (hash-table-count lean4-info--trace-children) 0)))
            (with-current-buffer lean4-info-buffer-name
              (let* ((trace (lean4-e2e--trace-section))
                     (parent (lean4-e2e--indentation-at (oref trace start)))
                     (child (lean4-e2e--indentation-at
                             (save-excursion
                               (goto-char (oref trace content))
                               (point)))))
                (should (> child parent)))))
        (clrhash lean4-info--trace-children)
        (clrhash lean4-info--trace-lazy)))))

(defun lean4-e2e--indentation-at (position)
  "Return how far in the line at POSITION is set."
  (save-excursion
    (goto-char position)
    (back-to-indentation)
    (current-column)))

(defun lean4-e2e--trace-section ()
  "Return the first trace section in the info buffer, or nil."
  (catch 'found
    (letrec ((walk (lambda (section)
                     (let ((value (oref section value)))
                       (when (and (consp value) (eq (car value) 'trace))
                         (throw 'found section)))
                     (mapc walk (oref section children)))))
      (funcall walk magit-root-section))
    nil))

(ert-deftest lean4-e2e-error-message-terms-are-interactive ()
  "A term inside an error message carries subterm information.

`hasWidgets' makes Lean tag terms embedded in messages, so the `String'
and `Nat' of a type mismatch are as hoverable as anything in a goal."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--goto-line lean4-e2e--error-line)
      (back-to-indentation)
      (lean4-info-buffer-refresh)
      (lean4-e2e--wait-until
       "the type mismatch to reach the info buffer"
       (lambda ()
         (with-current-buffer lean4-info-buffer-name
           (string-search "Type mismatch" (buffer-string)))))
      (with-current-buffer lean4-info-buffer-name
        (goto-char (point-min))
        (should (search-forward "String" nil t))
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'lean4-info))))))

;;;; Following point

(ert-deftest lean4-e2e-goal-display-follows-point ()
  "Moving point updates the goal display, with no other prompting.

Regression test for the bug where the display only ever showed the goal
it was opened on: point movement re-rendered the goals already stored
but never fetched the ones for the new position, so the buffer appeared
stuck until it was closed and reopened."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      ;; Start on the `sorry', whose goal is 2 + 2 = 4.
      (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
      (with-current-buffer lean4-info-buffer-name
        (should (string-search "2 + 2 = 4" (buffer-string))))
      ;; Now move to the other theorem and do nothing but let the
      ;; post-command hook run, exactly as typing would.
      (lean4-e2e--goto-line 4)
      (back-to-indentation)
      (run-hooks 'post-command-hook)
      (lean4-e2e--wait-until
       "the display to follow point to the other goal"
       (lambda ()
         (accept-process-output nil 0.05)
         (with-current-buffer lean4-info-buffer-name
           (string-search "1 + 1 = 2" (buffer-string)))))
      (with-current-buffer lean4-info-buffer-name
        (should-not (string-search "2 + 2 = 4" (buffer-string)))))))

(defun lean4-e2e--followed-location ()
  "Return the position the display says it is reporting on.
The section following point is the first one in the display, so its
heading -- which names the position -- is the buffer's first line."
  (with-current-buffer lean4-info-buffer-name
    (save-excursion
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(ert-deftest lean4-e2e-goal-display-follows-a-go-to-control ()
  "Pressing a go-to control takes the display where it sends point.

Regression test.  Point moving was watched for by a hook local to the
Lean buffer, so it saw only commands run there -- and a control pressed
by mouse runs with the display's own buffer current, `push-button'
making it current again afterwards.  The jump happened and the display
went on reporting on the position point had left, until the Lean buffer
was given a command of its own."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (let ((source (current-buffer)))
        ;; Start on the `sorry', whose goal is 2 + 2 = 4.
        (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
        (should (string-search "Fixture.lean:8:" (lean4-e2e--followed-location)))
        (with-current-buffer lean4-info-buffer-name
          (should (string-search "2 + 2 = 4" (buffer-string))))
        ;; Press the control beside the type error, which is elsewhere in
        ;; the file entirely.
        (lean4-e2e--wait-until
         "the type error to be listed among the file's messages"
         (lambda ()
           (accept-process-output nil 0.05)
           (with-current-buffer lean4-info-buffer-name
             (string-search "Fixture.lean:10:" (buffer-string)))))
        (with-current-buffer lean4-info-buffer-name
          (goto-char (point-min))
          (should (search-forward "Fixture.lean:10:" nil t))
          (forward-line 0)
          ;; The control set hard right of that message's own row.
          (forward-button 1)
          ;; As a click presses it: `push-button' runs a mouse action with
          ;; the clicked window's buffer current and restores it, so the
          ;; hook that notices point has moved runs here, not in the file.
          (save-current-buffer (push-button))
          (run-hooks 'post-command-hook))
        (with-current-buffer source
          (should (= (line-number-at-pos) 10)))
        (lean4-e2e--wait-until
         "the display to follow the control to the type error"
         (lambda ()
           (accept-process-output nil 0.05)
           (string-search "Fixture.lean:10:" (lean4-e2e--followed-location))))
        (with-current-buffer lean4-info-buffer-name
          (should-not (string-search "2 + 2 = 4" (buffer-string))))))))

;;;; Pinning and pausing

(ert-deftest lean4-e2e-info-buffer-pause-freezes-the-display ()
  "A paused display keeps showing what it showed, wherever point goes."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
            (with-current-buffer lean4-info-buffer-name
              (should (string-search "2 + 2 = 4" (buffer-string))))
            (lean4-info-toggle-pause)
            (should lean4-info-paused)
            ;; Move somewhere with an entirely different goal and refresh.
            (lean4-e2e--goto-line 4)
            (back-to-indentation)
            (lean4-info-buffer-refresh)
            (accept-process-output nil 0.3)
            (with-current-buffer lean4-info-buffer-name
              ;; The goal is what must not move.  The heading does change,
              ;; to report that the display is paused.
              (should (string-search "2 + 2 = 4" (buffer-string)))
              (should-not (string-search "1 + 1 = 2" (buffer-string)))))
        (setq lean4-info-paused nil)))))

(ert-deftest lean4-e2e-info-buffer-pin-follows-its-location ()
  "A pinned position keeps its goal on display beside the followed one.

Pinning no longer stops the display following point: the pinned position
becomes a section of its own above the one point is on, so both goals
are visible at once.  That is what pinning is for -- watching one goal
while working on another."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            ;; Pin to the `sorry', whose goal is 2 + 2 = 4.
            (lean4-e2e--goto-line lean4-e2e--sorry-line)
            (back-to-indentation)
            (lean4-info-toggle-pin)
            (should lean4-info--pins)
            (lean4-e2e--wait-until
             "the pinned goal to appear"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (string-search "2 + 2 = 4" (buffer-string)))))
            ;; Point moves to the other theorem; the display should not.
            (lean4-e2e--goto-line 4)
            (back-to-indentation)
            (lean4-info-buffer-refresh)
            (accept-process-output nil 0.5)
            (lean4-e2e--wait-until
             "the goal at point to appear beside the pinned one"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (string-search "1 + 1 = 2" (buffer-string)))))
            (with-current-buffer lean4-info-buffer-name
              ;; Both: the pinned goal and the one point is on.
              (should (string-search "2 + 2 = 4" (buffer-string)))
              (should (string-search "1 + 1 = 2" (buffer-string)))
              (should (string-search "pinned" (buffer-string))))
            ;; Unpinning takes only that section away.
            (lean4-info-unpin-all)
            (lean4-info--redisplay-source)
            (with-current-buffer lean4-info-buffer-name
              (should-not (string-search "2 + 2 = 4" (buffer-string)))
              (should (string-search "1 + 1 = 2" (buffer-string)))))
        (lean4-info-unpin-all)))))

(ert-deftest lean4-e2e-pin-key-acts-on-the-section-point-is-in ()
  "In the display, the pin key unpins whichever pinned section point is in.

With several pins there is no one pin for a key to mean, so it means the
one point is standing in.  Anywhere else in the display it does what it
would do in the Lean buffer."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (lean4-e2e--goto-line lean4-e2e--sorry-line)
            (back-to-indentation)
            (lean4-info-toggle-pin)
            (lean4-e2e--goto-line 4)
            (back-to-indentation)
            (lean4-info-toggle-pin)
            (should (= (length lean4-info--pins) 2))
            (lean4-e2e--wait-until
             "both pinned sections"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (and (string-search "2 + 2 = 4" (buffer-string))
                      (string-search "1 + 1 = 2" (buffer-string))))))
            (with-current-buffer lean4-info-buffer-name
              ;; Stand in the first pinned section and unpin it.
              (goto-char (point-min))
              (should (search-forward "2 + 2 = 4" nil t))
              (should (get-text-property (point) 'lean4-info-pin))
              (lean4-info-toggle-pin)
              (should (= (length lean4-info--pins) 1))
              ;; The other one is untouched.
              (should (string-search "1 + 1 = 2" (buffer-string)))
              (should-not (string-search "2 + 2 = 4" (buffer-string)))))
        (lean4-info-unpin-all)))))

(ert-deftest lean4-e2e-info-buffer-takes-several-pins ()
  "Any number of positions can be pinned, each a section of its own."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (lean4-e2e--goto-line lean4-e2e--sorry-line)
            (back-to-indentation)
            (lean4-info-toggle-pin)
            (lean4-e2e--goto-line 4)
            (back-to-indentation)
            (lean4-info-toggle-pin)
            (should (= (length lean4-info--pins) 2))
            ;; Pinning the same position again unpins it rather than
            ;; making a second section saying the same thing.
            (lean4-info-toggle-pin)
            (should (= (length lean4-info--pins) 1))
            (lean4-info-toggle-pin)
            (should (= (length lean4-info--pins) 2))
            (lean4-e2e--wait-until
             "both pinned goals"
             (lambda ()
               (with-current-buffer lean4-info-buffer-name
                 (and (string-search "2 + 2 = 4" (buffer-string))
                      (string-search "1 + 1 = 2" (buffer-string)))))))
        (lean4-info-unpin-all)))))

(ert-deftest lean4-e2e-info-buffer-announces-pinned-and-paused ()
  "The heading says when the display has stopped following point.
A goal buffer that has quietly stopped updating looks broken."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (unwind-protect
          (progn
            (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
            (lean4-info-toggle-pause)
            (with-current-buffer lean4-info-buffer-name
              (should (string-search "paused" (buffer-string))))
            (lean4-info-toggle-pause)
            (lean4-e2e--show-goal-at lean4-e2e--sorry-line)
            (with-current-buffer lean4-info-buffer-name
              (should-not (string-search "paused" (buffer-string))))
            (lean4-info-toggle-pin)
            (lean4-e2e--wait-until
             "the heading to report the pin"
             (lambda ()
               (accept-process-output nil 0.05)
               (with-current-buffer lean4-info-buffer-name
                 (string-search "pinned" (buffer-string))))))
        (setq lean4-info-paused nil)
        (lean4-info-unpin-all)))))

;;;; Inlay hints

(defun lean4-e2e--current-line ()
  "Return the text of the line point is on."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun lean4-e2e--wait-for-hint ()
  "Wait until the server offers an inlay hint on the current line.
Hints arrive with elaboration, which the fixture's error only proves has
reached the error."
  (lean4-e2e--wait-until
   "an inlay hint on the declaration"
   (lambda ()
     (lean4-hints--nearest
      (jsonrpc-request (eglot-current-server) :textDocument/inlayHint
                       (lean4-hints--params))
      (point)))))

(ert-deftest lean4-e2e-inlay-hint-is-offered ()
  "The server sends a hint carrying both an edit and the inferred type."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--hint-line)
    (end-of-line)
    (let ((hint (lean4-e2e--wait-for-hint)))
      (should (equal (string-trim (lean4-hints--label hint)) "{α}"))
      (should (lean4-hints--insertion hint))
      (should (string-search "α : Sort"
                             (plist-get (plist-get hint :tooltip) :value))))))

(ert-deftest lean4-e2e-inlay-hint-is-inserted ()
  "The command writes what Lean inferred into the buffer.
The fixture is left alone: the buffer is discarded unsaved."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--hint-line)
    (end-of-line)
    (lean4-e2e--wait-for-hint)
    (should (equal (lean4-e2e--current-line) "def autoBound (a : α) : α := a"))
    (lean4-insert-inlay-hint)
    (should (equal (lean4-e2e--current-line)
                   "def autoBound {α} (a : α) : α := a"))))

(ert-deftest lean4-e2e-inlay-hint-reaches-eldoc ()
  "ElDoc is told the inferred type and how to write it down."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--hint-line)
    (end-of-line)
    (lean4-e2e--wait-for-hint)
    (let (report)
      (should (lean4-hints-eldoc-function
               (lambda (documentation &rest _) (setq report documentation))))
      (lean4-e2e--wait-until "ElDoc to be told about the hint"
                             (lambda () report))
      (should (string-search "α : Sort" report))
      ;; The key of `lean4-apply-suggestion', that being the one bound.
      (should (string-search "C-c C-." report)))))

(ert-deftest lean4-e2e-inlay-hint-eldoc-answers-with-nothing ()
  "Where there is no hint, ElDoc is told so rather than left waiting.
`eldoc-documentation-compose', which is the strategy Eglot sets, shows
what it has collected only once every function has answered.  A function
which promises an answer and never gives it silences the whole echo area
-- the hover, the signature, and Eglot's own report of an available code
action among it -- on every line without a hint, which is most of them."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (let ((reported 'never))
      (should (lean4-hints-eldoc-function
               (lambda (&rest arguments) (setq reported arguments))))
      (lean4-e2e--wait-until "ElDoc to be answered at all"
                             (lambda () (not (eq reported 'never))))
      (should (equal reported '(nil))))))

(ert-deftest lean4-e2e-inlay-hint-eldoc-function-is-installed ()
  "The report joins Eglot's own ElDoc functions rather than replacing them.
Composed, and appended: what Lean inferred is a footnote to the type of
the thing under point."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (should (eq eldoc-documentation-strategy #'eldoc-documentation-compose))
    (should (memq #'lean4-hints-eldoc-function eldoc-documentation-functions))
    (should (memq #'eglot-hover-eldoc-function eldoc-documentation-functions))
    (should (> (seq-position eldoc-documentation-functions
                             #'lean4-hints-eldoc-function)
               (seq-position eldoc-documentation-functions
                             #'eglot-hover-eldoc-function)))))

(ert-deftest lean4-e2e-inlay-hint-declines-elsewhere ()
  "Where Lean offers no hint, the command says so and changes nothing."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--sorry-line)
    (back-to-indentation)
    (should-error (lean4-insert-inlay-hint) :type 'user-error)
    (should-not (buffer-modified-p))))

;;;; Searching a trace

(ert-deftest lean4-e2e-trace-search-opens-what-holds-the-match ()
  "Lean answers a search with the message again, opened where the match is.
The fixture's trace is a `synthInstance' one, whose children Lean has not
sent: what comes back has them, thinned to those a match is in, and the
matching text marked.  Which is the point of asking the server rather than
searching the buffer -- most of what is searched was never in it."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--with-info-window
      (lean4-e2e--show-goal-at lean4-e2e--trace-line)
      (with-current-buffer lean4-info-buffer-name
        (let* ((section (lean4-e2e--wait-until
                         "a message with a trace in it"
                         (lambda ()
                           (accept-process-output nil 0.05)
                           (goto-char (point-min))
                           (when (re-search-forward "Meta.synthInstance" nil t)
                             (lean4-info--message-section-at-point)))))
               (place (progn (goto-char (oref section start))
                             (lean4-info--message-place-at-point)))
               (message (lean4-info--message-at-place place))
               (answer nil))
          (should place)
          (should message)
          (should (lean4-info--has-trace-p message))
          (lean4-rpc-highlight-matches
           lean4-info--handle "Inhabited" message
           (lambda (highlighted) (setq answer highlighted))
           (lambda (error) (error "Search failed: %S" error)))
          (lean4-e2e--wait-until "the search to come back"
                                 (lambda () answer))
          ;; The answer carries the marks a plain message never does.
          (should (lean4-e2e--marks-a-match-p answer)))))))

(defun lean4-e2e--marks-a-match-p (object)
  "Return non-nil if OBJECT carries a search-match tag anywhere in it.
Walked without regard for shape.  What matters is that the answer carries
marks the message did not; where in the tree they sit is Lean's business,
and in practice they are deep -- inside the terms inside the messages of
the trace children it fetched.

"
  (cond ((stringp object) (lean4-render--highlight-p object))
        ((consp object) (or (lean4-e2e--marks-a-match-p (car object))
                            (lean4-e2e--marks-a-match-p (cdr object))))
        ((vectorp object) (seq-some #'lean4-e2e--marks-a-match-p object))
        (t nil)))

;;;; Folding, against what the server folds

(ert-deftest lean4-e2e-outline-headings-cover-the-servers-ranges ()
  "Every region Lean would fold begins at a heading, or just above one.
This is the check that the outline model is Lean's and not an invention:
`textDocument/foldingRange' is asked what it would fold, and each answer
has to line up with a heading here.

\"Just above one\" is one known divergence, and the reason the test is
written this loosely: Lean folds `set_option ... in' together with the
declaration it prefixes, beginning the region at the `set_option', where
this puts the heading on the declaration itself."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (let ((ranges (lean4-e2e--wait-until
                   "the server's folding ranges"
                   (lambda ()
                     (let ((answer (jsonrpc-request
                                    (eglot-current-server)
                                    :textDocument/foldingRange
                                    (list :textDocument
                                          (eglot--TextDocumentIdentifier)))))
                       (and (not (seq-empty-p answer)) answer))))))
      (seq-doseq (range ranges)
        (lean4-e2e--goto-line (plist-get range :startLine))
        (should (or (lean4-outline-search nil nil nil t)
                    (save-excursion
                      (forward-line 1)
                      (lean4-outline-search nil nil nil t))))))))

;;;; Applying what Lean suggests

(defun lean4-e2e--pick-first-suggestion ()
  "Call `lean4-apply-suggestion', taking the first thing offered."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _) (car (car collection)))))
    (lean4-apply-suggestion)))

(ert-deftest lean4-e2e-suggestion-offers-try-this ()
  "A `simp?' suggestion reaches us as a code action."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--suggestion-line)
    (search-forward "simp?")
    (goto-char (match-beginning 0))
    (let ((actions (lean4-e2e--wait-until
                    "the \"Try this\" code action"
                    (lambda () (lean4-suggest--code-actions)))))
      (should (seq-find (lambda (action)
                          (string-search "simp" (plist-get action :title)))
                        actions)))))

(ert-deftest lean4-e2e-suggestion-applies-try-this ()
  "Choosing it rewrites the tactic, Eglot applying the edit."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--suggestion-line)
    (search-forward "simp?")
    (goto-char (match-beginning 0))
    (lean4-e2e--wait-until "the \"Try this\" code action"
                           (lambda () (lean4-suggest--code-actions)))
    (lean4-e2e--pick-first-suggestion)
    (should-not (string-search "simp?" (lean4-e2e--current-line)))
    (should (string-search "simp" (lean4-e2e--current-line)))))

(ert-deftest lean4-e2e-suggestion-offers-the-inlay-hint ()
  "Where the only thing on offer is the hint, the same key inserts it.
Without asking: a single suggestion is applied and named, since a prompt
offering one answer is a keystroke spent on nothing.  The stub here
signals rather than choosing, so a prompt would fail the test."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (lean4-e2e--goto-line lean4-e2e--hint-line)
    (end-of-line)
    (lean4-e2e--wait-for-hint)
    (should (equal (mapcar #'car (lean4-suggest--candidates
                                  (lean4-suggest--code-actions)
                                  (lean4-suggest--hint)))
                   '("Insert \" {α}\"")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Asked about a single suggestion"))))
      (lean4-apply-suggestion))
    (should (equal (lean4-e2e--current-line)
                   "def autoBound {α} (a : α) : α := a"))))

(ert-deftest lean4-e2e-suggestion-says-so-when-there-is-nothing ()
  "Where Lean offers nothing, it says so rather than signalling."
  :tags '(:e2e)
  (lean4-e2e--with-fixture
    (goto-char (point-min))
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (setq said (apply #'format format arguments)))))
        (lean4-apply-suggestion))
      (should (equal said "Lean suggests nothing here")))
    (should-not (buffer-modified-p))))

(provide 'lean4-e2e-test)
;;; lean4-e2e-test.el ends here
