;;; lean4-eglot.el --- Eglot glue for lean4-mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Everything that reaches into Eglot lives here, and nothing outside this
;; file may use an `eglot--' symbol.  Eglot's public surface is small and its
;; private surface moves between releases, so confining the exposure to one
;; library keeps the blast radius of an Eglot upgrade to a single file.
;;
;; The supported range is Emacs 29.1 (Eglot 1.12) upwards.  Rather than
;; requiring a newer Eglot from GNU ELPA -- which would force an upgrade of
;; the preloaded `eldoc', a well-known way to break a running Emacs -- the
;; handful of functions Eglot renamed in 1.16 are aliased below, and anything
;; newer than 1.12 is used only behind `fboundp'.
;;
;; This file also owns the pieces of the Lean protocol that are about
;; connection management rather than about presentation: how the server is
;; launched, which directory counts as the workspace, and the fan-out of
;; `$/lean/fileProgress'.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'project)
(require 'seq)

(require 'lean4-settings)
(require 'lean4-util)

(declare-function lean4-root-dir-p "lean4-lake")

;;;; Compatibility with Eglot 1.12 (Emacs 29)

;; Eglot 1.16 renamed these four from `eglot--X' to `eglot-X'.  Bind our own
;; name to whichever exists, rather than defining the new `eglot-' names
;; ourselves: those belong to Eglot, and defining them here would shadow a
;; future Eglot whose version of them differs.
(eval-and-compile
  (defmacro lean4--defalias-eglot (name new old)
    "Define NAME as whichever of Eglot's NEW or OLD function exists."
    `(defalias ',name (if (fboundp ',new) ',new ',old))))

(lean4--defalias-eglot lean4--uri-to-path
                       eglot-uri-to-path eglot--uri-to-path)
(lean4--defalias-eglot lean4--path-to-uri
                       eglot-path-to-uri eglot--path-to-uri)
(lean4--defalias-eglot lean4--range-region
                       eglot-range-region eglot--range-region)
(lean4--defalias-eglot lean4--server-capable
                       eglot-server-capable eglot--server-capable)

(defconst lean4--eglot-internals
  '(eglot--current-server-or-lose
    eglot--TextDocumentPositionParams
    eglot--TextDocumentIdentifier
    eglot--pos-to-lsp-position
    eglot--lsp-position-to-point
    eglot--capabilities
    eglot--managed-buffers
    eglot--signal-textDocument/didOpen
    eglot--signal-textDocument/didClose)
  "Private Eglot functions this package calls.
Eglot exposes no public equivalent for any of these.")

(defun lean4--check-eglot-internals ()
  "Warn about private Eglot functions this package needs but cannot find.
Eglot's internals move between releases.  Failing loudly at load time,
naming the symbol, beats a `void-function' backtrace surfacing weeks
later from inside a jsonrpc callback."
  (when-let* ((missing (seq-remove #'fboundp lean4--eglot-internals)))
    (display-warning
     'lean4
     (format "This Eglot is missing %s, which lean4-mode uses.  \
Some features will not work.  Please report this together with your \
Emacs and Eglot versions."
             (mapconcat #'symbol-name missing ", "))
     :warning)))

(lean4--check-eglot-internals)

;;;; The server

(defclass lean4-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Eglot server subclass for the Lean 4 language server.
Exists so that Lean's protocol extensions can be attached with
`cl-defmethod' without affecting other languages' servers.")

;; Declared before the referent so that a user who had customized the old
;; name still has that value when the defcustom below is evaluated.
(define-obsolete-variable-alias
  'lean4-extra-arguments 'lean4-server-arguments "2.0.0")

(defcustom lean4-server-arguments nil
  "Extra command-line arguments passed to the Lean language server.
The counterpart of VS Code's `lean4.serverArgs'."
  :group 'lean4
  :type '(repeat string))

(defun lean4--server-command (root)
  "Return the command starting a Lean language server for ROOT.
A Lake package is served by Lake itself, so that the package's
dependencies are on the search path; a loose file is served by Lean
directly.  ROOT may be nil, meaning there is no enclosing package."
  (require 'lean4-lake)
  (append
   (if (and root (lean4-root-dir-p root))
       (list (lean4--program lean4-lake-name) "serve" "--")
     (list (lean4--program lean4-executable-name) "--server"))
   lean4-server-arguments))

(defun lean4--server-contact (&optional _interactive)
  "Return the Eglot contact for the Lean server in the current buffer.
Deliberately does not run `lake --version' to decide what to launch:
Eglot calls this before binding `default-directory' to the project root,
so such a probe would consult whichever toolchain happens to govern the
current directory, which in a multi-toolchain checkout is the wrong one.
The decision is made from the workspace root instead."
  (cons 'lean4-eglot-lsp-server
        (lean4--server-command (lean4--workspace-root))))

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (cons '(lean4-mode :language-id "lean4")
                     #'lean4--server-contact)))

;;;; Initialization options

(cl-defmethod eglot-initialization-options
  ((_server lean4-eglot-lsp-server))
  "Tell Lean this client can display interactive content.

Without `hasWidgets', the server flattens messages before sending them:
traces arrive as pre-indented text rather than as tree nodes, and no
`MsgEmbed' tags appear at all, so nothing in a message can be folded or
interacted with."
  (list :hasWidgets t))

;;;; Client capabilities

(defvar lean4-client-capabilities
  '(:rpcWireFormat "v1" :silentDiagnosticSupport t)
  "Lean's own additions to the LSP client capabilities.

`rpcWireFormat' picks how server-side references are encoded.  Version 0
names the field \"p\", which can collide with a field of the same name
in user data (leanprover/vscode-lean4#712); v1 renames it to
\"__rpcref\".  Asking for v1 is safe against an older toolchain: a
server that has never heard of the capability simply keeps speaking v0,
and `lean4-rpc' reads back what was actually negotiated.

`silentDiagnosticSupport' asks for the diagnostics Lean means for the
goal display rather than the editor, the completed-proof report among
them.  Both halves of that matter: without the capability Lean withholds
them entirely, and asking is still not enough, because they are never
pushed -- they arrive only through
`Lean.Widget.getInteractiveDiagnostics'.  Both verified against Lean
4.32.2 by removing each in turn.

Declared here, in one list, rather than appended to from the libraries
that care: a capability added at load time is a capability that depends
on whether that library happened to be loaded.")

(cl-defmethod eglot-client-capabilities :around
  ((_server lean4-eglot-lsp-server))
  "Add Lean's non-standard capabilities to the standard set."
  (let ((capabilities (cl-call-next-method)))
    ;; Lean reads these from `ClientCapabilities.lean?'.
    (plist-put capabilities :lean lean4-client-capabilities)
    capabilities))

;;;; Workspace discovery

(defcustom lean4-workspace-roots nil
  "Directories to treat as Lean workspace roots.
Checked before the search for a \"lean-toolchain\" file."
  :group 'lean4
  :type '(repeat directory))

(defcustom lean4-workspace-exclusions nil
  "Directories in which never to start a Lean language server."
  :group 'lean4
  :type '(repeat directory))

(defun lean4--directory-member-p (directory directories)
  "Return non-nil if DIRECTORY is one of DIRECTORIES."
  (seq-some (lambda (candidate)
              (and (file-exists-p candidate)
                   (file-equal-p directory candidate)))
            directories))

(defun lean4--workspace-root (&optional file-name)
  "Return the Lean workspace root governing FILE-NAME, or nil.
FILE-NAME defaults to the current buffer's file.

`lean4-workspace-roots' and `lean4-workspace-exclusions' win if either
is found first; an excluded directory yields nil.  Otherwise the search
looks for \"lean-toolchain\" and keeps going upwards, returning the
*outermost* directory that has one.  That is deliberate: a Lake package
vendored under `.lake/packages' carries its own toolchain file, but it
has to be elaborated by the toolchain of the project that pulled it in,
so the enclosing one is the one that matters."
  (when-let* ((file-name (or file-name buffer-file-name)))
    (let (root excluded)
      (if-let* ((configured
                 (locate-dominating-file
                  file-name
                  (lambda (dir)
                    (and (file-directory-p dir)
                         (or (lean4--directory-member-p dir lean4-workspace-roots)
                             (setq excluded
                                   (lean4--directory-member-p
                                    dir lean4-workspace-exclusions))))))))
          (unless excluded (setq root configured))
        (let ((from file-name))
          (while-let ((dir (and from (locate-dominating-file
                                      from "lean-toolchain"))))
            (setq root dir
                  from (file-name-directory (directory-file-name dir))))))
      root)))

(cl-defmethod project-root ((project (head lean4)))
  "Return the root directory of a Lean PROJECT.
A Lean project is the cons (lean4 . DIRECTORY)."
  (cdr project))

(cl-defmethod project-name ((project (head lean4)))
  "Return a display name for the Lean PROJECT.
Used for Eglot's mode-line nickname and for `workspaceFolders'."
  (file-name-nondirectory (directory-file-name (cdr project))))

(defun lean4-project-find (file-name)
  "Return the Lean project containing FILE-NAME, for `project-find-functions'.
Answers only when Eglot is asking.  Lean workspaces are rooted at a
toolchain file, which is rarely what the user means by \"project\" for
version control or `project-find-file', so claiming every Lean file
unconditionally would hijack `project.el' for the whole repository."
  (when (and (stringp file-name)
             (bound-and-true-p eglot-lsp-context))
    (when-let* ((root (lean4--workspace-root file-name)))
      (cons 'lean4 root))))

(defun lean4-register-project-backend ()
  "Have project.el ask us where a Lean file's project root is.

Registered when the first Lean file is opened rather than when the
package loads: `project-find-functions' is a global hook, and a package
that has been installed but not used has no business being in it."
  (add-hook 'project-find-functions #'lean4-project-find))

;;;; Buffers and URIs

(defun lean4--uri-to-truename (uri)
  "Return the true file name URI denotes, or nil if it names no file."
  (ignore-errors
    (when-let* ((path (lean4--uri-to-path uri)))
      (file-truename path))))

(defmacro lean4-with-uri-buffers (server uri &rest body)
  "Evaluate BODY in each buffer of SERVER visiting the file named by URI.

Buffers are matched on `buffer-file-truename' rather than on Eglot's
`eglot--TextDocumentIdentifier-cache': that cache does not exist before
Eglot 1.16, so a `boundp' guard around it silently matches no buffers at
all on Emacs 29 and quietly disables everything driven from server
notifications."
  (declare (indent 2) (debug (form form &rest form)))
  (let ((truename (make-symbol "truename")))
    `(when-let* ((,truename (lean4--uri-to-truename ,uri)))
       (dolist (buf (eglot--managed-buffers ,server))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and buffer-file-truename
                        (file-equal-p buffer-file-truename ,truename))
               ,@body)))))))

;;;; File progress

(defvar lean4-file-progress-functions nil
  "Functions called when the server reports elaboration progress.
Each is called with three arguments: the server, the URI of the file
being elaborated, and the raw `processing' array, a vector of plists
with `:range' and `:kind' keys.

An abnormal hook rather than a direct call, because several subsystems
care about progress -- the fringe indicator, semantic tokens and the
info buffer -- and the notification handler should not have to know
about any of them.")

(cl-defmethod eglot-handle-notification ((server lean4-eglot-lsp-server)
                                         (_method (eql $/lean/fileProgress))
                                         &key textDocument processing
                                         &allow-other-keys)
  "Broadcast Lean's TEXTDOCUMENT elaboration PROCESSING ranges from SERVER."
  (let ((uri (plist-get textDocument :uri)))
    (run-hook-with-args 'lean4-file-progress-functions server uri processing)))

;;;; Semantic tokens

;; Lean leans on semantic tokens far more than most languages: almost nothing
;; in a Lean file can be classified without elaborating it, so regexp
;; font-lock can only ever colour keywords.
;;
;; Eglot grew `eglot-semantic-tokens-mode' in 1.20, which is newer than the
;; Eglot bundled with either Emacs 29 (1.12) or Emacs 30 (1.17).  We ask for
;; it when it is there and say nothing when it is not, rather than shipping a
;; second implementation of a standard LSP feature: `M-x package-install RET
;; eglot' gets a user on any supported Emacs the upstream one.

(defface lean4-semantic-leanSorryLike
  '((t :inherit font-lock-warning-face))
  "Face for `sorry' and other proof-admitting syntax.
Lean reports these with its own `leanSorryLike' semantic token type;
they abandon a proof, so they should be hard to miss."
  :group 'lean4)

(defun lean4--setup-semantic-tokens ()
  "Turn on Eglot's semantic-token highlighting, if this Eglot has it."
  (when (and (fboundp 'eglot-semantic-tokens-mode)
             (boundp 'eglot-semantic-token-types))
    ;; `leanSorryLike' is Lean's own token type, so Eglot does not know to
    ;; ask for it.  Its face is found by name, hence the defface above.
    (unless (member "leanSorryLike" eglot-semantic-token-types)
      (setq eglot-semantic-token-types
            (append eglot-semantic-token-types '("leanSorryLike"))))
    (eglot-semantic-tokens-mode 1)))

;;;; Completion

(defun lean4-eglot-completion-at-point ()
  "Eglot's completion, but not exclusive.

Eglot's own entry sets no `:exclusive', which makes it exclusive: when
the server offers nothing, completion stops there rather than trying
anything else.  Lean's completion is contextual -- tactics inside `by',
identifiers and dot-completion inside terms -- and it returns nothing at
all for a bare word at the top level.  Left exclusive, that silences
every other source the reader has configured, at precisely the positions
where the server is no help."
  (when-let* ((result (eglot-completion-at-point)))
    (if (plist-member (nthcdr 3 result) :exclusive)
        result
      (append result '(:exclusive no)))))

(defun lean4--setup-completion ()
  "Use the non-exclusive wrapper in place of Eglot's own entry.
Run from `eglot-managed-mode-hook', which is when Eglot installs it."
  (when (memq #'eglot-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (cl-substitute #'lean4-eglot-completion-at-point
                               #'eglot-completion-at-point
                               completion-at-point-functions))))

;;;; Diagnostics

(defcustom lean4-diagnostics-delay 0.05
  "Seconds to wait before reacting to a batch of diagnostics.
Lean publishes diagnostics repeatedly while elaborating a file, so
responding to each notification individually would redisplay the info
buffer far more often than anyone can read it."
  :group 'lean4
  :type 'number)

(defvar lean4--diagnostics-timer nil
  "Timer coalescing bursts of `textDocument/publishDiagnostics'.")

(defun lean4--jsonrpc-busy-p (server)
  "Return non-nil if SERVER still has synchronous requests outstanding.
Issuing another request while jsonrpc is waiting on one nests the event
loop.  The accessor arrived in jsonrpc 1.0.17, after the version Emacs
29 ships; treat its absence as \"not busy\"."
  (and (fboundp 'jsonrpc--sync-request-alist)
       (jsonrpc--sync-request-alist server)))

(defun lean4--handle-diagnostics (server uri)
  "Refresh the info buffer and Flymake for URI, managed by SERVER."
  (setq lean4--diagnostics-timer nil)
  (if (lean4--jsonrpc-busy-p server)
      (lean4--schedule-diagnostics server uri)
    (lean4-with-uri-buffers server uri
      (when (fboundp 'lean4-info-buffer-refresh)
        (lean4-info-buffer-refresh))
      (flymake-start))))

(defun lean4--schedule-diagnostics (server uri)
  "Arrange to react to SERVER's diagnostics for URI, once, shortly."
  (unless lean4--diagnostics-timer
    (setq lean4--diagnostics-timer
          (run-with-timer lean4-diagnostics-delay nil
                          #'lean4--handle-diagnostics server uri))))

(cl-defmethod eglot-handle-notification :after
  ((server lean4-eglot-lsp-server)
   (_method (eql textDocument/publishDiagnostics))
   &key uri &allow-other-keys)
  "Refresh Lean's own views after SERVER publishes diagnostics for URI."
  (lean4--schedule-diagnostics server uri))

;;;; File watching

(defcustom lean4-enable-file-watchers nil
  "Whether to honour the server's request to watch files.
The Lean server asks to watch every \"*.ilean\" file in the workspace.
On a project the size of Mathlib that is tens of thousands of files, and
the notifications it generates cost more than the stale-index detection
it buys."
  :group 'lean4
  :type 'boolean)

(cl-defmethod eglot-register-capability ((_server lean4-eglot-lsp-server)
                                         (_method (eql workspace/didChangeWatchedFiles))
                                         _id &key _watchers)
  "Honour the server's file-watch request only when configured to."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

(cl-defmethod eglot-unregister-capability ((_server lean4-eglot-lsp-server)
                                           (_method (eql workspace/didChangeWatchedFiles))
                                           _id)
  "Undo a file-watch registration only if one was made."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

;;;; Commands

(defun lean4-refresh-file-dependencies ()
  "Reload the current file and its imports on the server.
Closing and reopening the document makes the file worker restart, which
is what picks up a dependency that has been rebuilt since the file was
opened."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Lean server is running for this buffer"))
  (eglot--signal-textDocument/didClose)
  (eglot--signal-textDocument/didOpen))

(provide 'lean4-eglot)
;;; lean4-eglot.el ends here
