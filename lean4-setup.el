;;; lean4-setup.el --- Checking a Lean setup before using it -*- lexical-binding: t -*-

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

;; Saying what is wrong with a Lean setup at the point where it stops
;; working, rather than leaving the reader to work back from a server that
;; never started.
;;
;; VS Code checks its setup before starting Lean and splits what it checks
;; in two: some things are about the machine -- Git and curl, elan, whether
;; a Lean can be found at all -- and some about the project a file belongs
;; to, above all whether it has a "lean-toolchain" and whether it is a Lean 3
;; project.  Both are checked here, the first once a session and the second
;; once per project, since neither answer changes as one moves between files.
;;
;; `M-x lean4-diagnose' has reported all of this since version 2, on request.
;; What is new is nothing more than asking without being asked, which is the
;; whole of the difference between a reader who knows their toolchain is
;; missing and one who has a Lean buffer where nothing happens.
;;
;; Everything goes through `display-warning', so it lands in a buffer that
;; waits to be read rather than in an echo area that scrolls away, and
;; `warning-minimum-level' and its relatives govern it as they govern
;; everything else.  One check per problem, in the order the reader will hit
;; them: what is missing from the machine before what is wrong with the
;; project, since a machine with no Lean makes every project unusable.

;;; Code:

(require 'seq)

(require 'lean4-eglot)
(require 'lean4-settings)
(require 'lean4-toolchain)
(require 'lean4-util)

;; Defined in lean4-mode.el, which requires this file rather than the other
;; way round; the mode body is the only caller of anything here, so it is
;; loaded by the time any of this runs.
(declare-function lean4--toolchain-string "lean4-mode")

(defcustom lean4-show-setup-warnings t
  "Whether to say so when something about the Lean setup looks wrong.
Reported through `display-warning' when a Lean file is visited: what is
missing from the machine once a session, and what is wrong with a project
once per project.  VS Code\\='s `showSetupWarnings', which defaults on as
well.

Nil says nothing and changes nothing else; `M-x lean4-diagnose' still
reports the same and more, on request."
  :group 'lean4
  :type 'boolean)

(defvar lean4-setup--machine-checked nil
  "Non-nil once the machine has been looked at this session.
What is installed does not change as one moves between files, and saying
so once is the difference between a warning and a nag.")

(defvar lean4-setup--projects-checked nil
  "The workspace roots looked at this session.")

(defun lean4-setup--warn (level format &rest arguments)
  "Report FORMAT with ARGUMENTS as a Lean setup warning of LEVEL.
The type is a list beginning `lean4', which is what makes
`warning-suppress-types' useful here: an entry of `(lean4)' silences
every warning this package raises, and `(lean4 setup)' only these.  A flat
symbol would have to be listed one by one."
  (display-warning '(lean4 setup) (apply #'format format arguments) level))

(defun lean4-setup--machine-problems ()
  "Return what is wrong with this machine, as a list of level and text.
Ordered as VS Code orders them: what stops Lean working before what only
holds part of this package back."
  (let ((problems nil))
    (when-let* ((missing (lean4-missing-dependencies)))
      (push (list :error
                  (format "%s missing.  Elan downloads toolchains with curl \
and Lake clones dependencies with Git, so neither works without them.  \
`M-x lean4-install-dependencies' offers to install %s"
                          (string-join missing " and ")
                          (if (cdr missing) "them" "it")))
            problems))
    (unless (executable-find (lean4--program lean4-executable-name))
      (push (list :error
                  (format "No `%s' on `exec-path'.  Normally elan puts one \
there; `M-x lean4-install-elan' installs elan, and `M-x \
lean4-install-toolchain' a Lean for it to run"
                          lean4-executable-name))
            problems))
    (unless (executable-find (lean4--program lean4-lake-name))
      (push (list :error
                  (format "No `%s' on `exec-path'.  A file inside a Lake \
package is served by `%s serve', so such a file will not be served"
                          lean4-lake-name lean4-lake-name))
            problems))
    (let ((elan (lean4-toolchain--elan-major-version)))
      (cond ((null elan)
             (push (list :warning
                         "Elan not found.  Lean will still run if one is on \
`exec-path', but nothing here can choose a version without elan; `M-x \
lean4-install-elan' installs it")
                   problems))
            ;; The same constant the toolchain commands themselves refuse
            ;; below, so that a warning and a refusal cannot come to
            ;; disagree about which elan is new enough.
            ((< elan lean4-toolchain--elan-minimum)
             (push (list :warning
                         (format "Elan %d is older than %d, which is where \
`elan dump-state' arrives; the commands under `Lean version' need it.  `M-x \
lean4-update-elan' updates it"
                                 elan lean4-toolchain--elan-minimum))
                   problems))))
    (nreverse problems)))

(defun lean4-setup--lean3-p (root)
  "Return non-nil if ROOT looks like a Lean 3 project.
Lean 3 read its package configuration from \"leanpkg.toml\", which Lake
replaced; a directory with one and no \"lean-toolchain\" is Lean 3\\='s."
  (and (file-readable-p (expand-file-name "leanpkg.toml" root))
       (not (file-readable-p (expand-file-name "lean-toolchain" root)))))

(defun lean4-setup--uninstalled-toolchain ()
  "Return the Lean this buffer's project pins and the machine has not got.
Nil if it has it, or if there is nothing pinned to have.

Only a version can be answered for.  A project pinning a channel --
`leanprover/lean4:stable' -- is pinning whatever that resolves to today,
which is elan's to decide and may be a version nobody has yet; there is
nothing to warn about that would still be true tomorrow.

Compared by version rather than by name, a `lean-toolchain' being free to
say `4.33.0' where elan reports `v4.33.0'.  If elan cannot be asked at
all, nothing is claimed: that is a warning of its own, from the machine
check."
  (when-let* ((pinned (lean4--toolchain-string))
              (parsed (lean4-toolchain--parse pinned))
              ((not (eq (car parsed) 'unknown)))
              (installed (ignore-errors (lean4-toolchain-installed)))
              ((not (seq-some
                     (lambda (version)
                       (equal (cadr parsed)
                              (cadr (lean4-toolchain--parse version))))
                     installed))))
    pinned))

(defun lean4-setup--project-problems (root)
  "Return what is wrong with the project at ROOT, as a list of level and text."
  (cond
   ((lean4-setup--lean3-p root)
    (list (list :error
                (format "%s has a `leanpkg.toml' and no `lean-toolchain', so \
it is a Lean 3 project.  This mode only supports Lean 4; Lean 3 has \
Lean3-Mode, at https://github.com/leanprover/lean3-mode" root))))
   ((not (file-readable-p (expand-file-name "lean-toolchain" root)))
    (list (list :warning
                (format "%s has no `lean-toolchain', so its files are served \
by whatever elan's default resolves to rather than by a version the project \
pins.  `M-x lean4-select-project-toolchain' writes one" root))))
   (t
    ;; Asked once: it runs elan.
    (when-let* ((pinned (lean4-setup--uninstalled-toolchain)))
      (list (list :warning
                  (format "%s pins %s, which elan has not got.  Opening a \
file here downloads the whole toolchain first, which on a first connection \
can outlast `eglot-connect-timeout' and look like a hang; `M-x \
lean4-install-toolchain' fetches it now instead"
                          root pinned)))))))

(defun lean4-setup--check-machine ()
  "Say what is wrong with this machine, once a session."
  (unless lean4-setup--machine-checked
    (setq lean4-setup--machine-checked t)
    (pcase-dolist (`(,level ,text) (lean4-setup--machine-problems))
      (lean4-setup--warn level "%s" text))))

(defun lean4-setup--check-project ()
  "Say what is wrong with this buffer\\='s project, once per project."
  (when-let* ((root (lean4--workspace-root)))
    (unless (member root lean4-setup--projects-checked)
      (push root lean4-setup--projects-checked)
      (pcase-dolist (`(,level ,text) (lean4-setup--project-problems root))
        (lean4-setup--warn level "%s" text)))))

(defun lean4-setup-check ()
  "Say what looks wrong about the Lean setup for this buffer.
Called when a Lean file is visited, and each part of it only once: the
machine per session, a project the first time a file in it is opened.
Says nothing at all if `lean4-show-setup-warnings' is nil.

VS Code has one more check, for a file it is editing which has not been
saved anywhere, since Lean cannot serve one.  It is not here: a buffer
with no file behind it is an ordinary thing in Emacs rather than a setup
gone wrong -- editing a Lean block inside an Org or Markdown document
gives one, and so does every `with-temp-buffer' -- and a warning that
arrives in all of those is one that gets turned off, taking the two above
with it.

Which is why the variable `buffer-file-name' gates the machine check as
well as the project one.  `markdown-code-lang-modes' maps Lean blocks to
this mode,
and Markdown fontifies such a block by running the mode in a temporary
buffer: without the guard, a reader whose elan is missing meets the news
in a `*Warnings*' window that a redisplay opened, with no Lean file in
sight."
  (when (and lean4-show-setup-warnings buffer-file-name)
    (lean4-setup--check-machine)
    (lean4-setup--check-project)))

(defun lean4-setup-forget ()
  "Forget what has been checked, so that it is checked again.
For after installing something, and for the tests."
  (interactive)
  (setq lean4-setup--machine-checked nil
        lean4-setup--projects-checked nil))

(provide 'lean4-setup)
;;; lean4-setup.el ends here
