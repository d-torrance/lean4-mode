;;; lean4-toolchain.el --- Choose which Lean to use -*- lexical-binding: t -*-

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

;; Which Lean a project uses, and which one everything else uses.  Four
;; commands, the counterparts of VS Code's:
;;
;;   `lean4-select-project-toolchain'  writes "lean-toolchain"
;;   `lean4-select-default-toolchain'  `elan default'
;;   `lean4-update-release-channel'   `elan toolchain install <channel>'
;;   `lean4-uninstall-toolchains'     `elan toolchain uninstall'
;;
;; All four need elan 4 or later, which is where `elan dump-state' arrives;
;; VS Code requires the same and says so, since without it the versions
;; cannot be listed with what elan has resolved them to.
;;
;; The versions offered are not only the installed ones.  As VS Code does,
;; release.lean-lang.org is asked for the stable, beta and nightly releases as
;; well, so a version that is not installed can be chosen and elan fetches it.
;; A failure to reach it is a warning rather than an error: the installed
;; versions are still worth offering.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'url)

(require 'lean4-eglot)
(require 'lean4-settings)
(require 'lean4-util)

(defgroup lean4-toolchain nil
  "Choosing which Lean to use."
  :group 'lean4)

(defcustom lean4-toolchain-releases-url "https://release.lean-lang.org/"
  "Where to ask which Lean versions have been released.
The same address VS Code asks."
  :group 'lean4-toolchain
  :type 'string)

(defcustom lean4-elan-name "elan"
  "How to invoke elan."
  :group 'lean4-toolchain
  :type 'string)

(defconst lean4-toolchain-stable "leanprover/lean4:stable"
  "The release channel that always means the newest stable version.")

(defconst lean4-toolchain-nightly "leanprover/lean4:nightly"
  "The release channel that always means the newest nightly version.")

(defconst lean4-toolchain--elan-minimum 4
  "The oldest elan whose `dump-state' these commands can read.
VS Code requires the same major version, for the same reason.")

;;;; Running elan

(defun lean4-toolchain--elan (&rest arguments)
  "Run elan with ARGUMENTS and return a cons of its exit status and output.
The status is the symbol `not-found' where elan could not be launched at
all, which is a different thing from a command that failed."
  (with-temp-buffer
    (let ((status (condition-case nil
                      (apply #'call-process (lean4--program lean4-elan-name)
                             nil t nil arguments)
                    (error 'not-found))))
      (cons status (buffer-string)))))

(defun lean4-toolchain--elan-in (toolchain &rest arguments)
  "Run elan with ARGUMENTS, resolving versions as TOOLCHAIN would.
Passed through `ELAN_TOOLCHAIN', which is how VS Code asks what a
release channel currently resolves to."
  (let ((process-environment
         (cons (format "ELAN_TOOLCHAIN=%s" toolchain) process-environment)))
    (apply #'lean4-toolchain--elan arguments)))

(defun lean4-toolchain--elan-major-version ()
  "Return elan\\='s major version, or nil if elan could not be run."
  (pcase-let ((`(,status . ,output) (lean4-toolchain--elan "--version")))
    (when (eq status 0)
      (when (string-match (rx "elan" (+ space) (group (+ digit))) output)
        (string-to-number (match-string 1 output))))))

(defun lean4-toolchain--require-elan ()
  "Signal unless elan is installed and new enough to be asked these things."
  (let ((major (lean4-toolchain--elan-major-version)))
    (cond
     ((null major)
      (user-error "Elan is not installed, or could not be run.  \
See https://lean-lang.org/lean4/doc/setup.html"))
     ((< major lean4-toolchain--elan-minimum)
      (user-error
       "These commands need elan %d or later; this is elan %d.  \
Update it with `elan self update'"
       lean4-toolchain--elan-minimum major)))))

(defun lean4-toolchain--state (&optional toolchain)
  "Return elan\\='s state, parsed, resolving versions as TOOLCHAIN would.
Signals if elan cannot be run or its answer cannot be read."
  (pcase-let ((`(,status . ,output)
               (if toolchain
                   (lean4-toolchain--elan-in toolchain "dump-state")
                 (lean4-toolchain--elan "dump-state" "--no-net"))))
    (unless (eq status 0)
      (user-error "Could not ask elan what it has: %s" (string-trim output)))
    (condition-case nil
        (lean4--json-parse-string output)
      (error (user-error "Could not read what elan said")))))

(defun lean4-toolchain-installed ()
  "Return the versions elan has installed, newest first."
  (let* ((state (lean4-toolchain--state))
         (installed (plist-get (plist-get state :toolchains) :installed)))
    (lean4-toolchain--sort
     (delq nil (mapcar (lambda (entry) (plist-get entry :resolved_name))
                       installed)))))

;;;; Ordering

(defconst lean4-toolchain--kinds '(unknown release nightly pr-release)
  "The kinds of Lean version, in the order VS Code lists them.
Anything unrecognised first, then releases, then nightlies, then the
builds made for a pull request.")

(defun lean4-toolchain--parse (toolchain)
  "Return TOOLCHAIN\\='s kind and the key to order it within that kind.

A release\\='s key is a version list, so that v4.10.0 comes above v4.9.0 --
and so that v4.26.0 comes above v4.26.0-rc1, a release candidate being
older than the release it led to.  `version-to-list' reads \"-rc1\" as
exactly that, which is what semantic versioning says and what VS Code
gets from its own semver library."
  (cond
   ((string-match (rx bos "leanprover/lean4:" (? "v") (group (+ nonl)) eos)
                  toolchain)
    (if-let* ((version (ignore-errors
                         (version-to-list (match-string 1 toolchain)))))
        (list 'release version)
      ;; "stable" and "nightly" name channels rather than versions.
      (list 'unknown toolchain)))
   ((string-match (rx bos "leanprover/lean4-nightly:nightly-"
                      (group (+ nonl)) eos)
                  toolchain)
    ;; An ISO date compares correctly as a string.
    (list 'nightly (match-string 1 toolchain)))
   ((string-match (rx bos "leanprover/lean4-pr-releases:pr-release-"
                      (group (+ digit)) eos)
                  toolchain)
    (list 'pr-release (string-to-number (match-string 1 toolchain))))
   (t (list 'unknown toolchain))))

(defun lean4-toolchain--earlier-in-list-p (a b)
  "Return non-nil if A belongs before B in the list of versions."
  (pcase-let* ((`(,kind-a ,key-a) (lean4-toolchain--parse a))
               (`(,kind-b ,key-b) (lean4-toolchain--parse b))
               (rank-a (seq-position lean4-toolchain--kinds kind-a))
               (rank-b (seq-position lean4-toolchain--kinds kind-b)))
    (if (/= rank-a rank-b)
        (< rank-a rank-b)
      (pcase kind-a
        ('release (version-list-< key-b key-a))
        ('nightly (string> key-a key-b))
        ('pr-release (> key-a key-b))
        ;; Unrecognised names go alphabetically, having no order of their own.
        (_ (string< key-a key-b))))))

(defun lean4-toolchain--sort (toolchains)
  "Return TOOLCHAINS newest first, as VS Code lists them."
  (sort (copy-sequence toolchains) #'lean4-toolchain--earlier-in-list-p))

;;;; What could be chosen

(defun lean4-toolchain--releases ()
  "Return the released versions, as a plist of channel to list of names.
Nil, with a warning, where the release list could not be fetched: the
installed versions are still worth offering."
  (condition-case error
      (with-current-buffer
          (let ((url-request-method "GET"))
            (url-retrieve-synchronously lean4-toolchain-releases-url
                                        'silent 'inhibit-cookies 30))
        (unwind-protect
            (let ((json (lean4--json-parse-http-body)))
              (list :stable (lean4-toolchain--release-names json :stable)
                    :beta (lean4-toolchain--release-names json :beta)
                    :nightly (lean4-toolchain--release-names json :nightly)))
          (kill-buffer (current-buffer))))
    (error (message "Could not fetch the list of Lean versions: %s"
                    (error-message-string error))
           nil)))

(defun lean4-toolchain--release-names (json channel)
  "Return the toolchain names of JSON\\='s CHANNEL."
  (mapcar (lambda (release)
            (concat "leanprover/lean4:" (plist-get release :name)))
          (plist-get json channel)))

(defun lean4-toolchain--candidates (&optional include-stable)
  "Return the versions to offer, and how to annotate each.

Returns a cons of the names, in the order to show them, and an alist of
name to annotation.  Installed versions come first, then the released
ones that are not installed -- stable, then beta, then nightly, as VS
Code groups them.  With INCLUDE-STABLE, the channel that always means the
newest stable version leads, which is offered for the default version but
not for a project\\='s."
  (let* ((installed (lean4-toolchain-installed))
         (known (make-hash-table :test #'equal))
         (releases (lean4-toolchain--releases))
         (names nil)
         (annotations nil))
    (dolist (toolchain installed) (puthash toolchain t known))
    (when include-stable
      (push lean4-toolchain-stable names)
      (push (cons lean4-toolchain-stable " always the newest stable version")
            annotations))
    (dolist (toolchain installed)
      (push toolchain names)
      (push (cons toolchain " (installed)") annotations))
    (dolist (channel '(:stable :beta :nightly))
      (dolist (toolchain (plist-get releases channel))
        (unless (gethash toolchain known)
          (puthash toolchain t known)
          (push toolchain names)
          (push (cons toolchain " (not installed)") annotations))))
    (cons (nreverse names) (nreverse annotations))))

(defun lean4-toolchain--read (prompt &optional include-stable)
  "Read a Lean version with PROMPT, offering INCLUDE-STABLE as well.
The order is the one VS Code shows rather than alphabetical, so the
metadata says not to sort."
  (pcase-let* ((`(,names . ,annotations)
                (lean4-toolchain--candidates include-stable))
               (table
                (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata
                        (annotation-function
                         . ,(lambda (name) (cdr (assoc name annotations))))
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
                    (complete-with-action action names string predicate)))))
    (unless names
      (user-error "Elan has no Lean versions installed, and none could be \
fetched"))
    (completing-read prompt table nil t)))

;;;; Choosing

(defun lean4-toolchain--workspace-root-or-lose ()
  "Return the workspace root of the current buffer, or signal."
  (or (lean4--workspace-root)
      (user-error "This file is not in a Lean project.  \
Visit a file of the project whose Lean version you want to set")))

;;;###autoload
(defun lean4-select-project-toolchain (toolchain)
  "Set the Lean version of this project to TOOLCHAIN.

Writes \"lean-toolchain\" in the workspace root and restarts the server.

Asks first, and the warning is VS Code\\='s: this is meant for maintainers
of a project.  Changing a project\\='s Lean version can break it, and not
only its own code -- its \"lakefile.toml\" or \"lakefile.lean\" and its Lake
dependencies may all turn out to be incompatible with the new version.  To
take a dependency\\='s version instead, in order to stay compatible with
it, `lean4-lake-update-dependency' is the command to reach for.

The file is under version control in most projects, so this shows up as a
change to be committed or reverted."
  (interactive
   (progn (lean4-toolchain--require-elan)
          ;; The root is settled before the version is read: there is no point
          ;; asking which version to write if there is nowhere to write it.
          (lean4-toolchain--workspace-root-or-lose)
          (list (lean4-toolchain--read "Lean version for this project: "))))
  (lean4-toolchain--require-elan)
  (let* ((root (lean4-toolchain--workspace-root-or-lose))
         (file (expand-file-name "lean-toolchain" root)))
    (unless (yes-or-no-p
             (format "Set the Lean version of %s to %s?  \
This can break the project, its lakefile and its dependencies.  Proceed? "
                     root toolchain))
      (user-error "Not changing the Lean version of %s" root))
    (write-region (concat toolchain "\n") nil file nil 'silent)
    (message "%s now uses %s" root toolchain)
    (when (and (eglot-current-server)
               (yes-or-no-p "Restart the Lean server for the new version? "))
      (eglot-reconnect (eglot-current-server) 'interactive))))

;;;###autoload
(defun lean4-select-default-toolchain (toolchain)
  "Make TOOLCHAIN the Lean version used outside any project.

Runs `elan default'.  This is what serves a loose .lean file, and what
`lean' and `lake' use on the command line outside a project; it is not
what a project uses, which its own \"lean-toolchain\" decides.

Choosing the stable channel rather than a version means the newest stable
version at any time, which elan will fetch as new ones appear."
  (interactive
   (progn (lean4-toolchain--require-elan)
          (list (lean4-toolchain--read "Default Lean version: " 'include-stable))))
  (lean4-toolchain--require-elan)
  (unless (yes-or-no-p (format "Make %s the Lean version used outside any \
project? " toolchain))
    (user-error "Not changing the default Lean version"))
  (pcase-let ((`(,status . ,output)
               (lean4-toolchain--elan "default" toolchain)))
    (if (eq status 0)
        (message "The default Lean version is now %s" toolchain)
      (user-error "Could not set the default Lean version: %s"
                  (string-trim output)))))

;;;; Updating a release channel

(defun lean4-toolchain--channel-state (channel)
  "Return what CHANNEL resolves to now and what it is cached at.
A cons of the live version and the cached one, either of which may be
nil.  Where they differ, the channel has moved on and installing it would
fetch a newer Lean."
  (let* ((state (lean4-toolchain--state channel))
         (active (plist-get (plist-get state :toolchains) :resolved_active))
         (live (plist-get (plist-get active :live) :Ok)))
    (cons live (plist-get active :cached))))

(defun lean4-toolchain--outdated-channels ()
  "Return the release channels that have moved on, and by how much.
An alist of channel to a description of the change."
  (delq nil
        (mapcar
         (lambda (channel)
           (pcase-let ((`(,live . ,cached)
                        (lean4-toolchain--channel-state channel)))
             (when (and live (not (equal live cached)))
               (cons channel
                     (format "%s ⟹ %s" (or cached "not installed") live)))))
         (list lean4-toolchain-stable lean4-toolchain-nightly))))

;;;###autoload
(defun lean4-update-release-channel ()
  "Fetch the newest Lean of a release channel that has moved on.

Only the channels actually behind are offered, each showing what it is
installed at and what it would become; where none are, this says so.
Runs `elan toolchain install', as VS Code does.  Needs the network, to
ask what each channel now resolves to."
  (interactive)
  (lean4-toolchain--require-elan)
  (message "Asking elan what the release channels resolve to...")
  (let ((outdated (lean4-toolchain--outdated-channels)))
    (unless outdated
      (user-error "Every release channel is already at its newest version"))
    (let* ((annotations outdated)
           (table
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (annotation-function
                     . ,(lambda (name)
                          (concat "  " (cdr (assoc name annotations)))))
                    (display-sort-function . identity))
                (complete-with-action action (mapcar #'car outdated)
                                      string predicate))))
           (channel (completing-read "Update which release channel: "
                                     table nil t)))
      (pcase-let ((`(,status . ,output)
                   (lean4-toolchain--elan "toolchain" "install" channel)))
        (if (eq status 0)
            (message "%s is now at its newest version" channel)
          (user-error "Could not update %s: %s" channel
                      (string-trim output)))))))

;;;; Installing a Lean version

;;;###autoload
(defun lean4-install-toolchain (toolchain)
  "Download and install TOOLCHAIN, without making it the default.

Runs `elan toolchain install'.  Choosing a version that is not installed
in any of the other commands fetches it as a side effect; this is for
fetching one ahead of needing it."
  (interactive
   (progn (lean4-toolchain--require-elan)
          (list (lean4-toolchain--read "Install Lean version: "))))
  (lean4-toolchain--require-elan)
  (message "Installing %s..." toolchain)
  (pcase-let ((`(,status . ,output)
               (lean4-toolchain--elan "toolchain" "install" toolchain)))
    (if (eq status 0)
        (message "Installed %s" toolchain)
      (user-error "Could not install %s: %s" toolchain (string-trim output)))))

;;;; Elan itself

;; VS Code installs elan by fetching a script from elan.lean-lang.org and
;; running it -- which is what Lean's own setup guide tells anyone to do, and
;; the only way onto a machine with no package manager for it.  The command is
;; shown in full first: "an editor downloaded and ran a shell script" is worth
;; being asked about rather than told about afterwards.

(defconst lean4-elan-unix-installer
  (concat "curl \"https://elan.lean-lang.org/elan-init.sh\" -sSf"
          " | sh -s -- -y --default-toolchain " lean4-toolchain-stable)
  "How to install elan anywhere with a POSIX shell.
Taken from VS Code, which takes it from Lean\\='s setup guide.")

(defconst lean4-elan-windows-installer
  (concat
   "$installCode = (Invoke-WebRequest"
   " -Uri \"https://elan.lean-lang.org/elan-init.ps1\""
   " -UseBasicParsing -ErrorAction Stop).Content; "
   "$installer = [ScriptBlock]::Create("
   "[System.Text.Encoding]::UTF8.GetString($installCode)); "
   "Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process; "
   "& $installer -NoPrompt 1 -DefaultToolchain " lean4-toolchain-stable)
  "How to install elan on Windows, as PowerShell.
The counterpart of VS Code\\='s own script.")

(defun lean4-elan--installer ()
  "Return the command that installs elan here, and the shell to run it in."
  (if (memq system-type '(windows-nt ms-dos))
      (cons lean4-elan-windows-installer "powershell")
    (cons lean4-elan-unix-installer shell-file-name)))

;;;###autoload
(defun lean4-install-elan ()
  "Install elan, which installs and selects Lean versions.

Fetches and runs the installer from elan.lean-lang.org, which is what VS
Code does and what Lean\\='s setup guide describes.  The exact command is
shown first, since this downloads a script and runs it.

The output goes to a compilation buffer.  Emacs will not find the new
elan on the variable `exec-path' until the environment it was started with is
refreshed, which in practice means restarting Emacs."
  (interactive)
  (when (lean4-toolchain--elan-major-version)
    (user-error "Elan is already installed; `lean4-update-elan' updates it"))
  (pcase-let ((`(,command . ,shell) (lean4-elan--installer)))
    (unless (yes-or-no-p
             (format "Run this to install elan?\n  %s\nProceed? " command))
      (user-error "Not installing elan"))
    (let ((shell-file-name shell)
          (compilation-buffer-name-function (lambda (&rest _) "*elan*")))
      (compile command))))

;;;###autoload
(defun lean4-update-elan ()
  "Update elan itself, with `elan self update'.
This updates the tool that manages Lean versions, not any Lean version;
`lean4-update-release-channel' does that."
  (interactive)
  (lean4-toolchain--require-elan)
  (unless (yes-or-no-p "Update elan itself? ")
    (user-error "Not updating elan"))
  (message "Updating elan...")
  (pcase-let ((`(,status . ,output) (lean4-toolchain--elan "self" "update")))
    (if (eq status 0)
        (message "%s" (string-trim output))
      (user-error "Could not update elan: %s" (string-trim output)))))

;;;###autoload
(defun lean4-uninstall-elan ()
  "Remove elan, and with it every Lean version it installed.

`elan self uninstall' takes the toolchains with it, so every project on
this machine loses the Lean it was using and both Lean and Lake leave the
PATH.  Asked twice, there being no undoing it short of installing elan
again and fetching every version afresh."
  (interactive)
  (lean4-toolchain--require-elan)
  (let ((installed (length (ignore-errors (lean4-toolchain-installed)))))
    (unless (yes-or-no-p
             (format "Remove elan and all %d Lean version%s it installed? "
                     installed (if (= installed 1) "" "s")))
      (user-error "Not removing elan"))
    (unless (yes-or-no-p "This cannot be undone.  Really remove elan? ")
      (user-error "Not removing elan")))
  (pcase-let ((`(,status . ,output)
               (lean4-toolchain--elan "self" "uninstall" "-y")))
    (if (eq status 0)
        (message "Elan removed")
      (user-error "Could not remove elan: %s" (string-trim output)))))

;;;; What Lean's setup needs

;; Elan fetches toolchains over the network and Lake clones dependencies with
;; Git, so both have to exist before either works.  VS Code offers to install
;; them, differently on every system, and offers in the same breath to hand the
;; command over instead of running it.  Both are offered here for the same
;; reason: installing system packages wants privilege, and being shown the
;; command beats being told about it afterwards.

(defconst lean4-dependency-programs '("git" "curl")
  "The programs Lean\\='s setup needs besides elan.
Git for Lake\\='s dependencies, curl for what elan downloads.  VS Code
checks for the same two.")

(defun lean4-missing-dependencies ()
  "Return the members of `lean4-dependency-programs' that are not installed."
  (seq-remove #'executable-find lean4-dependency-programs))

(defconst lean4-dependency--macos-script
  (concat
   "set -e\n"
   "touch \"/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress\"\n"
   "aclt_label=\"$(/usr/sbin/softwareupdate -l |"
   " grep -B 1 -E 'Command Line Tools' |"
   " awk -F'*' '/^ *\\*/ {print $2}' |"
   " sed -e 's/^ *Label: //' -e 's/^ *//' |"
   " sort -V | tail -n1 | tr -d '\\n')\"\n"
   "/usr/sbin/softwareupdate -i \"$aclt_label\"")
  "How macOS installs Git: Apple\\='s Command Line Tools bring it.
VS Code\\='s script, which asks `softwareupdate' what the current label for
them is rather than assuming one.")

(defun lean4-dependency-installation (&optional missing)
  "Return how to install MISSING here, defaulting to what is missing.

A plist.  `:script' is the command to run, or nil where nothing is known
for this system; `:manual' is the command to run in a terminal instead,
which on Linux is the ordinary `sudo' one; `:note' says anything else the
reader needs; `:shell' is the shell the script wants."
  (let* ((missing (or missing (lean4-missing-dependencies)))
         (packages (string-join missing " ")))
    (cond
     ((memq system-type '(windows-nt ms-dos))
      (if (executable-find "winget")
          (list :script (concat "winget install -e --id Git.Git"
                                " --source winget --silent"
                                " --accept-package-agreements"
                                " --accept-source-agreements"
                                " --disable-interactivity")
                :shell "powershell"
                :manual "winget install -e --id Git.Git")
        ;; VS Code downloads a pinned Git installer here.  A version pinned
        ;; in this file would go stale between releases, and an editor
        ;; fetching and silently running an installer is worth less than a
        ;; sentence pointing at the one place that is always current.
        (list :note "Install Git from https://git-scm.com/download/win")))
     ((eq system-type 'darwin)
      (list :script lean4-dependency--macos-script
            :shell shell-file-name
            :note (concat "Git comes with Apple's Command Line Tools;"
                          " `xcode-select --install' installs them too")))
     ((executable-find "apt-get")
      (list :script (and (executable-find "pkexec")
                         (concat "ulimit -Sn 1024; pkexec bash -c '"
                                 "export DEBIAN_FRONTEND=noninteractive; "
                                 "apt-get update -y && apt-get install -y "
                                 packages "'"))
            :shell shell-file-name
            :manual (format "sudo apt update && sudo apt install %s" packages)))
     ((executable-find "dnf")
      (list :script (and (executable-find "pkexec")
                         (format "pkexec dnf install -y %s" packages))
            :shell shell-file-name
            :manual (format "sudo dnf install %s" packages)))
     (t (list :note (format "Install %s with this system's package manager"
                            (string-join missing " and ")))))))

;;;###autoload
(defun lean4-install-dependencies ()
  "Install the programs Lean\\='s setup needs: Git and curl.

Git is what Lake clones dependencies with and curl is what elan downloads
through, so neither elan nor Lake works without them.  VS Code checks for
the same two.

Offers to run the command or to put it in the kill ring, as VS Code offers
to run it or copy it: this installs system packages, which wants
privilege, so handing the command over to a terminal is a reasonable
answer.  The command is shown either way.

On Linux the command goes through `pkexec' where that exists, which is
what asks for the password; the `sudo' form is what gets copied.  Where
nothing is known for this system, this says what to install and leaves it
at that.

Emacs will not find a newly installed program until the environment it was
started with is refreshed, which in practice means restarting Emacs."
  (interactive)
  (let ((missing (lean4-missing-dependencies)))
    (unless missing
      (user-error "%s are both installed already"
                  (string-join lean4-dependency-programs " and ")))
    (let* ((plan (lean4-dependency-installation missing))
           (script (plist-get plan :script))
           (manual (plist-get plan :manual))
           (note (plist-get plan :note))
           (copyable (or manual script)))
      (when note (message "%s" note))
      (unless copyable
        (user-error "%s missing.  %s"
                    (string-join missing " and ") (or note "")))
      (pcase (car (read-multiple-choice
                   (format "%s missing. " (string-join missing " and "))
                   (append (when script '((?r "run it")))
                           '((?c "copy the command"))
                           '((?q "do nothing")))))
        (?r (let ((shell-file-name (or (plist-get plan :shell) shell-file-name))
                  (compilation-buffer-name-function
                   (lambda (&rest _) "*lean4 dependencies*")))
              (message "Running: %s" script)
              (compile script)))
        (?c (kill-new copyable)
            (message "Copied: %s" copyable))
        (_ (message "Nothing done; %s still missing"
                    (string-join missing " and ")))))))

;;;; Uninstalling

(defun lean4-toolchain--gc ()
  "Return which installed versions are unused, and who uses the rest.
A cons of the list of unused versions and an alist of version to the
projects using it, as `elan toolchain gc' reports them."
  (pcase-let ((`(,status . ,output)
               (lean4-toolchain--elan "toolchain" "gc" "--json")))
    (if (not (eq status 0))
        (cons nil nil)
      (condition-case nil
          (let* ((json (lean4--json-parse-string output))
                 (used nil))
            (dolist (entry (plist-get json :used_toolchains))
              (let* ((toolchain (plist-get entry :toolchain))
                     (cell (assoc toolchain used)))
                (if cell
                    (setcdr cell (cons (plist-get entry :user) (cdr cell)))
                  (push (cons toolchain (list (plist-get entry :user))) used))))
            (cons (plist-get json :unused_toolchains) used))
        (error (cons nil nil))))))

;;;###autoload
(defun lean4-uninstall-toolchains (toolchains)
  "Remove TOOLCHAINS, a list of installed Lean versions.

Each is annotated with whether elan considers it unused, or which
projects still use it -- from `elan toolchain gc', which is where VS Code
gets the same annotation.  Removing one a project uses means elan will
fetch it again the next time that project is opened."
  (interactive
   (progn
     (lean4-toolchain--require-elan)
     (let* ((installed (lean4-toolchain-installed))
            (gc (lean4-toolchain--gc))
            (unused (car gc))
            (used (cdr gc))
            (annotations
             (mapcar (lambda (toolchain)
                       (cons toolchain
                             (cond
                              ((member toolchain unused) "  (unused)")
                              ((assoc toolchain used)
                               (format "  (used by %s)"
                                       (string-join
                                        (cdr (assoc toolchain used)) ", ")))
                              (t ""))))
                     installed))
            (table
             (lambda (string predicate action)
               (if (eq action 'metadata)
                   `(metadata
                     (annotation-function
                      . ,(lambda (name) (cdr (assoc name annotations))))
                     (display-sort-function . identity))
                 (complete-with-action action installed string predicate)))))
       (unless installed
         (user-error "Elan has no Lean versions installed"))
       (list (completing-read-multiple
              "Remove Lean versions (comma-separated): " table nil t)))))
  (lean4-toolchain--require-elan)
  (unless toolchains
    (user-error "No Lean version chosen"))
  (unless (yes-or-no-p (format "Remove %s? " (string-join toolchains ", ")))
    (user-error "Not removing anything"))
  (pcase-let ((`(,status . ,output)
               (apply #'lean4-toolchain--elan
                      "toolchain" "uninstall" toolchains)))
    (if (eq status 0)
        (message "Removed %s" (string-join toolchains ", "))
      (user-error "Could not remove: %s" (string-trim output)))))

(provide 'lean4-toolchain)
;;; lean4-toolchain.el ends here
