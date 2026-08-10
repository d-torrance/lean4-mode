;;; lean4-toolchain-test.el --- Tests for choosing a Lean version  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Doug Torrance

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Elan is stubbed throughout: what is being checked is how its answers are
;; read and ordered, and nothing here installs or removes anything.  The JSON
;; below is what elan 4.2.2 actually printed, trimmed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-toolchain)

;;;; Ordering

(ert-deftest lean4-toolchain-parses-each-kind-of-version ()
  "A release, a nightly, a pull-request build, and something unrecognised."
  (should (equal (car (lean4-toolchain--parse "leanprover/lean4:v4.26.0"))
                 'release))
  (should (equal (car (lean4-toolchain--parse
                       "leanprover/lean4-nightly:nightly-2026-08-10"))
                 'nightly))
  (should (equal (car (lean4-toolchain--parse
                       "leanprover/lean4-pr-releases:pr-release-42"))
                 'pr-release))
  (should (equal (car (lean4-toolchain--parse "my-own-build")) 'unknown))
  ;; A channel names no particular version, so it is not a release.
  (should (equal (car (lean4-toolchain--parse "leanprover/lean4:stable"))
                 'unknown)))

(ert-deftest lean4-toolchain-orders-versions-numerically ()
  "v4.10.0 is newer than v4.9.0, whatever a string comparison thinks."
  (should (equal (lean4-toolchain--sort
                  '("leanprover/lean4:v4.9.0" "leanprover/lean4:v4.10.0"))
                 '("leanprover/lean4:v4.10.0" "leanprover/lean4:v4.9.0"))))

(ert-deftest lean4-toolchain-puts-a-release-above-its-candidate ()
  "v4.26.0 is newer than v4.26.0-rc1.

The release candidate led to the release, so it is older -- which is what
semantic versioning says, what VS Code gets from its semver library, and
what a naive comparison of the digits gets backwards."
  (should (equal (lean4-toolchain--sort
                  '("leanprover/lean4:v4.26.0-rc1" "leanprover/lean4:v4.26.0"))
                 '("leanprover/lean4:v4.26.0"
                   "leanprover/lean4:v4.26.0-rc1"))))

(ert-deftest lean4-toolchain-groups-kinds-as-vs-code-does ()
  "Unrecognised first, then releases, then nightlies, then PR builds."
  (should (equal (lean4-toolchain--sort
                  '("leanprover/lean4-pr-releases:pr-release-9"
                    "leanprover/lean4-nightly:nightly-2026-08-09"
                    "leanprover/lean4:v4.9.0"
                    "my-own-build"
                    "leanprover/lean4-nightly:nightly-2026-08-10"
                    "leanprover/lean4-pr-releases:pr-release-10"))
                 '("my-own-build"
                   "leanprover/lean4:v4.9.0"
                   "leanprover/lean4-nightly:nightly-2026-08-10"
                   "leanprover/lean4-nightly:nightly-2026-08-09"
                   "leanprover/lean4-pr-releases:pr-release-10"
                   "leanprover/lean4-pr-releases:pr-release-9"))))

;;;; Elan's answers

(defconst lean4-toolchain-test--state
  "{\"elan_version\": {\"current\": \"4.2.2\"},
    \"toolchains\": {
      \"installed\": [{\"resolved_name\": \"leanprover/lean4:v4.20.1\"},
                    {\"resolved_name\": \"leanprover/lean4:v4.32.2\"}],
      \"resolved_active\": {\"live\": {\"Ok\": \"leanprover/lean4:v4.33.0\"},
                          \"cached\": \"leanprover/lean4:v4.32.2\"}}}"
  "What `elan dump-state' says, trimmed to what is read.")

(defmacro lean4-toolchain-test--with-elan (spec &rest body)
  "Stub elan with SPEC and evaluate BODY.
SPEC is a function of the argument list, returning the cons that
`lean4-toolchain--elan' would."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'lean4-toolchain--elan)
              (lambda (&rest arguments) (funcall ,spec arguments))))
     ,@body))

(ert-deftest lean4-toolchain-reads-the-elan-version ()
  "The major version is what decides whether these commands can run."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(0 . "elan 4.2.2 (35628cce1 2026-05-28)\n"))
    (should (equal (lean4-toolchain--elan-major-version) 4))
    (should-not (lean4-toolchain--require-elan)))
  ;; Elan absent altogether.
  (lean4-toolchain-test--with-elan
      (lambda (_) '(not-found . ""))
    (should-not (lean4-toolchain--elan-major-version))
    (should-error (lean4-toolchain--require-elan) :type 'user-error))
  ;; Too old for `dump-state', which VS Code refuses on as well.
  (lean4-toolchain-test--with-elan
      (lambda (_) '(0 . "elan 3.1.1\n"))
    (should (equal (lean4-toolchain--elan-major-version) 3))
    (should-error (lean4-toolchain--require-elan) :type 'user-error)))

(ert-deftest lean4-toolchain-lists-what-is-installed-newest-first ()
  "The installed versions come from `dump-state', sorted."
  (lean4-toolchain-test--with-elan
      (lambda (_) (cons 0 lean4-toolchain-test--state))
    (should (equal (lean4-toolchain-installed)
                   '("leanprover/lean4:v4.32.2" "leanprover/lean4:v4.20.1")))))

(ert-deftest lean4-toolchain-reports-a-broken-dump-state ()
  "Elan failing, or answering with something unreadable, is a user error."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(1 . "elan: no such subcommand"))
    (should-error (lean4-toolchain-installed) :type 'user-error))
  (lean4-toolchain-test--with-elan
      (lambda (_) '(0 . "this is not json"))
    (should-error (lean4-toolchain-installed) :type 'user-error)))

;;;; What is offered

(ert-deftest lean4-toolchain-offers-installed-and-downloadable ()
  "Installed versions lead, then released ones that are not installed.
Each is marked, as VS Code marks them, and nothing appears twice."
  (lean4-toolchain-test--with-elan
      (lambda (_) (cons 0 lean4-toolchain-test--state))
    (cl-letf (((symbol-function 'lean4-toolchain--releases)
               (lambda ()
                 ;; v4.32.2 is installed, so it must not be offered twice.
                 (list :stable '("leanprover/lean4:v4.33.0"
                                 "leanprover/lean4:v4.32.2")
                       :beta '("leanprover/lean4:v4.33.0-rc2")
                       :nightly '("leanprover/lean4-nightly:nightly-2026-08-10")))))
      (pcase-let ((`(,names . ,annotations)
                   (lean4-toolchain--candidates)))
        (should (equal names
                       '("leanprover/lean4:v4.32.2"
                         "leanprover/lean4:v4.20.1"
                         "leanprover/lean4:v4.33.0"
                         "leanprover/lean4:v4.33.0-rc2"
                         "leanprover/lean4-nightly:nightly-2026-08-10")))
        (should (equal (cdr (assoc "leanprover/lean4:v4.20.1" annotations))
                       " (installed)"))
        (should (equal (cdr (assoc "leanprover/lean4:v4.33.0" annotations))
                       " (not installed)"))))))

(ert-deftest lean4-toolchain-offers-the-stable-channel-only-when-asked ()
  "The default version may be a channel; a project's may not.
VS Code draws the same distinction: a project pins a version."
  (lean4-toolchain-test--with-elan
      (lambda (_) (cons 0 lean4-toolchain-test--state))
    (cl-letf (((symbol-function 'lean4-toolchain--releases) (lambda () nil)))
      (should-not (member lean4-toolchain-stable
                          (car (lean4-toolchain--candidates))))
      (should (equal (car (car (lean4-toolchain--candidates 'include-stable)))
                     lean4-toolchain-stable)))))

(ert-deftest lean4-toolchain-survives-an-unreachable-release-list ()
  "The installed versions are still worth offering without the network."
  (lean4-toolchain-test--with-elan
      (lambda (_) (cons 0 lean4-toolchain-test--state))
    (cl-letf (((symbol-function 'lean4-toolchain--releases) (lambda () nil)))
      (should (equal (car (lean4-toolchain--candidates))
                     '("leanprover/lean4:v4.32.2"
                       "leanprover/lean4:v4.20.1"))))))

;;;; Release channels

(ert-deftest lean4-toolchain-notices-a-channel-that-has-moved-on ()
  "A channel whose live version differs from its cached one is behind."
  (lean4-toolchain-test--with-elan
      (lambda (_) (cons 0 lean4-toolchain-test--state))
    (let ((outdated (lean4-toolchain--outdated-channels)))
      (should (assoc lean4-toolchain-stable outdated))
      (should (equal (cdr (assoc lean4-toolchain-stable outdated))
                     "leanprover/lean4:v4.32.2 ⟹ leanprover/lean4:v4.33.0")))))

(ert-deftest lean4-toolchain-notices-a-channel-that-is-current ()
  "Where live and cached agree there is nothing to fetch."
  (lean4-toolchain-test--with-elan
      (lambda (_)
        (cons 0 "{\"toolchains\": {\"installed\": [],
                   \"resolved_active\": {\"live\": {\"Ok\": \"leanprover/lean4:v4.33.0\"},
                                       \"cached\": \"leanprover/lean4:v4.33.0\"}}}"))
    (should-not (lean4-toolchain--outdated-channels))))

;;;; Removing

(ert-deftest lean4-toolchain-reads-what-is-unused ()
  "`elan toolchain gc' says which versions nothing uses, and who uses the rest."
  (lean4-toolchain-test--with-elan
      (lambda (_)
        (cons 0 "{\"unused_toolchains\": [\"leanprover/lean4:v4.22.0\"],
                  \"used_toolchains\": [
                    {\"user\": \"default toolchain\",
                     \"toolchain\": \"leanprover/lean4:v4.32.2\"},
                    {\"user\": \"/src/pkg\",
                     \"toolchain\": \"leanprover/lean4:v4.32.2\"}]}"))
    (pcase-let ((`(,unused . ,used) (lean4-toolchain--gc)))
      (should (equal unused '("leanprover/lean4:v4.22.0")))
      (should (equal (sort (cdr (assoc "leanprover/lean4:v4.32.2" used))
                           #'string<)
                     '("/src/pkg" "default toolchain"))))))

(ert-deftest lean4-toolchain-tolerates-an-elan-without-gc ()
  "An older elan has no `gc --json'; the versions are still listed."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(1 . "error: unrecognized subcommand"))
    (should (equal (lean4-toolchain--gc) '(nil)))))

(ert-deftest lean4-toolchain-uninstall-declines-nothing ()
  "Choosing no versions removes none."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(0 . "elan 4.2.2\n"))
    (should-error (lean4-uninstall-toolchains nil) :type 'user-error)))

;;;; Elan itself

(ert-deftest lean4-toolchain-installer-is-the-documented-one ()
  "The command is the one Lean's setup guide gives, as VS Code uses it."
  (should (string-search "elan.lean-lang.org/elan-init.sh"
                         lean4-elan-unix-installer))
  (should (string-search "--default-toolchain leanprover/lean4:stable"
                         lean4-elan-unix-installer))
  (should (string-search "elan-init.ps1" lean4-elan-windows-installer))
  ;; PowerShell where PowerShell is what there is.
  (let ((system-type 'windows-nt))
    (should (equal (cdr (lean4-elan--installer)) "powershell")))
  (let ((system-type 'gnu/linux))
    (should (equal (car (lean4-elan--installer)) lean4-elan-unix-installer))))

(ert-deftest lean4-toolchain-install-elan-declines-when-it-is-there ()
  "Installing over an existing elan is refused; updating is the command."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(0 . "elan 4.2.2\n"))
    (should-error (lean4-install-elan) :type 'user-error)))

(ert-deftest lean4-toolchain-install-elan-asks-before-running-anything ()
  "Declining runs nothing at all."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(not-found . ""))
    (let (compiled)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'compile)
                 (lambda (command &rest _) (setq compiled command))))
        (should-error (lean4-install-elan) :type 'user-error))
      (should-not compiled))))

(ert-deftest lean4-toolchain-install-elan-runs-the-shown-command ()
  "What was agreed to is what runs."
  (lean4-toolchain-test--with-elan
      (lambda (_) '(not-found . ""))
    (let (compiled shown)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt &rest _) (setq shown prompt) t))
                ((symbol-function 'compile)
                 (lambda (command &rest _) (setq compiled command))))
        (lean4-install-elan))
      (should (equal compiled (car (lean4-elan--installer))))
      ;; The command is in the question, not merely implied by it.
      (should (string-search (car (lean4-elan--installer)) shown)))))

(ert-deftest lean4-toolchain-update-elan-asks-first ()
  "Declining leaves elan alone."
  (let (calls)
    (lean4-toolchain-test--with-elan
        (lambda (arguments)
          (push arguments calls)
          (cons 0 (if (equal arguments '("--version")) "elan 4.2.2\n" "")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (lean4-update-elan) :type 'user-error))
      (should-not (member '("self" "update") calls)))))

(ert-deftest lean4-toolchain-update-elan-runs-self-update ()
  "Agreeing runs `elan self update' and nothing else."
  (let (calls)
    (lean4-toolchain-test--with-elan
        (lambda (arguments)
          (push arguments calls)
          (cons 0 (if (equal arguments '("--version"))
                      "elan 4.2.2\n"
                    "elan updated\n")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'message) #'ignore))
        (lean4-update-elan))
      (should (member '("self" "update") calls)))))

(ert-deftest lean4-toolchain-uninstall-elan-asks-twice ()
  "One `yes' is not enough to remove elan and every Lean with it."
  (let ((asked 0) calls)
    (lean4-toolchain-test--with-elan
        (lambda (arguments)
          (push arguments calls)
          (cons 0 (if (equal arguments '("--version"))
                      "elan 4.2.2\n"
                    lean4-toolchain-test--state)))
      ;; Yes to the first question, no to the second: nothing happens.
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf asked) (= asked 1))))
        (should-error (lean4-uninstall-elan) :type 'user-error))
      (should (= asked 2))
      (should-not (member '("self" "uninstall" "-y") calls)))))

(ert-deftest lean4-toolchain-uninstall-elan-runs-self-uninstall ()
  "Both answers yes runs it, with the flag that stops elan asking again."
  (let (calls)
    (lean4-toolchain-test--with-elan
        (lambda (arguments)
          (push arguments calls)
          (cons 0 (if (equal arguments '("--version"))
                      "elan 4.2.2\n"
                    lean4-toolchain-test--state)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'message) #'ignore))
        (lean4-uninstall-elan))
      (should (member '("self" "uninstall" "-y") calls)))))

(ert-deftest lean4-toolchain-install-toolchain-runs-elan-install ()
  "Installing a version ahead of needing it."
  (let (calls)
    (lean4-toolchain-test--with-elan
        (lambda (arguments)
          (push arguments calls)
          (cons 0 (if (equal arguments '("--version")) "elan 4.2.2\n" "")))
      (cl-letf (((symbol-function 'message) #'ignore))
        (lean4-install-toolchain "leanprover/lean4:v4.33.0"))
      (should (member '("toolchain" "install" "leanprover/lean4:v4.33.0")
                      calls)))))

(provide 'lean4-toolchain-test)
;;; lean4-toolchain-test.el ends here
