;;; lean4-settings.el --- Lean4-Mode User-Options  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;; Commentary:

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defgroup lean4 nil
  "Major mode for Lean4 programming language and theorem prover."
  :group 'languages
  :link '(info-link :tag "Info Manual" "(lean4-mode)")
  :link '(url-link
          :tag "Website"
          "https://github.com/d-torrance/lean4-mode")
  :link '(emacs-library-link :tag "Library Source" "lean4-mode.el")
  :prefix "lean4-")

(defconst lean4-default-executable-name
  (cl-case system-type
    (windows-nt "lean.exe")
    (t          "lean"))
  "Default executable name of Lean.")

(defconst lean4-default-lake-name
  (cl-case system-type
    (windows-nt "lake.exe")
    (t          "lake"))
  "Default executable name of Lake.")

(defcustom lean4-auto-start-server t
  "Whether to start the Lean language server on visiting a Lean file.
Set to nil to start it by hand with \\[eglot]."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-rootdir nil
  "Directory holding the Lean toolchain.
Only needed when Lean is not found through the variable `exec-path'.
Leave this nil when Lean is installed with elan, which is the usual
case: elan's shims select the toolchain a project pins, and naming a
directory here bypasses that."
  :group 'lean4
  :type '(choice (const :tag "Find on `exec-path'" nil) directory))

(defcustom lean4-executable-name lean4-default-executable-name
  "Name of lean executable."
  :group 'lean4
  :type 'string)

(defcustom lean4-lake-name lean4-default-lake-name
  "Name of lake executable."
  :group 'lean4
  :type 'string)

(defcustom lean4-delete-trailing-whitespace nil
  "Automatically delete trailing whitespace.
Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-read-only-dependencies t
  "Whether a Lean file belonging to a dependency opens read-only.
That is one under a `.lake' directory -- a package Lake fetched -- or
under a `.elan' one, which is Lean\='s own source.  Jumping into either
with \[xref-find-definitions] is the ordinary way to read a proof; editing
what one finds there is not, the file being rebuilt from upstream and
overwritten by the next build or cache fetch, and the diagnostics for it
being about what was built rather than about what is on screen.

VS Code says the same thing with `files.readonlyInclude', which it sets
for both directories.  Read-only rather than not shown: reading them is
the point.

The buffer is merely marked read-only, so \[read-only-mode] gets you in
if you mean it."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.
Set this variable to t to highlight inaccessible names in the info display
using `font-lock-comment-face' instead of the `✝` suffix used by Lean."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-show-file-progress t
  "Highlight file progress in the current buffer."
  :group 'lean4
  :type 'boolean)

(provide 'lean4-settings)
;;; lean4-settings.el ends here
