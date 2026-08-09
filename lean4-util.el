;;; lean4-util.el --- Lean4-Mode Utilities  -*- lexical-binding: t; -*-

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

;; This library provides utilities for `lean4-mode'.

;;; Code:

(require 'lean4-settings)

(defun lean4--program (name)
  "Return how to invoke the Lean toolchain program NAME.

Normally NAME is returned unchanged and resolved through the variable
`exec-path', which is what an elan installation wants: elan's shims read the
project's \"lean-toolchain\" and dispatch to the right version, so
pinning an absolute path defeats it.  `lean4-rootdir' overrides this for
installations that are not on the variable `exec-path'."
  (if lean4-rootdir
      (expand-file-name name (expand-file-name "bin" lean4-rootdir))
    name))

(defun lean4-whitespace-cleanup ()
  "Delete trailing whitespace if `lean4-delete-trailing-whitespace' is t."
  (when lean4-delete-trailing-whitespace
      (delete-trailing-whitespace)))

(provide 'lean4-util)
;;; lean4-util.el ends here
