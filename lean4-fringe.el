;;; lean4-fringe.el --- Lean4-Mode Processing Progress in Fringe  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2026 Lean4-Mode contributors

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

;; Show Lean processing progress in the editor fringe, the counterpart of the
;; orange gutter bar in VS Code.
;;
;; Lean elaborates a file incrementally and reports which ranges are still in
;; flight with `$/lean/fileProgress'.  `lean4-eglot' turns that notification
;; into a call of `lean4-file-progress-functions'; this library subscribes and
;; maintains one overlay per outstanding range.

;;; Code:

(require 'seq)

(require 'lean4-settings)
(require 'lean4-eglot)

(defface lean4-fringe-face
  nil
  "Face to highlight Lean file progress."
  :group 'lean4)

(if (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'lean4-fringe-fringe-bitmap
      (vector) 16 8))

(defface lean4-fringe-fringe-processing-face
  '((((class color) (background light))
     :background "chocolate1")
    (((class color) (background dark))
     :background "navajo white")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file processing progress."
  :group 'lean4)

(defface lean4-fringe-fringe-fatal-error-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file fatal errors."
  :group 'lean4)

(defconst lean4-fringe--processing-kind 1
  "Value of `LeanFileProgressKind.processing' on the wire.
Any other value means the file worker hit a fatal error.")

(defun lean4-fringe-face (item)
  "Return the fringe face for progress ITEM."
  (if (eq (plist-get item :kind) lean4-fringe--processing-kind)
      'lean4-fringe-fringe-processing-face
    'lean4-fringe-fringe-fatal-error-face))

(defvar-local lean4-fringe-data nil
  "Ranges the server is still elaborating in this buffer.")

(defvar-local lean4-fringe-delay-timer nil
  "Timer coalescing progress redraws in this buffer.")

(defconst lean4-fringe-delay 0.3
  "Seconds to wait before redrawing progress overlays.
Progress notifications arrive continuously while a file elaborates;
redrawing on each one costs more than it communicates.")

(defun lean4-fringe-update-progress-overlays ()
  "Update processing bars in the current buffer."
  (setq lean4-fringe-delay-timer nil)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get ov 'face) 'lean4-fringe-face)
      (delete-overlay ov)))
  (when lean4-show-file-progress
    (seq-doseq (item lean4-fringe-data)
      (when-let* ((region (ignore-errors
                            (lean4--range-region (plist-get item :range))))
                  (ov (make-overlay (car region) (cdr region))))
        (overlay-put ov 'face 'lean4-fringe-face)
        (overlay-put ov 'line-prefix
                     (propertize " " 'display
                                 `(left-fringe lean4-fringe-fringe-bitmap
                                               ,(lean4-fringe-face item))))
        (overlay-put ov 'help-echo "Lean is processing this region")))))

(defun lean4-fringe-update (server uri processing)
  "Record SERVER's PROCESSING ranges for the buffer visiting URI.
Suitable for `lean4-file-progress-functions'."
  (lean4-with-uri-buffers server uri
    (setq lean4-fringe-data processing)
    (unless lean4-fringe-delay-timer
      (setq lean4-fringe-delay-timer
            (run-with-timer
             lean4-fringe-delay nil
             (lambda (buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (lean4-fringe-update-progress-overlays))))
             (current-buffer))))))

(add-hook 'lean4-file-progress-functions #'lean4-fringe-update)

(provide 'lean4-fringe)
;;; lean4-fringe.el ends here
