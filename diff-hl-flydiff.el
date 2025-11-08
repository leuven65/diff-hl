;; Copyright (C) 2015-2025 Free Software Foundation, Inc. -*- lexical-binding: t -*-

;; Author:   Jonathan Hayase <PythonNut@gmail.com>
;; URL:      https://github.com/dgutov/diff-hl

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode enables diffing on-the-fly (i.e. without saving the buffer first)
;; Toggle in all buffers with M-x diff-hl-flydiff-mode

;;; Code:

(require 'diff-hl)
(require 'diff)

(defgroup diff-hl-flydiff nil
  "Highlight changes on the fly"
  :group 'diff-hl)

(defcustom diff-hl-flydiff-delay 0.3
  "The idle delay in seconds before highlighting is updated."
  :type 'number)

(defvar diff-hl-flydiff-modified-tick nil)
(make-variable-buffer-local 'diff-hl-flydiff-modified-tick)

(defvar diff-hl-flydiff-timer nil)

(defun diff-hl-flydiff-changes-buffer (old-fun file backend &optional new-rev buf-base-name)
  (setq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick))
  (let ((diff-buf (diff-hl-generate-new-buffer (or buf-base-name " *diff-hl-flydiff*") t)))
    (if new-rev
        (funcall old-fun file backend new-rev diff-buf)
      (diff-hl-diff-buffer-with-reference file diff-buf backend))))

(defsubst diff-hl-flydiff-update-p ()
  (not (or
        (not diff-hl-mode)
        (eq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick))
        (not buffer-file-name)
        (file-remote-p default-directory)
        (not (file-exists-p buffer-file-name)))))

(defun diff-hl-flydiff-update ()
  (when (diff-hl-flydiff-update-p)
    (diff-hl-update)))

(defun diff-hl-flydiff-update-debounce ()
  (when (diff-hl-flydiff-update-p)
    (diff-hl-update-debounce)))

(defun diff-hl-flydiff-update-throttle ()
  (when (and (not diff-hl-update-throttle-timer)
             (diff-hl-flydiff-update-p))
    ;; async version
    ;; Add #'funcall as callback to ensure that errors are reported.
    (aio-listen (diff-hl-update-throttle-async) #'funcall)
    ;; sync version
    ;; (diff-hl-update-throttle)
    ))

(defun diff-hl-flydiff/modified-p (_state)
  (buffer-modified-p))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works."
  :lighter "" :global t
  (when (timerp diff-hl-flydiff-timer)
    (cancel-timer diff-hl-flydiff-timer)
    (setq diff-hl-flydiff-timer nil))

  (if diff-hl-flydiff-mode
      (progn
        (advice-add 'diff-hl-overlay-modified :override #'ignore)

        (advice-add 'diff-hl-modified-p :before-until
                    #'diff-hl-flydiff/modified-p)
        (advice-add 'diff-hl-changes-buffer :around
                    #'diff-hl-flydiff-changes-buffer)
        (setq diff-hl-flydiff-timer
              (run-with-idle-timer diff-hl-flydiff-delay t #'diff-hl-flydiff-update-throttle)))

    (advice-remove 'diff-hl-overlay-modified #'ignore)

    (advice-remove 'diff-hl-modified-p #'diff-hl-flydiff/modified-p)
    (advice-remove 'diff-hl-changes-buffer #'diff-hl-flydiff-changes-buffer)))

(provide 'diff-hl-flydiff)
