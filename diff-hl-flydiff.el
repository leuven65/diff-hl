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

(defun diff-hl-flydiff-changes-buffer (old-fun file backend &optional new-rev buf-base-name)
  (let ((diff-buf (diff-hl-generate-new-buffer (or buf-base-name " *diff-hl-flydiff*") t)))
    (if new-rev
        (funcall old-fun file backend new-rev diff-buf)
      (diff-hl-diff-buffer-with-reference file diff-buf backend))))

(defun diff-hl-flydiff-update ()
  (unless (or
           (not diff-hl-mode)
           (not buffer-file-name)
           (file-remote-p default-directory)
           (not (file-exists-p buffer-file-name)))
    (diff-hl-update)))

(defvar diff-hl-flydiff-timer nil)

(defun diff-hl-flydiff-update-on-after-change (_beg _end _len)
  (if (timerp diff-hl-flydiff-timer)
      (timer-set-idle-time diff-hl-flydiff-timer diff-hl-flydiff-delay)

    (setq diff-hl-flydiff-timer
          (run-with-idle-timer
           diff-hl-flydiff-delay nil
           (lambda (buf)
             (cancel-timer diff-hl-flydiff-timer)
             (setq diff-hl-flydiff-timer nil)
             (with-current-buffer buf
               (diff-hl-flydiff-update)))
           (current-buffer)))))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works."
  :lighter "" :global t

  (if diff-hl-flydiff-mode
      (progn
        (advice-add 'diff-hl-overlay-modified :override #'ignore)

        (advice-add 'diff-hl-changes-buffer :around
                    #'diff-hl-flydiff-changes-buffer)

        (add-hook 'after-change-functions #'diff-hl-flydiff-update-on-after-change nil t)        
        )

    (advice-remove 'diff-hl-overlay-modified #'ignore)

    (advice-remove 'diff-hl-changes-buffer #'diff-hl-flydiff-changes-buffer)
    
    (remove-hook 'after-change-functions #'diff-hl-flydiff-update-on-after-change t)

    (when (timerp diff-hl-flydiff-timer)
      (cancel-timer diff-hl-flydiff-timer)
      (setq diff-hl-flydiff-timer nil))
    ))

(provide 'diff-hl-flydiff)
