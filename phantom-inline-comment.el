;;; phantom-inline-comment --- Visible but not affect the code

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513/phantom-inline-comment
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'phantom-inline-comment)
;;

;;; Code:

(require 'popwin)

(defvar phantom-inline-comment-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-c") 'phantom-inline-comment--apply-buffer)
    (define-key kmap (kbd "C-g") 'phantom-inline-comment--cancel)
    kmap))

(defvar phantom-inline-comment-edit-buffer "*phantom-inline-comment-edit*")

(defvar phantom-inline-comment-state nil) ;; 'add or 'edit

(defvar inline--phantom-comments nil)

(defface phantom-inline-commnet-face '((t (:inherit highlight))) nil)

(define-minor-mode phantom-inline-comment-minor-mode
  :init-value nil
  :global nil
  :keymap phantom-inline-comment-minor-mode-map
  :lighter " Phantom")

(defun pic--find-overlays-specifying ()
  "Find overlays below the cursor."
  (let ((overlays (overlays-at (point-at-eol)))
	found)
    (while overlays
      (let ((overlay (car overlays)))
	(if (overlay-get overlay 'phantom)
	    (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun pic--get-overlays-after-string ()
  "Get text property from the overlays below the cursor."
  (let* ((ovs (pic--find-overlays-specifying))
	 (ov (car ovs)))
    (if (overlayp ov)
	(overlay-get ov 'after-string)
      "")))

(defun pic--exist-overlays ()
  (let* ((ovs (pic--find-overlays-specifying))
	 (ov (car ovs)))
    (if (overlayp ov)
	ov)))

(defun generate-inline-phantom-comment (msg)
  "Generate overlay below the cursor with MSG."
  (pcase-let* ((pos-eol (point-at-eol))
	       (ov (make-overlay pos-eol (1+ pos-eol)))
	       (str (concat (when (eq pos-eol (point-max)) "\n")
			    msg "\n"))
	       (propertized-str (propertize str 'face 'phantom-inline-commnet-face)))
    (overlay-put ov 'phantom t)
    (overlay-put ov 'after-string propertized-str)
    ov))

(defun phantom-inline-comment--delete (phantom)
  "Delete overlay with PHANTOM property."
  (when (overlay-get phantom 'phantom)
    (delete-overlay phantom)))

(defun phantom-inline-comment--add (msg)
  "Push overlay with MSG to the stack."
  (push (generate-inline-phantom-comment msg) inline--phantom-comments))

(defun phantom-inline-comment--delete-all ()
  "Delete all the overlays in the stack."
  (mapc #'phantom-inline-comment--delete inline--phantom-comments)
  (setq inline--phantom-comments nil)
  (message "All the phantom inline comments are deleted!"))

(defun phantom-inline-comment--display-edit-buffer ()
  "Open popup-window to edit a phantom inline comment."
  (interactive)
  (popwin:popup-buffer
   (generate-new-buffer phantom-inline-comment-edit-buffer))
  (phantom-inline-comment-minor-mode 1))

(defun phantom-inline-comment--delete-below ()
  "Delete overlay after Find it."
  (interactive)
  (let* ((ovs (pic--find-overlays-specifying))
	 (ov (car ovs)))
    (if ov
	(phantom-inline-comment--delete ov)
      (message "No phantom inline comment is found"))))

(defun phantom-inline-comment--apply-buffer ()
  "Apply change in edit-buffer to the master buffer."
  (interactive)
  (let* ((str (buffer-string)))
    (popwin:close-popup-window)
    (cond ((eq phantom-inline-comment-state 'add)
	   (phantom-inline-comment--add str))
	  ((eq phantom-inline-comment-state 'edit)
	   (phantom-inline-comment--add str)
	   (phantom-inline-comment--delete-below)))))

(defun phantom-inline-comment--edit-below ()
  "Edit text on overlay and Open edit-buffer."
  (let* ((prev-comment (pic--get-overlays-after-string))
	 (raw-prev-comment (substring-no-properties prev-comment)))
    (phantom-inline-comment--display-edit-buffer)
    (insert raw-prev-comment)))

(defun phantom-inline-comment--cancel ()
  "Close popup-window."
  (interactive)
  (popwin:close-popup-window))

;;; Main Functions

(defun phantom-inline-comment-add ()
  "Add phantom inline comment below line of the cursor."
  (setq phantom-inline-comment-state 'add)
  (phantom-inline-comment--display-edit-buffer))

(defun phantom-inline-comment-edit-below ()
  "Edit phantom inline comment below line of the cursor."
  (setq phantom-inline-comment-state 'edit)
  (phantom-inline-comment--edit-below))

(defun phantom-inline-comment ()
  (interactive)
  (if (pic--exist-overlays)
      (phantom-inline-comment-edit-below)
    (phantom-inline-comment-add)))

(defun phantom-inline-comment-delete-below()
  "Delete phantom inline comment below line of the cursor."
  (interactive)
  (phantom-inline-comment--delete-below))

(defun phantom-inline-comment-delete-all ()
  "Delete all the phantom inline comments."
  (interactive)
  (phantom-inline-comment--delete-all))

;; * provide

(provide 'phantom-inline-comment)

;;; phantom-inline-comment.el ends here
