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

(defvar phantom-inline-comment-show-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") 'phantom-goto-comment)
    kmap))

(defvar phantom-inline-comment-edit-buffer "*phantom-inline-comment-edit*")
(defvar phantom-inline-comment-show-buffer "*phantom-inline-comments*")

(defvar phantom-inline-comment-state nil) ;; 'add or 'edit
(defvar phantom-inline-comment-visibility 'show) ;; 'hide or 'show

(defvar inline--phantom-comments nil)

(defface phantom-inline-commnet-face '((t (:inherit highlight))) nil)
(defface phantom-inline-commnet-list-face '((t (:foreground "purple"))) nil)

(define-minor-mode phantom-inline-comment-minor-mode
  :init-value nil
  :global nil
  :keymap phantom-inline-comment-minor-mode-map
  :lighter " Phantom")

(defun phantom-show-mode nil
  "Major mode for `phantom-show-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'phantom-show-mode)
  (setq mode-name "phantom-comments")
  (use-local-map phantom-inline-comment-show-mode-map))

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

(defun pic--build-file-info-list ()
  (let* ((header "FileName:LineNum/Comment")
	 (phantoms inline--phantom-comments)
	 (raw-file-info-list
	  (mapconcat
	   #'(lambda (phantom)
	       (with-current-buffer (overlay-buffer phantom)
		 (let* ((line (s-trim-left (buffer-substring (overlay-start phantom)
							     (overlay-end phantom))))
			(line-num (count-lines (point-min) (overlay-start phantom)))
			(comment (overlay-get phantom 'after-string))
			(string
			 (concat (format "%s:%d %s" (buffer-name) line-num comment))))
		   (put-text-property 0 (length string) 'phantom-buffer (buffer-name) string)
		   (put-text-property 0 (length string) 'phantom-comment line-num string)
		   string)))
	   phantoms
	   " "))
	 (file-info-list (split-string raw-file-info-list)))
    (list header file-info-list)))

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

(defun pic--toggle-text-visibility (phantom bool)
  "Modify visibility of PHANTOM comment depends on BOOL."
  (if (overlayp phantom)
      (let* ((str (overlay-get phantom 'after-string)))
	(put-text-property 0 (max 0 (- (length str) 1)) 'invisible bool str))))

(defun phantom-inline-comment--clean-up-comments ()
  (let* ((result nil))
    (dolist (phantom-comment inline--phantom-comments)
      (if (overlay-start phantom-comment)
	  (push phantom-comment result)))
    (setq inline--phantom-comments result)))

(defun phantom-inline-comment--hide (phantom)
  "Fold PHANTOM."
  (pic--toggle-text-visibility phantom t))

(defun phantom-inline-comment--open (phantom)
  "Unfold PHANTOM."
  (pic--toggle-text-visibility phantom nil))

(defun phantom-inline-comment--delete (phantom)
  "Delete overlay with PHANTOM property."
  (when (overlay-get phantom 'phantom)
    (delete-overlay phantom))
  (phantom-inline-comment--clean-up-comments))

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

(defun phantom-inline-comment--toggle-all ()
  "Toggle visibility of phantom comments."
  (interactive)
  (cond ((eq phantom-inline-comment-visibility 'show)
	 (phantom-inline-comment--hide-all))
	((eq phantom-inline-comment-visibility 'hide)
	 (phantom-inline-comment--open-all))))

(defun phantom-inline-comment--edit-below ()
  "Edit text on overlay and Open edit-buffer."
  (let* ((prev-comment (pic--get-overlays-after-string))
	 (raw-prev-comment (substring-no-properties prev-comment)))
    (phantom-inline-comment--display-edit-buffer)
    (insert raw-prev-comment)))

(defun phantom-goto-comment ()
  (interactive)
  (let ((buffer-name (get-text-property (point) 'phantom-buffer))
	(line (get-text-property (point) 'phantom-comment)))
    (if (null buffer-name)
	(message "No phantom comment at this line.")
      (pop-to-buffer (get-buffer buffer-name))
      (goto-line line))))

(defun phantom-inline-comment--show (header lines)
  (if (= (length lines) 0)
      (message "No phantom inline comments found.")
    (popwin:popup-buffer
     (generate-new-buffer phantom-inline-comment-show-buffer))
    (phantom-show-mode)
    (dolist (line lines)
      (let* ((line-info
	      (propertize
	       line 'font-lock-face 'phantom-inline-commnet-list-face)))
	(insert (concat line-info "\n"))))
    (setq header-line-format
	  (concat (propertize " " 'display '((space :align-to 0))) header))
    (setq buffer-read-only t)))

(defun phantom-inline-comment--show-all ()
  "Show all phantom overlays."
  (apply #'phantom-inline-comment--show
	 (pic--build-file-info-list)))

(defun phantom-inline-comment--hide-all ()
  "Fold all the phantoms."
  (let* ((overlays inline--phantom-comments))
    (mapc #'phantom-inline-comment--hide overlays)
    (setq phantom-inline-comment-visibility 'hide)
    (message "All the phantom inline comments are hidden!")))

(defun phantom-inline-comment--open-all ()
  "Unfold all the phantoms."
  (let* ((overlays inline--phantom-comments))
    (mapc #'phantom-inline-comment--open overlays)
    (setq phantom-inline-comment-visibility 'show)
    (message "All the phantom inline comments are appeared!")))

(defun phantom-inline-comment--cancel ()
  "Close popup-window."
  (interactive)
  (popwin:close-popup-window))

(defun phantom-inline-comment-add ()
  "Add phantom inline comment below line of the cursor."
  (setq phantom-inline-comment-state 'add)
  (phantom-inline-comment--display-edit-buffer))

(defun phantom-inline-comment-edit-below ()
  "Edit phantom inline comment below line of the cursor."
  (setq phantom-inline-comment-state 'edit)
  (phantom-inline-comment--edit-below))

;;; Main Functions

(defun phantom-inline-comment ()
  "Add or Edit phantom inline comment below line of the cursor."
  (interactive)
  (if (pic--exist-overlays)
      (phantom-inline-comment-edit-below)
    (phantom-inline-comment-add)))

(defun phantom-inline-comment-delete()
  "Delete phantom inline comment below line of the cursor."
  (interactive)
  (phantom-inline-comment--delete-below))

(defun phantom-inline-comment-delete-all ()
  "Delete all the phantom inline comments."
  (interactive)
  (phantom-inline-comment--delete-all))

(defun phantom-inline-comment-show-all ()
  "Show all the phantom inline comments."
  (interactive)
  (phantom-inline-comment--show-all))

(defun phantom-inline-comment-toggle-all ()
  "Fold/Unfold all the phantom inline comments."
  (interactive)
  (phantom-inline-comment--toggle-all))

;; * provide

(provide 'phantom-inline-comment)

;;; phantom-inline-comment.el ends here
