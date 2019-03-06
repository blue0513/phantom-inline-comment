(defvar phantom-inline-comment-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-c") 'phantom-inline-comment--apply-buffer)
    (define-key kmap (kbd "C-g") 'phantom-inline-comment--cancel)
    kmap))

(defvar phantom-inline-comment-edit-buffer "*phantom-inline-comment-edit*")
(defface phantom-inline-commnet-face '((t (:inherit highlight))) nil)
(defvar phantom-inline-comment-state nil) ;; 'add or 'edit

(define-minor-mode phantom-inline-comment-minor-mode
  :init-value nil
  :global nil
  :keymap phantom-inline-comment-minor-mode-map
  :lighter " Phantom")

(defvar inline--phantom-comments nil)

(defun pic--find-overlays-specifying ()
  (let ((overlays (overlays-at (point-at-eol)))
	found)
    (while overlays
      (let ((overlay (car overlays)))
	(if (overlay-get overlay 'phantom)
	    (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun pic--get-overlays-after-string ()
  (let* ((ovs (pic--find-overlays-specifying)))
    (overlay-get (car ovs) 'after-string)))

(defun generate-inline-phantom-comment (msg)
  (pcase-let* ((pos-eol (point-at-eol))
	       (ov (make-overlay pos-eol (1+ pos-eol)))
	       (str (concat (when (eq pos-eol (point-max)) "\n")
			    msg "\n"))
	       (propertized-str (propertize str 'face 'phantom-inline-commnet-face)))
    (overlay-put ov 'phantom t)
    (overlay-put ov 'after-string propertized-str)
    ov))

(defun phantom-inline-comment--delete (phantom)
  (when (overlay-get phantom 'phantom)
    (delete-overlay phantom)))

(defun phantom-inline-comment--add (msg)
  (push (generate-inline-phantom-comment msg) inline--phantom-comments))

(defun phantom-inline-comment--delete-all ()
  (mapc #'phantom-inline-comment--delete inline--phantom-comments)
  (setq inline--phantom-comments nil))

(defun phantom-inline-comment--display-edit-buffer ()
  (interactive)
  (popwin:popup-buffer
   (generate-new-buffer phantom-inline-comment-edit-buffer))
  (phantom-inline-comment-minor-mode 1))

(defun phantom-inline-comment--delete-below ()
  (interactive)
  (let* ((ovs (pic--find-overlays-specifying)))
    (phantom-inline-comment--delete (car ovs))))

(defun phantom-inline-comment--apply-buffer ()
  (interactive)
  (let* ((str (buffer-string)))
    (popwin:close-popup-window)
    (cond ((eq phantom-inline-comment-state 'add)
	   (phantom-inline-comment--add str))
	  ((eq phantom-inline-comment-state 'edit)
	   (phantom-inline-comment--add str)
	   (phantom-inline-comment--delete-below)))))

(defun phantom-inline-comment--edit-below ()
  (let* ((prev-comment (pic--get-overlays-after-string))
	 (raw-prev-comment (substring-no-properties prev-comment)))
    (phantom-inline-comment--display-edit-buffer)
    (insert raw-prev-comment)))

(defun phantom-inline-comment--cancel ()
  (interactive)
  (popwin:close-popup-window))

;;; Main Functions

(defun phantom-inline-comment-add ()
  (interactive)
  (setq phantom-inline-comment-state 'add)
  (phantom-inline-comment--display-edit-buffer))

(defun phantom-inline-comment-delete-below()
  (interactive)
  (phantom-inline-comment--delete-below))

(defun phantom-inline-comment-delete-all ()
  (interactive)
  (phantom-inline-comment--delete-all))

(defun phantom-inline-comment-edit-below ()
  (interactive)
  (setq phantom-inline-comment-state 'edit)
  (phantom-inline-comment--edit-below))
