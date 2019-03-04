(defvar phantom-inline-comment-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-g") 'phantom-inline-comment--apply-buffer)
    kmap))

(defvar phantom-inline-comment-edit-buffer "*phantom-inline-comment-edit*")

(define-minor-mode phantom-inline-comment-minor-mode
  :init-value nil
  :global nil
  :keymap phantom-inline-comment-minor-mode-map
  :lighter " Phantom")

(defvar inline--phantom-comments nil)

(defun generate-inline-phantom-comment (msg)
  (pcase-let* ((pos-eol (point-at-eol))
	       (ov (make-overlay pos-eol (1+ pos-eol)))
	       (str (concat (when (eq pos-eol (point-max)) "\n")
			    msg "\n")))
    (overlay-put ov 'phantom t)
    (overlay-put ov 'after-string str)
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

(defun phantom-inline-comment--apply-buffer ()
  (interactive)
  (let* ((str (buffer-string)))
    (popwin:close-popup-window)
    (phantom-inline-comment--add str)))

;;; Main Functions

(defun phantom-inline-comment-add ()
  (interactive)
  (phantom-inline-comment--display-edit-buffer))

(defun phantom-inline-comment-delete-all ()
  (interactive)
  (phantom-inline-comment--delete-all))
