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

(defun phantom-inline-comment-add (msg)
  (interactive "sEnter Comment: ")
  (phantom-inline-comment--add msg))

(defun phantom-inline-comment-delete-all ()
  (interactive)
  (phantom-inline-comment--delete-all))
