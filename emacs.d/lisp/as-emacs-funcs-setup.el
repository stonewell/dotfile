;;; as-emacs-funcssetup.el -- add personal useful functions  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(92 . 50) '(100 . 100)))))

(defun qjp-open-new-line (&optional prefix)
  "Open a new line just below or above the current line."
  (interactive "P")
  (if prefix
      (progn
        (beginning-of-line)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode))
    (end-of-line)
    (newline-and-indent)))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (interactive)
  (while list
    (print (car list))
    (setq list (cdr list))))


(provide 'as-emacs-funcs-setup)
;;; as-emacs-funcs-setup.el ends here
