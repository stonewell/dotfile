;;; as-emacs-funcssetup.el -- add personal useful functions
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

(provide 'as-emacs-funcs-setup)
;;; as-emacs-funcs-setup.el ends here
