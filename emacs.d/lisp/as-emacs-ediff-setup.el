;;; as-emacs-ediff-setup.el -- ediff setup file
;;; Code:
;;; Commentary:

;; restore window configuration after ediff quit
(add-hook 'ediff-load-hook
	  (lambda ()

	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))

	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

(provide 'as-emacs-ediff-setup)
;;; as-emacs-ediff-setup.el ends here
