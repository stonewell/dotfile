;;; as-emacs-ibuffer-setup.el -- ibuffer setup file
;;; Code:
;;; Commentary:

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("perl" (mode . cperl-mode))
	       ("python" (mode . python-mode))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))
	       )
	      )
	     )
)

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'as-emacs-ibuffer-setup)
;;; as-emacs-ibuffer-setup.el ends here
