;;; as-emacs-treesit-setup.el -- treesit mode setup
;;; commentary:
;;; code:

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'no)
)

(provide 'as-emacs-treesit-setup)
;;; as-emacs-treesit-setup.el ends here
