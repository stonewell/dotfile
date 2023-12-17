;;; as-emacs-treesit-setup.el -- treesit mode setup
;;; commentary:
;;; code:

(use-package treesit-auto
  :after treesit
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'nil)
)

(provide 'as-emacs-treesit-setup)
;;; as-emacs-treesit-setup.el ends here
