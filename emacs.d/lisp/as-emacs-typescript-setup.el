;;; as-emacs-typescript-setup.el -- typescript mode setup
;;; commentary:
;;; code:

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-ts-mode))

(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode +1)
  (make-local-variable 'flycheck-check-syntax-automatically)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (editorconfig-apply)
  )

(add-hook 'tsx-ts-mode-hook 'setup-tide-mode)
(add-hook 'typescript-ts-mode-hook 'setup-tide-mode)
(add-hook 'before-save-hook 'tide-format-before-save)

(use-package tide
  :quelpa (tide :fetcher git :url "https://github.com/ananthakumaran/tide.git")
  :ensure t
  :defer t
  :after (company flycheck)
  :hook (
          (typescript-ts-mode . setup-tide-mode)
          (tsx-ts-mode . setup-tide-mode)
          (before-save . tide-format-before-save)
          )
  :init
  (add-hook 'editorconfig-after-apply-functions (defun fix-tide-indentation (props)
                                                  (when (and (boundp 'tide-mode) tide-mode)
                                                    (make-local-variable 'standard-indent)
                                                    (setq standard-indent typescript-ts-mode-indent-offset)
                                                    (tide-command:configure)
                                                    )
                                                  )
    )
  )

(provide 'as-emacs-typescript-setup)
;;; as-emacs-typescript-setup.el ends here
