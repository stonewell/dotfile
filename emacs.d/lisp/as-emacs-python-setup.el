;;; as-emacs-python-setup.el -- python mode setup
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.py?\\'" . python-ts-mode))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-ts-mode :before 'elpy-enable)
  )

(defun my-python-mode-hook ()
  "My own python indent hook."
  (setq python-indent 4
	tab-width 4
	indent-tabs-mode nil)
  (editorconfig-apply)
  (define-key python-ts-mode-map (kbd "C-M-\\") 'elpy-format-code)
  (elpy-mode)
)

(add-hook 'python-ts-mode-hook 'my-python-mode-hook)

(with-eval-after-load 'elpy-mode
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )
)

(setenv "PYTHONIOENCODING" "utf-8")
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))

(provide 'as-emacs-python-setup)
;;; as-emacs-python-setup.el ends here
