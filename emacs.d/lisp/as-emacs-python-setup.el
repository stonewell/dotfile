;;; as-emacs-python-setup.el -- python mode setup  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.py?\\'" . python-ts-mode))

(use-package elpy
  :ensure t
  :defer t
  :preface
  (defun my-python-mode-hook ()
    "My own python indent hook."
    (setq python-indent 4
      tab-width 4
      indent-tabs-mode nil)
    (editorconfig-apply)
    (define-key python-ts-mode-map (kbd "C-M-\\") 'elpy-format-code)
    (elpy-mode)
    (message "my python mode hook")
    )
  :hook (
          (python-ts-mode . my-python-mode-hook)
          )
  :init
  (advice-add 'python-ts-mode :before 'elpy-enable)
  :config
  (setq elpy-modules
    (delq 'elpy-module-eldoc
      (delq 'elpy-module-django
        (delq 'elpy-module-yasnippet
          (delq 'elpy-module-highlight-indentation
            (delq 'elpy-module-company
              (delq 'elpy-module-flymake elpy-modules)
              )
            )
          )
        )
      )
    )
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
