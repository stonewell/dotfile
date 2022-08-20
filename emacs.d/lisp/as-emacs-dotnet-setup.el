;;; as-emacs-dotnet-setup.el --- Summary
;;; Commentary:
;;; Code:

;;;Set CSharp Auto Load Mode
(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs :ensure t)

(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-tree-sitter-mode-hook 'my-csharp-mode-setup t)

(if (file-readable-p "~/.program/omnisharp-server/run")
	(setq omnisharp-server-executable-path "~/.program/omnisharp-server/run")
)

(provide 'as-emacs-dotnet-setup)
;;; as-emacs-dotnet-setup ends here
