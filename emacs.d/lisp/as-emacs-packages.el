;;;; package --- All Packages
;;; Code:
;;; Commentary:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(quelpa
 '(quelpa-use-package
  :fetcher git
  :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

(use-package auto-pair+
  :quelpa (auto-pair+ :fetcher git :url "https://github.com/emacsmirror/auto-pair-plus.git")
  :ensure t
  :defer t
  )
(use-package bind-key
  :ensure t
  :defer t
  )
(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  )
(use-package flycheck-cask
  :ensure t
  :defer t
  )
(use-package google-c-style
  :ensure t
  :defer t
  )
(use-package highlight-indentation
  :ensure t
  :defer t
  )
(use-package magit
  :ensure t
  :defer t
  )
(use-package maxframe
  :ensure t
  :defer t
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :defer t
  :bind
  (
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   )
  :config
  (progn
    (setq org-log-done 'time)
    (setq org-agenda-files
	  (list "~/org/agenda"))
    (setq org-startup-indented t)
    )
  )

(use-package p4
  :ensure t
  :config

  ;; ------ remove file visit hook
  (remove-hook 'find-file-hooks 'p4-update-status)
  )

(use-package popwin
  :ensure t
  :defer t
  :config
  (popwin-mode 1)
  )
(use-package python-mode
  :ensure t
  :defer t
  )
(use-package use-package
  :ensure t
  :defer t
  )
(use-package zenburn-theme
  :ensure t
  :defer t
  )
(use-package elpy
  :ensure t
  :defer t
  )

(use-package go-mode
  :ensure t
  :defer t
  :config
    (define-key go-mode-map (kbd "C-M-\\") 'gofmt t)
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  )

(use-package htmlize
  :ensure t
)

(use-package ws-butler
  :ensure t)

(use-package guess-style
  :quelpa (guess-style :fetcher git :url "https://github.com/nschum/guess-style.git")
  :ensure t
  :config
  (global-guess-style-info-mode 1)
  )

(use-package tree-sitter-langs
  :ensure t
  :defer t
  )

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
