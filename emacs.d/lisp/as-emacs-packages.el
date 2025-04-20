;;;; package --- All Packages  -*- lexical-binding: t; -*-
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

(use-package bind-key
  :ensure t
  )

(use-package diminish
  :ensure t
  :defer t
  )

(use-package auto-pair+
  :quelpa (auto-pair+ :fetcher git :url "https://github.com/emacsmirror/auto-pair-plus.git")
  :ensure t
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

(use-package highlight-indentation
  :ensure t
  :defer t
  :hook (
          (prog-mode . highlight-indentation-mode)
          (org-mode . highlight-indentation-mode)
          (text-mode . highlight-indentation-mode)
          )
  )

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  )
(use-package use-package
  :ensure t
  :defer t
  )
(use-package zenburn-theme
  :ensure t
  :defer t
  )
(use-package dracula-theme
  :ensure t
  :defer t
  )
(use-package ef-themes
  :ensure t
  :defer t
  )
(use-package gruvbox-theme
  :ensure t
  :defer t
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1)
  )

(use-package htmlize
  :ensure t
  :defer t
  )

(use-package ws-butler
  :ensure t
  :hook prog-mode
  :config
  ;; use ws-butler to handle trailing white space
  (ws-butler-global-mode)
  )

(use-package dtrt-indent
  :ensure t
  :preface
  (defun fix-indentation(&optional props )
    (dtrt-indent-mode 0)
    (dtrt-indent-mode 1)
    (message "fix indentation by disable/enable dtrt-indent-mode")
    )
  :after editorconfig
  :hook (
          (prog-mode . dtrt-indent-mode)
          (hack-local-variables-hook . fix-indentation)
          )
  :config
  (setq dtrt-indent-run-after-smie t) ;; Run even if SMIE is active
  (add-hook 'editorconfig-after-apply-functions 'fix-indentation)
  )

(use-package avy
  :ensure t
  :after (bind-key)
  :config
  (avy-setup-default)
  )

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  )

(use-package whole-line-or-region
  :quelpa (whole-line-or-region :fetcher git :url "https://github.com/purcell/whole-line-or-region.git")
  :ensure t
  :config
  (whole-line-or-region-global-mode +1)
  )

;; Fix path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'nil)
  )

(use-package reformatter
  :ensure t
  :defer t
  )

(use-package go-ts-mode
  :hook(
         (go-ts-mode . go-format-on-save-mode)
         )
  :config
  (reformatter-define go-format
    :program "~/go/bin/goimports"
    :args '("/dev/stdin"))
  )

(use-package winner
  :init
  (winner-mode +1)
  )

(use-package clipetty
  :ensure t
  :if (not (display-graphic-p))
  :config
  (add-hook 'after-init 'global-clipetty-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (
          (prog-mode . rainbow-delimiters-mode)
          )
  )

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook prog-mode
  :config
  (setq rainbow-x-colors nil)
  )

(use-package hl-line
  :hook (
          (prog-mode . hl-line-mode)
          (text-mode . hl-line-mode)
          (org-mode . hl-line-mode)
          )
  )

(use-package emacs
  :demand
  :config
  (menu-bar-mode -1)
  (when (featurep 'scroll-bar)
    (scroll-bar-mode -1))
  (when (featurep 'tool-bar)
    (tool-bar-mode -1))
  (column-number-mode)
  )

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
