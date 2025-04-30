;;;; package --- All Packages  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  )

(use-package flycheck-cask
  :ensure t
  :defer t
  )

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook (
          ((prog-mode org-mode text-mode) . highlight-indent-guides-mode)
          )
  :config
  (setq highlight-indent-guides-method 'column)
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

(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme 1)
  (spaceline-helm-mode 1)
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
  (setq treesit-language-source-alist
      '(
         (cpp . ("https://github.com/stonewell/tree-sitter-cpp"))
         )
    )
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

(use-package hl-line
  :hook (
          ((prog-mode text-mode org-mode) . hl-line-mode)
          )
  :config
  ;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
  ;; clutter and slightly improving `hl-line-mode' performance.
  (setq hl-line-sticky-flag nil)
  (setq global-hl-line-sticky-flag nil)
  )

(use-package gcmh
  :ensure t
  :demand
  :hook
  (focus-out-hook . gcmh-idle-garbage-collect)

  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600)

  :config
  (gcmh-mode +1)
  )

(use-package emacs
  :demand
  :config
  (defun split-horizontally-for-temp-buffers ()
    "Split the window horizontally for temp buffers."
    (when (and (one-window-p t)
	    (not (active-minibuffer-window)))
      (split-window-horizontally)))

  (add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

  (defun split-window-prefer-horizonally (window)
    "If there's only one WINDOW (excluding any possibly active minibuffer), then split the current window horizontally."
    (if (and (one-window-p t)
	  (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
      (split-window-sensibly window)))
  (setq split-window-preferred-function 'split-window-prefer-horizonally)
  )

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
