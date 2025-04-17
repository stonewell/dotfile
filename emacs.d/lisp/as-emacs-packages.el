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
(use-package google-c-style
  :ensure t
  :defer t
  :config
  (c-add-style "My-C-Style" '("Google"
                               (c-basic-offset . 4)
                               (c-indent-level . 4)
                               (c-offsets-alist . ((innamespace . 4)
                                                    (access-label . -)
                                                    (case-label . 0)
                                                    (member-init-intro . +)
                                                    (topmost-intro . 0)))))

  (defun my-c-mode-hook ()
    "My own c/c++ hook."
    (google-set-c-style)
    (c-set-style "My-C-Style")
    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq tab-width 4)
    (setq indent-tabs-mode nil)  ; use spaces only if nil
    (editorconfig-apply)
    )

  (defun my-c++-mode-hook ()
    "my own c++ mode hook"
    (setq flycheck-gcc-language-standard "c++14")
    (setq flycheck-clang-language-standard "c++14")
    )

  (add-hook 'c-mode-common-hook 'my-c-mode-hook)
  (add-hook 'c++-mode-common-hook 'my-c-mode-hook)
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  )

(use-package highlight-indentation
  :ensure t
  :defer t
  )

(use-package maxframe
  :ensure t
  :defer t
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
  :config
  (editorconfig-mode 1)
  )

(use-package htmlize
  :ensure t
  :defer t
  )

(use-package ws-butler
  :ensure t
  :config
  ;; use ws-butler to handle trailing white space
  (ws-butler-global-mode)
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  )

(use-package dtrt-indent
  :ensure t
  :after (editorconfig)
  :config
  (setq dtrt-indent-run-after-smie t) ;; Run even if SMIE is active
  (defun fix-indentation (&optional props)
    (dtrt-indent-mode 0)
    (dtrt-indent-mode 1)
    )
  (add-hook 'prog-mode-hook 'fix-indentation)
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
  :hook
  (go-ts-mode . go-format-on-save-mode)
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
  :hook (after-init . global-clipetty-mode)
  :if (not (display-graphic-p))
  )

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
