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
  )
(use-package bind-key
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
  )
(use-package highlight-indentation
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
(use-package dracula-theme
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

(use-package avy
  :ensure t
  :after (bind-key)
  :config
  (avy-setup-default)
  (bind-keys :prefix-map avy-prefix-map
    :prefix "C-c f"
    ("f" . avy-goto-char-2)
    ("t" . avy-goto-char)
    ("j" . avy-resume)
    )
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

(use-package key-chord
  :quelpa (key-chord :fetcher git :url "https://github.com/emacsorphanage/key-chord.git")
  :ensure t
  :after (evil)
  :config
  (key-chord-mode 1)
  (key-chord-define evil-emacs-state-map "kk" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "jj" (lambda () (interactive) (god-local-mode-resume)))
 )

(use-package evil-god-state
  :ensure t
  :quelpa
  )

(use-package evil
  :quelpa
  :ensure t
  :after (evil-god-state god-mode)
  :config
  (setcdr evil-insert-state-map nil)
  (setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-move-cursor-back nil)
  (setq-default evil-symbol-word-search t)
  (evil-mode 1)
  (evil-set-leader nil (kbd "<space>"))
  (evil-define-key 'normal 'global (kbd "i") 'evil-emacs-state)
  (add-hook 'evil-emacs-state-exit-hook (lambda () (god-local-mode -1)))
  (add-hook 'evil-emacs-state-entry-hook (lambda () (god-local-mode 1)))
  )

(use-package god-mode
  :quelpa
  :ensure t
  :config
  (define-key god-local-mode-map (kbd "i") #'(lambda () (interactive) (god-local-mode-pause)))
  )

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
