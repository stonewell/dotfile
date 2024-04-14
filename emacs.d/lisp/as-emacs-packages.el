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

(use-package counsel
  :ensure t
  :config

  (defun emacs-counsel-launcher ()
    "Create and select a frame called emacs-counsel-launcher which consists only of a minibuffer and has specific dimensions. Runs counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
    (interactive)
    (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                     (minibuffer . only)
                     (fullscreen . 0) ; no fullscreen
                     (undecorated . t) ; remove title bar
                     ;;(auto-raise . t) ; focus on this frame
                     ;;(tool-bar-lines . 0)
                     ;;(menu-bar-lines . 0)
                     (internal-border-width . 10)
                     (width . 80)
                     (height . 11)))
      (unwind-protect
        (counsel-linux-app)
        (delete-frame))))
  )

(use-package tramp
  :init
  (setq tramp-verbose 2)
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-chunksize 2000)
  (if (eq window-system 'w32)
    (setq tramp-default-method "plink")
    (progn
      (setq tramp-default-method "ssh")
      (setq tramp-ssh-controlmaster-options
        (concat "-o ControlPath=~/.ssh/control-%%r@%%h:%%p "
                "-o ControlMaster=auto "
                "-o ControlPersist=yes"))
    )
  )

  )

(use-package all-the-icons
  :ensure t
  :defer t
  :if (display-graphic-p))

(use-package dirvish
  :ensure t
  :defer t
  :init (dirvish-override-dired-mode)
  :custom
   (dirvish-mode-line-format
   '(:left (sort file-time "" file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  :config
  (dirvish-peek-mode)
  (setq dired-dwim-target         t
        dired-recursive-copies    'always
        dired-recursive-deletes   'top)
  )

(use-package winner
  :init
  (winner-mode +1)
  )

(provide 'as-emacs-packages)
;;; as-emacs-packages.el ends here
