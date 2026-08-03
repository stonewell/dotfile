;;; as-emacs-setup.el -- main setup file  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(require 'as-emacs-defaults-setup)

;;package
(require 'as-emacs-packages)

;;load font and color theme settings
;;load theme first, we can override later
(require 'as-emacs-setup-font-color-theme)

;;load personal functions
(require 'as-emacs-funcs-setup)

;;load menus
(cond
  ((eq as-emacs-menu-stack 'transient) (require 'as-emacs-transient-setup))
  (t (require 'as-emacs-hydra-setup)))

(cond
  ((eq as-emacs-completion-stack 'vertico) (require 'as-emacs-vertico-setup))
  (t (require 'as-emacs-helm-setup)))

;;load key bindings
(require 'as-emacs-keys)

;; c/c++ mode
(require 'as-emacs-c-setup)

;;Python Mode
(require 'as-emacs-python-setup)

;; org/hugo mode
(require 'as-emacs-org-setup)

;; typescript mode
(require 'as-emacs-typescript-setup)

;; lsp mode
(require 'as-emacs-lsp-setup)

(if (eq system-type 'darwin)
  ;;darwin only setup
  (require 'as-emacs-setup-darwin)
  )

(if (eq system-type 'windows-nt)
  ;;windows only setup
  (require 'as-emacs-setup-windows)
  )

(if (eq system-type 'gnu/linux)
  ;;linux only setup
  (require 'as-emacs-setup-linux)
  )

(when (file-exists-p custom-file)
  (load custom-file))

;; load customize setup
(if (file-readable-p local-custom-file)
  (load local-custom-file)
  )

(provide 'as-emacs-setup)
;;; as-emacs-setup.el ends here
