;;; as-emacs-setup.el -- main setup file  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(require 'as-emacs-defaults-setup)

;;package
(load "as-emacs-packages")

(load "as-emacs-helm-setup")

;;load font and color theme settings
(load "as-emacs-setup-font-color-theme")

;;load hydra
(require 'as-emacs-hydra-setup)

;;load personal functions
(require 'as-emacs-funcs-setup)

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
  (load "as-emacs-setup-darwin")
  )

(if (eq system-type 'windows-nt)
  ;;windows only setup
  (load "as-emacs-setup-windows")
  )

(if (eq system-type 'gnu/linux)
  ;;linux only setup
  (load "as-emacs-setup-linux")
  )

(when (file-exists-p custom-file)
  (load custom-file))

;; ------load customize setup
(if (file-readable-p "~/.emacs.d/local-customize-setup.el")
  (load "~/.emacs.d/local-customize-setup.el")
  )

(provide 'as-emacs-setup)
;;; as-emacs-setup.el ends here
