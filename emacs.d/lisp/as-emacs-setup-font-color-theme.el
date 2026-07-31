;;; as-emacs-setup-font-color-theme --- set up color related stuff  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

;;; (load-theme 'solarized t t)
;;; (load-theme 'zenburn t)
(load-theme 'dracula t)
;;; (load-theme 'gruvbox-dark-hard t)

;; (require 'ef-themes)
;; (load-theme 'ef-summer :no-confirm)


;;; reset selection/region background
;;; dracula set the region background to dark grey
(set-face-attribute 'region nil :foreground "#282a36" :background "#f1fa8c")
(set-face-attribute 'default nil :font "SauceCodePro NFM-14")

;; `helm-grep--filter-candidate-1' always re-propertizes the filename/
;; line-number segments with these two faces, unconditionally overriding
;; whatever path/line color ripgrep's own `--colors' output would
;; otherwise show -- so give them their own theme-matching colors here
;; instead. Only defined when the helm completion stack is active.
(when (facep 'helm-grep-file)
  (set-face-attribute 'helm-grep-file nil :foreground "#bd93f9")
  (set-face-attribute 'helm-grep-lineno nil :foreground "#50fa7b"))

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(provide 'as-emacs-setup-font-color-theme)
;;; as-emacs-setup-font-color-theme.el ends here
