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

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(provide 'as-emacs-setup-font-color-theme)
;;; as-emacs-setup-font-color-theme.el ends here
