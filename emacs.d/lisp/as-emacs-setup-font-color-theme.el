;;; as-emacs-setup-font-color-theme --- set up color related stuff
;;; Code:
;;; Commentary:
;; Using emacs --daemon and emacsclient I have often had cause to use emacs at the terminal,
;; but I like to have a colour scheme in my graphical frames that is unreadable in the console.
;; This code lives in my .emacs file and allows me to setup color and font settings for
;; graphical frames, but leave the console frames to use the default colour scheme.
;; I've found this very useful.  Tested with Emacs 24.0.50.1 @ 2010-20-07 -- Geoff Teale

;;; (require 'color-theme)
;;; (setq color-theme-is-global nil)

;;;(add-to-list 'load-path "~/.emacs.d/themes")
;;;(load-theme 'solarized t t)
(load-theme 'zenburn t)

;; (defun setup-window-system-frame-colours (&rest frame)
;;   (message "Running setup window system frame colours")
;;   (let ((f (if (car frame)
;; 	       (car frame)
;; 	     (selected-frame))))
;;     (if (display-graphic-p)
;; 	(progn
;; 	  (message "Running in Graphic mode")

;; 	  ;; set background color
;; 	  ;; (set-background-color "#D8F5D8")

;; 	  ;; set font
;; 	  (set-face-attribute 'default nil :height 140)
;; 	  ;;(set-face-background 'default "#232F2F" f)
;; 	  ;;(set-face-foreground 'default "#FFFFFF" f)
;; 	  ;;(set-face-background 'fringe  "#000000" f)
;; 	  ;;(set-face-background 'cursor "#2F4F4F" f)
;; 	  ;;(set-face-background 'mode-line "#2F4F4F" f)
;; 	  ;;(set-face-foreground 'mode-line "#BCBf91" f)
;; 	  )
;;       (progn
;; 	(message "Running Terminal mode")
;; 	;; (set-terminal-parameter f 'background-mode 'dark)
;; 	;; (set-frame-parameter f 'background-mode 'dark)
;; 	;; (require 'color-theme-solarized)
;; 	;; (color-theme-solarized)
;; 	)
;;       )
;;     )
;;   )

;; (require 'server)
;; (defadvice server-create-window-system-frame
;;     (after set-window-system-frame-colours ())
;;   "Set custom frame colours when creating the first frame on a display"
;;   (message "Running after frame-initialize")
;;   (setup-window-system-frame-colours))
;; (ad-activate 'server-create-window-system-frame)
;; (add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; set background color
;; (set-background-color "#D8F5D8")

;; set font
;; (set-face-attribute 'default nil :height 130)

(provide 'as-emacs-setup-font-color-theme)
;;; as-emacs-setup-font-color-theme.el ends here
