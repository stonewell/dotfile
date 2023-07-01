;;; as-emacs-setup.el -- main setup file
;;; Code:
;;; Commentary:
(setq inhibit-startup-message t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;(require 'cl)

;;package
(load "as-emacs-packages")

(load "as-emacs-helm-setup")

;;load font and color theme settings
(load "as-emacs-setup-font-color-theme")

;;load dired setup
(require 'as-emacs-dired-setup)

;;load ediff setup
(require 'as-emacs-ediff-setup)

;;load ibuffer setup
(require 'as-emacs-ibuffer-setup)

;;load personal functions
(require 'as-emacs-funcs-setup)

(load "as-emacs-treesit-setup")

;;do maximize frame
;;(require 'maxframe)
;;(add-hook 'window-setup-hook 'maximize-frame t)

;;make buffer name unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; c/c++ mode
(require 'as-emacs-c-setup)

;;Python Mode
(require 'as-emacs-python-setup)

;; objc mode
(require 'as-emacs-objc-setup)

;; crosshair cursor mode
;; (crosshairs-mode t)

;; deft mode
(require 'as-emacs-deft-setup)

;; org/hugo mode
(require 'as-emacs-org-setup)

;; cquery mode
(require 'as-emacs-cquery-setup)

;; typescript mode
(require 'as-emacs-typescript-setup)

;; set no backup
(setq auto-save-default nil)
(setq auto-save-default -1)
(setq backup-inhibited t)

;;; Shell mode
;;(setq ansi-color-names-vector ; better contrast colors
;;      ["black" "red4" "green4" "yellow4"
;;      "blue3" "magenta4" "cyan4" "white"])
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;(tool-bar-mode nil)
;;(menu-bar-mode nil)
;;(scroll-bar-mode nil)
(if (not (eq system-type 'gnu/linux))
    (progn
     (if (functionp 'tool-bar-mode)	(tool-bar-mode -1))
     (if (functionp 'menu-bar-mode)	(menu-bar-mode -1))
     (if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
     )
  )

;; -----------------------------------------------------------------------
;; Set up coding system.
;; -----------------------------------------------------------------------
(prefer-coding-system 'utf-8)

;; ---------[match delemeters]
(load "delemiter_match")
(global-set-key "\C-c[" 'ben-bounce-sexp)
(global-set-key "\C-c%" 'bc-bounce-cpp)

;; ---------[find file using root]
(load "find_file_root")
(global-set-key [(control x) (control r)] 'find-file-root)
(global-set-key [(control x) (meta r)] 'find-alternative-file-root)

;;(require 'whitespace)
;;(setq whitespace-style (quote (face trailing)))
;;(global-whitespace-mode 1)

;; make yank replace high light region
(delete-selection-mode 1)

;; show paren pair
(show-paren-mode 1)

;; Turn on global line number mode
(if (fboundp 'global-display-line-numbers-mode)
	(global-display-line-numbers-mode 1)
	(global-linum-mode 1)
  )

;; Turn on winner mode for buffer layout restore
(winner-mode 1)

;; Delete trail white space before saving
;; only delete in not fundamental mode
;;(defun my-delete-trailing-whitespace()
;;  (interactive)
;;  (if (string-equal major-mode "fundamental-mode")
;;      (message "do not delete-trailing-withespace fundamental mode")
;;    (delete-trailing-whitespace)
;;    )
;;  )
;;(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

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

;; ---transparent settings
(set-frame-parameter (selected-frame) 'alpha '(92 . 50))
(add-to-list 'default-frame-alist '(alpha . (92 . 50)))

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

;; ------load customize setup
(if (file-readable-p "~/.emacs.d/local-customize-setup.el")
    (load "~/.emacs.d/local-customize-setup.el")
  )

(provide 'as-emacs-setup)
;;; as-emacs-setup.el ends here
