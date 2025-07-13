;;; as-emacs-defaults-setup.el -- setup emacs defaults file  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(setq inhibit-startup-message t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq native-comp-speed 3)
(setq native-comp-async-report-warnings-errors nil)

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq local-custom-file (locate-user-emacs-file "local-customize-setup.el"))

(setq gc-cons-threshold 200000000)
(setq max-specpdl-size 5000)

;; set no backup
(setq auto-save-default nil)
(setq auto-save-default -1)
(setq backup-inhibited t)

;;; Shell mode
(setq comint-prompt-read-only t)

;; -----------------------------------------------------------------------
;; Set up coding system.
;; -----------------------------------------------------------------------
(prefer-coding-system 'utf-8)

;; make yank replace high light region
(delete-selection-mode 1)

;; Disable the alarm bell (https://www.emacswiki.org/emacs/AlarmBell).
(setq ring-bell-function 'ignore)

;; show paren pair
(show-paren-mode 1)

;; Turn on global line number mode
(if (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1)
  (global-linum-mode 1)
  )

(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))

(pixel-scroll-mode)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 35.0)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Remeber recent files.
(recentf-mode +1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)
(setq use-file-dialog nil)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setq comment-empty-lines t)

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or more
;; non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;; Modeline

;; Makes Emacs omit the load average information from the mode line.
(setq display-time-default-load-average nil)

(setq read-process-output-max (* 1024 1024))

;; ---transparent settings
(set-frame-parameter (selected-frame) 'alpha '(92 . 50))
(add-to-list 'default-frame-alist '(alpha . (92 . 50)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))
(when (featurep 'tool-bar)
  (tool-bar-mode -1))
(column-number-mode)

(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.5) ;; Seconds between font locking.
(setq jit-lock-chunk-size 4096)

(provide 'as-emacs-defaults-setup)
;;; as-emacs-defaults-setup.el ends here
