;;; as-emacs-objc-setup ------- config objc mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

;; hook objc-mode indent
(add-hook 'objc-mode-hook '(lambda ()
			     (setq c-basic-offset 2
				   tab-width 2
				   indent-tabs-mode t)
			     (c-set-offset 'statement-block-intro 0)))

(provide 'as-emacs-objc-setup)
;;; as-emacs-objc-setup ends here
