;;; as-emacs-c-setup ------- config c/c++ mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'google-c-style)
(c-add-style "My-C-Style" '("Google"
                            (c-basic-offset . 4)
                            (c-indent-level . 4)
                            (c-offsets-alist . ((innamespace . 4)
                                                (access-label . -)
                                                (case-label . 0)
                                                (member-init-intro . +)
                                                (topmost-intro . 0)))))

(defun my-c-mode-hook ()
  "My own c/c++ hook."
  (google-set-c-style)
  (c-set-style "My-C-Style")
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; use spaces only if nil
  (editorconfig-apply)
  )

(defun my-c++-mode-hook ()
  "my own c++ mode hook"
  (setq flycheck-gcc-language-standard "c++14")
  (setq flycheck-clang-language-standard "c++14")
  )

(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "class\\s-+"
					  magic-mode-regexp-match-limit t)))
	       . c++-mode))

(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "namespace\\s-+"
					  magic-mode-regexp-match-limit t)))
	       . c++-mode))

(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "template\\s-*<\\s-*"
					  magic-mode-regexp-match-limit t)))
	       . c++-mode))

(add-to-list 'major-mode-remap-alist '(c-ts-mode . c-mode))
(add-to-list 'major-mode-remap-alist '(c++-ts-mode . c++-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-ts-mode . c-or-c++-mode))

(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(provide 'as-emacs-c-setup)
;;; as-emacs-c-setup ends here
