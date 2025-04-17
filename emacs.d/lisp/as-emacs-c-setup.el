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

(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; For dealing with WebKit long lines and word wrapping.
(defun c-mode-adaptive-indent (beg end)
  "Set the 'wrap-prefix' for the the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))
        (put-text-property lbp lep 'wrap-prefix (concat (fill-context-prefix lbp lep) (make-string c-basic-offset ? )))
	(search-forward "\n" end t))))

(define-minor-mode c-adaptive-wrap-mode
  "Wrap the buffer text with adaptive filling for c-mode."
  :lighter ""
  (save-excursion
    (save-restriction
      (widen)
      (let ((buffer-undo-list t)
	    (inhibit-read-only t)
	    (mod (buffer-modified-p)))
	(if c-adaptive-wrap-mode
	    (jit-lock-register 'c-mode-adaptive-indent)
	  (jit-lock-unregister 'c-mode-adaptive-indent)
	  (remove-text-properties (point-min) (point-max) '(wrap-prefix pref)))
        (restore-buffer-modified-p mod)))))

(defun c-adaptive-wrap-mode-for-webkit ()
  "Turn on visual line mode and adaptive wrapping for WebKit source files."
  (if (or (string-equal "My-C-Style" c-indentation-style)
          (string-equal "my-c-style" c-indentation-style))
      (progn
        (visual-line-mode t)
        (c-adaptive-wrap-mode t))))

(add-hook 'c-mode-common-hook 'c-adaptive-wrap-mode-for-webkit)
(add-hook 'hack-local-variables-hook 'c-adaptive-wrap-mode-for-webkit)

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

(defun my-indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(;; Here are your custom rules
     ((node-is ")") parent-bol 0)
     ((node-is "case_statement") parent-bol 0)

     ((match nil "argument_list" nil 1 1) parent-bol c-mode-indent-offset)
     ((parent-is "argument_list") prev-sibling 0)
     ((match nil "parameter_list" nil 1 1) parent-bol c-mode-indent-offset)
     ((parent-is "parameter_list") prev-sibling 0)

     ((parent-is "else_clause") parent-bol 0)
     ((parent-is "try_statement") parent-bol 0)
     ((parent-is "catch_clause") parent-bol 0)
     ((parent-is "for_range_loop") parent-bol 0)

       ;; namespace body do not indent
     ((match "access_specifier" "base_class_clause") parent-bol c-mode-indent-offset)
     ((node-is "access_specifier") parent-bol 0)
     ((match "}" "field_declaration_list") parent-bol 0)
     ((match "{" "field_declaration_list") parent-bol 0)
     ((parent-is "field_declaration_list") parent-bol c-mode-indent-offset)
     ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)

     ((match "." "field_expression" nil 1 1) parent-bol c-mode-indent-offset)

     ;; Append here the indent style you want as base
     ,@(alist-get 'bsd (c-mode--indent-styles 'cpp)))
  )

(provide 'as-emacs-c-setup)
;;; as-emacs-c-setup ends here
