;;; as-emacs-c-setup ------- config c/c++ mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
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

(defvar my-custom-ts-indent-style
  `(
     ((node-is ")") parent-bol 0)
     ((node-is "case_statement") parent-bol 0)

     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
     ((parent-is "argument_list") prev-sibling 0)
     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
     ((parent-is "parameter_list") prev-sibling 0)

     ((parent-is "else_clause") parent-bol 0)
     ((parent-is "try_statement") parent-bol 0)
     ((parent-is "catch_clause") parent-bol 0)
     ((parent-is "for_range_loop") parent-bol 0)

     ;; Do not indent preprocessor statements.
     ((node-is "preproc") column-0 0)

     ;; Do not indent namespace children.
     ((parent-is "namespace_definition") parent-bol 0)
     ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)

     ((match "access_specifier" "base_class_clause") parent-bol c-ts-mode-indent-offset)
     ((node-is "access_specifier") parent-bol 0)
     ((match "}" "field_declaration_list") parent-bol 0)
     ((match "field_declaration_list" "class_specifier" "body") parent-bol 0)
     ((parent-is "field_declaration_list") parent-bol c-ts-mode-indent-offset)

     ((match "." "field_expression" nil 1 1) parent-bol c-ts-mode-indent-offset)
     )
  )

(defun my-ts-indent-style()
  "Override the default indentation style with some additional rules.
Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree, especially paired
with `(setq treesit--indent-verbose t)' to debug what rules is being applied at a given point.

This function must return a plain rules list (not a lang-keyed alist) because
`c-ts-mode-set-style' calls `c-ts-mode--simple-indent-rules' which already
wraps the result in ((LANG . RULES)).  Returning ((cpp . RULES)) here would
produce ((cpp . ((cpp . RULES)))), causing treesit to evaluate the symbol `cpp'
as an indent predicate and signal \"Symbol's function definition is void: cpp\"."
  (let* ((mode (if (derived-mode-p 'c-ts-mode) 'c 'cpp))
         (default-style (cdr (assq mode treesit-simple-indent-rules)))
         ;; Prepend custom rules; defaults act as fallback at the end.
         (combined-style (append my-custom-ts-indent-style default-style)))
    combined-style)
  )

(defun my-c-ts-mode-hook()
  (c-ts-mode-set-style #'my-ts-indent-style)
  )

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  :config
  (add-hook 'c-ts-mode-hook 'my-c-ts-mode-hook)
  )

(use-package c++-ts-mode
  :if (treesit-language-available-p 'cpp)
  :config
  (add-hook 'c++-ts-mode-hook 'my-c-ts-mode-hook)
  )

(provide 'as-emacs-c-setup)
;;; as-emacs-c-setup ends here
