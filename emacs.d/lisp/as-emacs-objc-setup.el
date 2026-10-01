;;; as-emacs-objc-setup ------- config objc/objcpp mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package c-ts-mode
  :ensure nil
  :preface
  (defvar objcpp-ts-mode--font-lock-settings nil
    "Objective-C font lock rules explicitly bound to the \\='objc parser.")

  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
    (define-derived-mode objcpp-ts-mode c++-ts-mode "ObjC++[TS]"
      "Major mode for editing Objective-C++ (.mm) files using separate official parsers."
      :group 'objc
      (when (and (treesit-ready-p 'cpp) (treesit-ready-p 'objc))
        (c-ts-common-comment-setup)

        ;; Dynamically build the font lock settings if not already defined
        (unless objcpp-ts-mode--font-lock-settings
          (setq objcpp-ts-mode--font-lock-settings
                (treesit-font-lock-rules
                 :language 'objc
                 :override t
                 :feature 'keyword
                 `((,(apply #'vector
                            (seq-filter
                             (lambda (tok)
                               ;; only keep tokens that exist in the installed grammar
                               (ignore-errors
                                 (treesit-query-compile
                                  'objc `([,tok] @font-lock-keyword-face) t)
                                 t))
                             '("@interface" "@implementation" "@protocol" "@end"
                               "@property" "@synthesize" "@dynamic" "@public"
                               "@private" "@protected" "@selector" "@encode"
                               "id" "Class" "SEL" "BOOL" "YES" "NO")))
                     @font-lock-keyword-face))
                 :language 'objc
                 :override t
                 :feature 'definition
                 '((method_definition (identifier) @font-lock-function-name-face)
                   (method_declaration (identifier) @font-lock-function-name-face))
                 :language 'objc
                 :override t
                 :feature 'function
                 '((message_expression method: (identifier) @font-lock-function-call-face))
                 :language 'objc
                 :override t
                 :feature 'preprocessor
                 '((module_import "@import" @font-lock-preprocessor-face)))))

        (treesit-parser-create 'objc)
        (setq-local treesit-font-lock-settings
                    (append (c-ts-mode--font-lock-settings 'cpp)
                            objcpp-ts-mode--font-lock-settings))
        (treesit-major-mode-setup))))
  :init
  ;; If tree-sitter is available, route to our custom mode; otherwise, fallback to classic objc-mode
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (add-to-list 'auto-mode-alist '("\\.mm\\'" . objcpp-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))))

(provide 'as-emacs-objc-setup)
;;; as-emacs-objc-setup ends here
