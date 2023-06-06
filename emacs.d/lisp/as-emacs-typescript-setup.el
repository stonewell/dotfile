;;; as-emacs-typescript-setup.el -- typescript mode setup
;;; commentary:
;;; code:
(use-package typescript-mode
  :after tree-sitter
  :ensure t
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;;(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))
;;(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . tsx))

;; (use-package tide
;;   :ensure t
;;   :after (company flycheck)
;;   :hook ((typescript-ts-mode . tide-setup)
;;         (tsx-ts-mode . tide-setup)
;;          (typescript-ts-mode . tide-hl-identifier-mode)
;;           (before-save . tide-format-before-save)))

(use-package tide
  :quelpa (tide :fetcher git :url "https://github.com/ananthakumaran/tide.git")
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
          (typescript-mode . tide-hl-identifier-mode)
          (before-save . tide-format-before-save)))

(provide 'as-emacs-typescript-setup)
;;; as-emacs-typescript-setup.el ends here
