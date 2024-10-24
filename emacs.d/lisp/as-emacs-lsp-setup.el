(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;;
         (c-or-c++-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
             :ensure t
             :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp
             :ensure t
             :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
             :ensure t
             :commands lsp-treemacs-errors-list)

(provide 'as-emacs-lsp-setup)
;;
