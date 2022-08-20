;;; as-emacs-cquery-setup --- setup cquery
;;; Commentary:
;;; Cquery
;;; Code:

(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))

(defun lsp-on ()
  (interactive)
  (condition-case nil
      (progn
	(lsp)
	(add-hook 'c-mode-hook #'cquery//enable)
	(add-hook 'c++-mode-hook #'cquery//enable)
	)
    (user-error nil)))

(defun global-disable-mode (mode-fn)
  "Disable `MODE-FN' in ALL buffers."
  (interactive "a")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode-fn -1))))

(defun lsp-off ()
  (interactive)
  (condition-case nil
      (progn
	(global-disable-mode 'lsp-mode)
	(remove-hook 'c-mode-hook #'cquery//enable)
	(remove-hook 'c++-mode-hook #'cquery//enable)
	)
    (user-error nil)))

(use-package markdown-mode
  :ensure t)

;; Language Server Protocol Plugin.
;; The actual plugin used to communicate with cquery.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t)

;; Flycheck and other IDE-feature support for LSP.
;; This has the "fancy features" and should be customized.
;; Personally, I turned the symbol highlighting off.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  )

;; Client to configure and auto-start cquery.
;; https://github.com/cquery-project/emacs-cquery
(use-package cquery
  :commands lsp-on lsp-off
  :config
  (setq cquery-executable "~/.program/cquery/bin/cquery")
  (setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t)))
  )


(provide 'as-emacs-cquery-setup)
;;; as-emacs-cquery-setup ends here
