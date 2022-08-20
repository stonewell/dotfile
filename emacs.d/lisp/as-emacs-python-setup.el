;;; as-emacs-python-setup.el -- python mode setup
;;; Commentary:
;;; Code:
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(elpy-enable)

(defun my-python-mode-hook ()
  "My own python indent hook."
  (setq python-indent 4
	tab-width 4
	indent-tabs-mode nil)
  (editorconfig-apply)
  (guess-style-guess-all)
  (when indent-tabs-mode
    (guess-style-guess-tab-width))
  (define-key python-mode-map (kbd "C-M-\\") 'elpy-format-code)
)

(add-hook 'python-mode-hook 'my-python-mode-hook)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

(setenv "PYTHONIOENCODING" "utf-8")
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))

(provide 'as-emacs-python-setup)
;;; as-emacs-python-setup.el ends here
