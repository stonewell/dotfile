;;; as-emacs-setup-windows -- windows setup code  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(load "server")

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

(message "server started:%s"
	 (server-running-p))

;;Add a hook to raise-frame
(add-hook 'server-visit-hook 'raise-frame)

;;; set font
(set-face-attribute 'default nil :font "Consolas-14")

;;; set null-device to aviod create NUL file
(setq null-device (concat (getenv "TMP") "\\null_device.txt"))

;; ---transparent settings
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(setq find-program (concat (getenv "SCOOP") "\\apps\\msys2\\current\\usr\\bin\\find.exe"))
(setq helm-rg-ripgrep-executable (concat (getenv "SCOOP") "\\shims\\rg.exe"))
(setq elpy-rpc-python-command (concat (getenv "PYTHON") "\\python.exe"))
(setq flycheck-python-pycompile-executable (concat (getenv "PYTHON") "\\python.exe"))

;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))

(provide 'as-emacs-setup-windows)
;;; as-emacs-setup-windows.el ends here
