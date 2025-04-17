;;; Package --- linux specific config  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:
;;(if (file-exists-p
;; (concat "/tmp/emacs"
;;        (number-to-string
;;          (user-real-uid)) "/server"))
;;nil (server-start))
(load "server")
(unless (server-running-p) (server-start))

;;Add a hook to raise-frame
(add-hook 'server-visit-hook 'raise-frame)
(add-hook 'server-switch-hook 'raise-frame)

(setq mf-offset-x 0)

(set-face-attribute 'default nil :height 130)

(setq elpy-rpc-python-command "python3")

(provide 'as-emacs-setup-linux)
;;; as-emacs-setup-linux.el ends here
