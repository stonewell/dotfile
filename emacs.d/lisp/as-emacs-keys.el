;;; as-emacs-key --- Setup key bindings
;;; Commentary:

;;; Code:
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-.") 'push-mark-command)
;;for tty emacs
(global-set-key (kbd "C-x ,") 'push-mark-command)
;; comment uncomment
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(eval-after-load "cc-mode"
  '(progn
     (define-key c++-mode-map (kbd "C-c C-c") nil)
     (define-key c-mode-map (kbd "C-c C-c") nil)
   )
)

;;; Unbind the stupid minimize that I always hit.
(global-unset-key "\C-z")
;;; disable ime in emacs
(global-unset-key "\C-\\")

;;; super + u to revert buffer
(global-set-key (kbd "s-u") 'revert-buffer)

(bind-keys :prefix-map my-customize-prefix-map
  :prefix "C-c s"
  ("f" . helm-browse-project)
  )

(provide 'as-emacs-keys)
;;; as-emacs-keys.el ends here
