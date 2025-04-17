;;; Package --- Mac specific config  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(defun ns-raise-emacs ()
  (if (eq system-type 'darwin)
      (if (display-graphic-p)
	  (ns-do-applescript "tell application \"Emacs\" to activate")
	)
    )
  )
(add-hook 'server-visit-hook 'ns-raise-emacs())

;; When loading files reuse existing frames.
(setq gnuserv-frame (car (frame-list)))

(if (file-exists-p
     (concat (getenv "TMPDIR") "emacs"
	     (number-to-string
	      (user-real-uid)) "/server"))
    nil (server-start))

;; do not open new frame on osX
(setq ns-pop-up-frames nil)

(setq mf-offset-x 0)

;; set global key to buffer begin and buffer-end

(set-keyboard-coding-system nil)
(set-face-attribute 'default nil :height 150)

(provide 'as-emacs-setup-darwin)
;;; as-emacs-setup-darwin.el ends here
