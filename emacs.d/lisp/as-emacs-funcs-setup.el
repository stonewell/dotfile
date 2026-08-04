;;; as-emacs-funcssetup.el -- add personal useful functions  -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(92 . 50) '(100 . 100)))))

(defun qjp-open-new-line (&optional prefix)
  "Open a new line just below or above the current line."
  (interactive "P")
  (if prefix
      (progn
        (beginning-of-line)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode))
    (end-of-line)
    (newline-and-indent)))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (interactive)
  (while list
    (print (car list))
    (setq list (cdr list))))

;; Shared dispatch commands for the menu stacks (`as-emacs-hydra-setup' and
;; `as-emacs-transient-setup'): both stacks bind these to the same key, and
;; giving each a real, documented symbol (instead of an inline sexp/lambda
;; in each menu definition) means the hydra hint, `which-key', and
;; `describe-key' all show a meaningful name instead of a raw dispatch form.
(defun as-menu-occur ()
  "Occur, via consult or helm depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p) (consult-line) (helm-occur)))

;; `helm-multi-occur' doesn't exist -- the real, bare-interactive function
;; is `helm-occur-visible-buffers' (`helm-multi-occur-1' takes a BUFFERS
;; list arg, not meant to be called with none).
(defun as-menu-occur-multi-buffer ()
  "Occur across all visible buffers, via consult or helm."
  (interactive)
  (if (as-emacs-vertico-p)
    (consult-line-multi)
    (helm-occur-visible-buffers)))

;; Mirrors `helm-fd-project' (still separately bound at `C-c h /' under the
;; helm stack); `consult-fd' has no other binding, this is its only path
;; under vertico.
(defun as-menu-find-files-fd ()
  "Find files by name via fd, via consult or helm depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p) (consult-fd) (helm-fd-project)))

(defun as-menu-switch-buffer ()
  "Switch buffer, via consult or helm depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p) (consult-buffer) (helm-mini)))

(defun as-menu-list-buffers ()
  "List buffers, via consult or helm depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p) (consult-buffer) (helm-buffers-list)))

(defun as-menu-find-file ()
  "Find file, via plain or helm `find-file' depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p)
    (call-interactively 'find-file)
    (call-interactively 'helm-find-files)))

(defun as-menu-recentf ()
  "Open a recent file, via consult or helm depending on `as-emacs-completion-stack'."
  (interactive)
  (if (as-emacs-vertico-p) (recentf-open) (helm-recentf)))

(provide 'as-emacs-funcs-setup)
;;; as-emacs-funcs-setup.el ends here
