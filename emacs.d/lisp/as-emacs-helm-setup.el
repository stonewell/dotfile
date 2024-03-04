;;; as-emacs-helm-setup --- setup helm
;;; Commentary:
;; Helm
;;; Code:

(use-package helm
  ;; :init
  ;; :bind (;("M-x" . helm-M-x)
  ;;        ("M-y" . helm-show-kill-ring)
  ;;        ("C-x b" . helm-mini)
  ;;        ("M-/" . helm-dabbrev))
  :ensure t
  :after (bind-key)
  :config
  (setq
    helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
    helm-quick-update t ; do not display invisible candidates
    helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
    helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
    helm-split-window-default-side 'other ;; open helm buffer in another window
    helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
    helm-candidate-number-limit 200 ; limit the number of displayed canidates
    helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
    ;; helm-command
    helm-M-x-requires-pattern 0     ; show all candidates when set to 0

    ;;helm-files
    helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
    helm-boring-file-regexp-list
    '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
    helm-ff-file-name-history-use-recentf t
    ;; helm-buffers
    helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
    ;; ido
    ido-use-virtual-buffers t      ; Needed in helm-buffers-list
    )
  (bind-keys ("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    ("C-x C-r" . helm-recentf)
    )
  (bind-keys :map helm-map
    ("C-o" . nil)
    ("TAB" . helm-execute-persistent-action)
    ("C-i" . helm-execute-persistent-action)
    ("C-z" . helm-select-action)
    ("C-h" . delete-backward-char))

  (when (executable-find "ag")
    (bind-keys
      ("M-p" . helm-do-ag-project-root)
      )
    )

  (when (executable-find "pyeverything")
    (progn
      (require 'as-emacs-helm-pyeverything)

      (bind-keys ("C-t" . helm-ff-run-pyeverything)
        ("C-M-t" . helm-ag-run-pyeverything)
        ("M-p" . helm-do-ag-project-root)
        )
      )
    )

  (when (executable-find "rg")
    (bind-keys
      ("M-p" . helm-do-ag-project-root)
      )
    )

  ;;helm-files
  (require 'helm-files)
  (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                      '(picture-mode artist-mode)))
  (bind-keys :map helm-find-files-map
    ("C-h" . delete-backward-char)
    ("C-i" . helm-execute-persistent-action))
  ;; helm-grep
  (bind-keys :map helm-grep-mode-map
    ("RET" . helm-grep-mode-jump-other-window)
    ("n" . helm-grep-mode-jump-other-window-forward)
    ("p" . helm-grep-mode-jump-other-window-backward))

  (bind-keys :prefix-map helm-prefix-map
    :prefix "C-c s"
    ("f" . helm-browse-project)
    )

  (with-eval-after-load 'tramp-cache (setq tramp-cache-read-persistent-data t))
  (with-eval-after-load 'auth-source (setq auth-source-save-behavior nil))

  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (define-key global-map [remap apropos-command] 'helm-apropos)

  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  )

(use-package helm-ag
  :ensure t
  :commands (helm-ag)
  :config
  (when (executable-find "pyeverything")
    (setq helm-ag-base-command "pyeverything helm-ag")
    )
  (when (executable-find "rg")
    (setq helm-ag-base-command "rg --no-heading --line-number --color never --vimgrep --smart-case")
    )
  (setq helm-ag-insert-at-point 'symbol
    helm-ag-success-exit-status '(0 2)
    helm-ag-show-status-function nil
	  )
  )

(use-package helm-swoop
  :ensure t
  :bind
  (("M-o" . helm-swoop)
    ("M-O" . helm-swoop-back-to-last-point)
    ("C-c M-o" . helm-multi-swoop)
    ;; ("C-c M-O" . helm-multi-swoop-all)
    )
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  (bind-keys :map isearch-mode-map
    ("M-o" . helm-swoop-from-isearch))
  (bind-keys :map helm-swoop-map
    ("M-o" . helm-multi-swoop-all-from-helm-swoop)
    ;; ("M-i" . helm-swoop-from-evil-search)
    )
  )

(use-package helm-xref
  :ensure t
  :defer t
  )

(use-package helm-ls-git
  :ensure t
  :defer t
  )

(use-package helm-fd
  :after helm
  :ensure nil
  :bind (:map helm-command-map
          ("/" . helm-fd-project))
  :config
  (setq helm-fd-mode-line-function nil)
  (setq helm-fd-switches '("--type" "f" "--type" "d" "--color" "always"))
  (defun helm-fd-project ()
    (interactive)
    (let ((directory (or (cdr (project-current))
                       (with-current-buffer "*scratch*" default-directory))))
      (helm-fd-1 directory))))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; start helm-mode
(use-package helm-mode
  :config
  (diminish 'helm-mode "")
  (helm-mode 1)
  )

(provide 'as-emacs-helm-setup)
;;; as-emacs-helm-setup.el ends here
