;;; as-emacs-helm-setup --- setup helm
;;; Commentary:
;; Helm
;;; Code:

(use-package diminish
  :ensure t
  :defer t
  )

(use-package helm-config
  :init
  (custom-set-variables '(helm-command-prefix-key "C-;"))
  :config
  )

(use-package helm
  ;; :init
  ;; :bind (;("M-x" . helm-M-x)
  ;;        ("M-y" . helm-show-kill-ring)
  ;;        ("C-x b" . helm-mini)
  ;;        ("M-/" . helm-dabbrev))
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
   )
  (bind-keys ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ("C-x b" . helm-mini))
  (bind-keys :map helm-map
             ("C-o" . nil)
             ("TAB" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)
             ("C-h" . delete-backward-char))
  (require 'as-emacs-helm-pyeverything)
  (bind-keys ("C-t" . helm-ff-run-pyeverything))
  (bind-keys ("C-M-t" . helm-ag-run-pyeverything))
  )

(use-package helm-files
  :bind ("C-x C-f" . helm-find-files)
  :config
  (setq
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
  (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                            '(picture-mode artist-mode)))
  (bind-keys :map helm-find-files-map
             ("C-h" . delete-backward-char)
             ("C-i" . helm-execute-persistent-action))
  )

(use-package helm-grep
  :config
  (bind-keys :map helm-grep-mode-map
             ("RET" . helm-grep-mode-jump-other-window)
             ("n" . helm-grep-mode-jump-other-window-forward)
             ("p" . helm-grep-mode-jump-other-window-backward)))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  )

(use-package helm-projectile
  :ensure t
  :bind ("M-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on)
  )

(use-package helm-ag
  :ensure t
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"
	      )
  :config
  (when (executable-find "pyeverything")
    (setq helm-ag-base-command "pyeverything helm-ag")
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

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; start helm-mode
(use-package helm-mode
  :config
  (diminish 'helm-mode "")
  (helm-mode 1)
  )
