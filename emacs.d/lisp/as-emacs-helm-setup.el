;;; as-emacs-helm-setup --- setup helm  -*- lexical-binding: t; -*-
;;; Commentary:
;; Helm
;;; Code:

(use-package helm
  :ensure t
  :after (bind-key)
  :config
  (setq
    helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
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
    ("M-o" . helm-occur)
    )
  (bind-keys :map helm-map
    ("C-o" . nil)
    ("TAB" . helm-execute-persistent-action)
    ("C-i" . helm-execute-persistent-action)
    ("C-z" . helm-select-action)
    ("C-h" . delete-backward-char))

  (when (executable-find "pyeverything")
    (progn
      (require 'as-emacs-helm-pyeverything)

      (bind-keys ("C-t" . helm-ff-run-pyeverything)
        ("C-M-t" . helm-ag-run-pyeverything)
        )
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

;; `helm-grep-ag-command' (part of helm core, actively maintained at
;; emacs-helm/helm) already prefers ripgrep over ag automatically, falling
;; back to ag if rg isn't installed -- so `helm-do-grep-ag-project' (also
;; helm core, project-root-aware via `project'/projectile) gives the same
;; incremental, helm-native candidate navigation `helm-ag'/`helm-rg' did,
;; with the actual search tool underneath chosen by helm itself.

;; `helm-grep--filter-candidate-1' only applies its own
;; `helm-grep-file'/`helm-grep-lineno'/`helm-grep-match' faces when the
;; backend output carries no ANSI codes -- with `--color=always' it defers
;; to ripgrep's own raw ANSI instead and `helm-grep-match' never gets
;; applied at all. Use `--color=never' so helm's faces are what's actually
;; used (see as-emacs-setup-font-color-theme.el for their colors).
(setq helm-grep-ag-command
  "rg --color=never --smart-case --search-zip --no-heading --line-number %s -- %s %s")

;; `helm-grep-ag-init's own sentinel calls `with-helm-window' when the rg
;; process finishes, which signals "Wrong type argument: window-live-p,
;; nil" if the helm session was already exited/cancelled before the async
;; process wrapped up -- a known upstream race (see the process-connection-
;; type comment in `helm-grep-ag-init', only ever patched for macOS).
;; Silence just that race rather than patching helm's sentinel outright.
(advice-add 'helm-grep-ag-init :around
  (lambda (orig-fn &rest args)
    (let ((proc (apply orig-fn args)))
      (when (processp proc)
        (let ((sentinel (process-sentinel proc)))
          (when sentinel
            (set-process-sentinel proc
              (lambda (p e) (ignore-errors (funcall sentinel p e)))))))
      proc)))

(defvar as-emacs-helm-grep-match-selection-overlays nil
  "Overlays keeping `helm-grep-match' visible over `helm-selection-overlay'.
One per match span on the selected line -- a line can contain more than
one occurrence of the search pattern.")

(defun as-emacs-helm--face-has-p (pos face)
  "Non-nil if the `face' text property at POS is or contains FACE.
`add-face-text-property' (used by `helm-grep-highlight-match') stores
faces as a list consed onto the existing value, not a bare symbol, so a
plain `eq'/`text-property-any' check against FACE never matches."
  (let ((val (get-text-property pos 'face)))
    (or (eq val face) (and (listp val) (memq face val)))))

(defun as-emacs-helm-highlight-selected-match ()
  "Re-apply `helm-grep-match' over every match span on the selected line."
  (mapc #'delete-overlay as-emacs-helm-grep-match-selection-overlays)
  (setq as-emacs-helm-grep-match-selection-overlays nil)
  (when (overlayp helm-selection-overlay)
    (let ((pos (overlay-start helm-selection-overlay))
           (end (overlay-end helm-selection-overlay)))
      (while (and pos (< pos end))
        (if (as-emacs-helm--face-has-p pos 'helm-grep-match)
          (let* ((match-end (or (next-single-property-change pos 'face nil end) end))
                  (ov (make-overlay pos match-end (current-buffer))))
            (overlay-put ov 'priority 2)
            (overlay-put ov 'face 'helm-grep-match)
            (push ov as-emacs-helm-grep-match-selection-overlays)
            (setq pos match-end))
          (setq pos (next-single-property-change pos 'face nil end)))))))

(add-hook 'helm-move-selection-after-hook #'as-emacs-helm-highlight-selected-match)
(add-hook 'helm-after-update-hook #'as-emacs-helm-highlight-selected-match)

(when (or (executable-find "rg") (executable-find "ag"))
  (bind-keys ("M-p" . helm-do-grep-ag-project)))

(when (facep 'helm-selection)
  (set-face-attribute 'helm-selection nil :weight 'bold))
(when (facep 'helm-grep-file)
  (set-face-attribute 'helm-grep-file nil :foreground "DarkTurquoise" :underline t))


;; Lets helm-grep-mode results (from helm-do-grep-ag et al.) be exported to
;; an editable buffer.
(use-package wgrep
  :ensure t
  :demand t)

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
    (let* ((proj (project-current))
           (directory (if proj
                          (project-root proj)
                        (with-current-buffer "*scratch*" default-directory))))
      (helm-fd-1 directory))))

;; `helm-command-map' (bound above via helm-fd's :map) isn't reachable from
;; any key on its own -- traditionally `(require 'helm-config)' binds it to
;; `C-c h', but this config never requires that file. Bind it directly.
(global-set-key (kbd "C-c h") helm-command-map)

;; start helm-mode
(use-package helm-mode
  :config
  (diminish 'helm-mode "")
  (helm-mode 1)
;;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
  )

(use-package helm-tramp
  :ensure t
  :defer t
  :config
  (setq helm-tramp-control-master t)
  )

(provide 'as-emacs-helm-setup)
;;; as-emacs-helm-setup.el ends here
