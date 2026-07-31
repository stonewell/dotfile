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
    ("C-c M-o" . helm-multi-occur)
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

(defvar as-emacs-helm-grep-match-selection-overlay nil
  "Overlay keeping `helm-grep-match' visible over `helm-selection-overlay'.")

(defun as-emacs-helm--face-has-p (pos face)
  "Non-nil if the `face' text property at POS is or contains FACE.
`add-face-text-property' (used by `helm-grep-highlight-match') stores
faces as a list consed onto the existing value, not a bare symbol, so a
plain `eq'/`text-property-any' check against FACE never matches."
  (let ((val (get-text-property pos 'face)))
    (or (eq val face) (and (listp val) (memq face val)))))

(defun as-emacs-helm-highlight-selected-match ()
  "Re-apply `helm-grep-match' over the match text on the selected line."
  (when (overlayp helm-selection-overlay)
    (unless as-emacs-helm-grep-match-selection-overlay
      (setq as-emacs-helm-grep-match-selection-overlay
        (make-overlay (point-min) (point-min)))
      (overlay-put as-emacs-helm-grep-match-selection-overlay 'priority 2)
      (overlay-put as-emacs-helm-grep-match-selection-overlay 'face 'helm-grep-match))
    (let* ((beg (overlay-start helm-selection-overlay))
            (end (overlay-end helm-selection-overlay))
            (pos beg)
            match-beg)
      (while (and pos (< pos end) (not match-beg))
        (if (as-emacs-helm--face-has-p pos 'helm-grep-match)
          (setq match-beg pos)
          (setq pos (next-single-property-change pos 'face nil end))))
      ;; Explicit BUFFER arg: without it, `move-overlay' re-homes a
      ;; previously-deleted overlay into whatever buffer it was *originally*
      ;; created in, which silently breaks this if helm ever recreates the
      ;; "*helm RG*" buffer object between sessions instead of reusing it.
      (if match-beg
        (move-overlay as-emacs-helm-grep-match-selection-overlay
          match-beg (or (next-single-property-change match-beg 'face nil end) end)
          (current-buffer))
        (move-overlay as-emacs-helm-grep-match-selection-overlay 1 1 (current-buffer))))))

(add-hook 'helm-move-selection-after-hook #'as-emacs-helm-highlight-selected-match)
(add-hook 'helm-after-update-hook #'as-emacs-helm-highlight-selected-match)

(when (or (executable-find "rg") (executable-find "ag"))
  (bind-keys ("M-p" . helm-do-grep-ag-project)))

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
