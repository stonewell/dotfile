;;; as-emacs-vertico-setup --- setup the vertico ("minad stack") completion stack  -*- lexical-binding: t; -*-
;;; Commentary:
;; Vertico + Consult + Corfu + Marginalia + Cape + Orderless + Embark
;; An alternative to as-emacs-helm-setup.el. Only one of the two stacks is
;; ever loaded, dispatched by `as-emacs-completion-stack' in
;; as-emacs-setup.el, so both freely rebind the same global keys.
;;
;; Parity with as-emacs-helm-setup.el's `(define-key global-map [remap ...])'
;; bindings:
;;   [remap find-file]               -- not remapped; find-file's own
;;                                      read-file-name prompt is already
;;                                      enhanced in place by Vertico.
;;   [remap occur]                   -> consult-line (below)
;;   [remap list-buffers]            -> consult-buffer (below)
;;   [remap dabbrev-expand]          -> cape-dabbrev (below)
;;   [remap execute-extended-command] -- not remapped; M-x's own
;;                                      read-extended-command prompt is
;;                                      already enhanced in place by Vertico.
;;   [remap apropos-command]         -- intentionally left unmapped: Consult's
;;                                      former `consult-apropos' was
;;                                      deprecated with no replacement, so
;;                                      there is no minad-stack equivalent.
;;; Code:

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
    vertico-count 15)
  ;; Ctrl-l style directory-segment deletion in find-file style prompts.
  (require 'vertico-directory)
  (bind-keys :map vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word))
  )

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles basic partial-completion orderless))))
  )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )

;; start-of consult
;; :demand t so the :config below (remaps, prefix map) runs at startup rather
;; than waiting for one of the specific :bind keys to be pressed first --
;; otherwise e.g. testing list-buffers/occur before ever pressing C-x b or
;; M-o would find the remap not yet installed.
(use-package consult
  :ensure t
  :demand t
  :bind
  (("M-y" . consult-yank-pop)
    ("C-x b" . consult-buffer)
    ("C-x C-r" . recentf-open) ;; vanilla, Vertico-enhanced; mirrors helm-recentf
    ("M-o" . consult-line)
    ("C-c M-o" . consult-line-multi)
    )
  :config
  ;; Default `consult-preview-key' is `any', which opens/jumps to a
  ;; candidate's buffer on every single up/down move -- slow when quickly
  ;; scrolling through consult-line/consult-ripgrep/consult-buffer results,
  ;; especially for large or remote files. Debounce so a preview only fires
  ;; once the selection has settled for a moment.
  (setq consult-preview-key '(:debounce 0.3 any))

  (when (executable-find "rg")
    (defun as-emacs-consult-ripgrep-symbol-at-point ()
      "Run `consult-ripgrep', pre-filled with the symbol at point."
      (interactive)
      (consult-ripgrep nil (thing-at-point 'symbol)))
    (bind-keys
      ("M-p" . as-emacs-consult-ripgrep-symbol-at-point)
      )
    )

  (bind-keys :prefix-map as-emacs-vertico-prefix-map
    :prefix "C-c s"
    ("f" . project-find-file) ;; mirrors helm-browse-project
    )

  ;; list-buffers and occur both open a dedicated results buffer rather than
  ;; acting through a plain completing-read, so Vertico doesn't enhance them
  ;; in place -- remap explicitly, mirroring helm-setup's
  ;; [remap list-buffers] -> helm-buffers-list and [remap occur] -> helm-occur.
  (define-key global-map [remap list-buffers] 'consult-buffer)
  (define-key global-map [remap occur] 'consult-line)
  )
;; end-of consult

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto t
    corfu-cycle t
    corfu-auto-delay 0.1
    corfu-auto-prefix 2
    corfu-quit-no-match 'separator)
  )

;; :demand t -- this use-package has no :bind/:hook of its own to trigger a
;; lazy load, so without :demand its :config (completion-at-point-functions,
;; dabbrev-expand remap) would never run at all.
(use-package cape
  :ensure t
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; cape-dabbrev doubles as an interactive command, so it stands in for
  ;; helm-setup's [remap dabbrev-expand] -> helm-dabbrev.
  (define-key global-map [remap dabbrev-expand] 'cape-dabbrev)
  )

;; contextual actions on the candidate at point/in the minibuffer.
;; NOTE: as-emacs-keys.el already globally binds the conventional Embark keys
;; (C-. -> push-mark-command, C-; -> backward-char), so Embark is bound under
;; its own C-c e prefix instead (discoverable via which-key-mode).
(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-c e a" . embark-act)
    ("C-c e d" . embark-dwim)
    ("C-c e e" . embark-export)
    )
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  )

;; Loaded eagerly (rather than :after (embark consult)) so it's always
;; present by the time either embark or consult lazily loads on first use --
;; otherwise whichever loads first warns that embark-consult is missing.
(use-package embark-consult
  :ensure t
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'as-emacs-vertico-setup)
;;; as-emacs-vertico-setup.el ends here
