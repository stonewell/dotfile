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

  ;; list-buffers and occur both open a dedicated results buffer rather than
  ;; acting through a plain completing-read, so Vertico doesn't enhance them
  ;; in place -- remap explicitly, mirroring helm-setup's
  ;; [remap list-buffers] -> helm-buffers-list and [remap occur] -> helm-occur.
  (define-key global-map [remap list-buffers] 'consult-buffer)
  (define-key global-map [remap occur] 'consult-line)
  )
;; end-of consult

;; Vertico-stack counterpart to helm's `helm-command-map'/`helm-command-prefix'
;; (see helm-global-bindings.el), bound at `C-c h' by
;; as-emacs-hydra-setup.el/as-emacs-transient-setup.el. There's no single
;; pre-built equivalent package -- this hand-picks the closest consult or
;; vanilla (Vertico-enhanced) command for each `helm-command-map' entry that
;; has one; entries already bound elsewhere in this file, or with no real
;; equivalent (helm-surfraw, helm-select-xfont, helm-google-suggest,
;; helm-run-external-command, helm-resume, helm-gid, helm-info-gnus,
;; helm-multi-files, helm-lisp-completion-at-point), are omitted.
(defvar as-emacs-vertico-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'apropos)
    (define-key map (kbd "e") 'xref-find-apropos)
    (when (executable-find "locate")
      (define-key map (kbd "l") 'consult-locate))
    (define-key map (kbd "L") 'locate-library)
    (define-key map (kbd "r") 're-builder)
    (define-key map (kbd "m") 'man)
    (define-key map (kbd "t") 'proced)
    (define-key map (kbd "o") 'consult-outline)
    (when (executable-find "find")
      (define-key map (kbd "/") 'consult-find))
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "I") 'consult-imenu-multi)
    (define-key map (kbd "p") 'list-processes)
    (define-key map (kbd "C-x r b") 'consult-bookmark)
    (define-key map (kbd "C-c <SPC>") 'consult-global-mark)
    (define-key map (kbd "C-:") 'eval-expression)
    (define-key map (kbd "C-,") 'quick-calc)
    (define-key map (kbd "M-g a") 'consult-ripgrep)
    (define-key map (kbd "c") 'list-colors-display)
    (define-key map (kbd "8") 'insert-char)
    (define-key map (kbd "h i") 'info-lookup-symbol)
    (define-key map (kbd "h r") 'info-emacs-manual)
    (define-key map (kbd "h h") 'info)
    (define-key map (kbd "C-x r i") 'consult-register)
    (define-key map (kbd "@") 'list-packages)
    (define-key map (kbd "h p") 'finder-by-keyword)
    map)
  "Vertico-stack command palette, parallel to `helm-command-map'.")
(fset 'as-emacs-vertico-command-map as-emacs-vertico-command-map)

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
;; (C-. -> push-mark-command, C-; -> backward-char), so Embark's own commands
;; aren't bound to a dedicated prefix here -- they're reachable from the
;; "Embark" group in as-emacs-transient-x/the `x' hydra instead.
(use-package embark
  :ensure t
  :demand t
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
