;;; as-emacs-hydra-setup --- setup hydra -*- lexical-binding: t -*-
;;; Commentary:
;; Hydra
;;; Code:

;; The completion-stack dispatch heads below (occur, find-file, etc.) use
;; the shared `as-menu-*' commands from `as-emacs-funcs-setup' -- those are
;; reused by `as-emacs-transient-setup' too, so they live somewhere loaded
;; regardless of which menu stack is active.

;; Helm has its own action-selection built into `helm-map' already, so
;; there's no helm equivalent to Embark -- message instead of erroring.
;; These stay hydra-specific since the transient stack instead hides the
;; whole Embark group under the helm stack via a group-level `:if'.
(defun as-hydra-embark-act ()
  "Embark act; only available under the vertico stack."
  (interactive)
  (if (as-emacs-vertico-p)
    (embark-act)
    (message "Embark is only available under the vertico stack")))

(defun as-hydra-embark-dwim ()
  "Embark dwim; only available under the vertico stack."
  (interactive)
  (if (as-emacs-vertico-p)
    (embark-dwim)
    (message "Embark is only available under the vertico stack")))

(defun as-hydra-embark-export ()
  "Embark export; only available under the vertico stack."
  (interactive)
  (if (as-emacs-vertico-p)
    (embark-export)
    (message "Embark is only available under the vertico stack")))

(use-package hydra
  :ensure t
  :config

  (defhydra avy
    (
      global-map "C-c a"
      :exit t
      :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region)
    ("f" avy-goto-char-2)
    ("j" avy-resume)
    )

  (defhydra org
    (
      global-map "C-c o"
      :exit t
      :hint nil)
    ("a" org-agenda "agenda")
    ("l" org-store-link "store link")
    ("c" org-capture "capture")
    )

  (defhydra hydra-isearch
    (
      :exit nil
      )
    ("n" isearch-repeat-forward "repeat forward")
    ("p" isearch-repeat-backward "repeat backward")
    )
  (define-key isearch-mode-map (kbd "<f12>") 'hydra-isearch/body)

  (defhydra search-replace
    (
      global-map "C-c s"
      :exit t
      :hint nil)
    ("s" isearch-forward "isearch")
    ("r" replace-string "replace string")
    ("R" replace-regexp "replace regexp")
    ("o" as-menu-occur "occur")
    ("O" as-menu-occur-multi-buffer "occur, multi-buffer")
    ;; `project-find-file' just uses a plain `completing-read', which
    ;; `helm-mode'/vertico already redirect through whichever stack is
    ;; active -- no need to dispatch to `helm-browse-project' here, which
    ;; is much heavier (VCS detection, dual buffer/file sources) and can
    ;; feel like a hang on a large repo.
    ("f" project-find-file "find file in project")
    ("/" as-menu-find-files-fd "find files (fd)")
    )

  (defhydra x-5
    (
      :exit t
      )
    ("1" delete-other-frames "delete other frames")
    ("0" delete-frame "delete frame")
    )

  (defhydra x
    (
      global-map "C-c x"
      :exit t
      )
    ("b" as-menu-switch-buffer "switch")
    ("B" as-menu-list-buffers "list")
    ("c" save-buffers-kill-terminal "save buffers & kill emacs")
    ("f" as-menu-find-file "find file")
    ("h" mark-whole-buffer "mark whole buffer")
    ("k" kill-buffer "kill buffer")
    ("o" other-window "other window")
    ("r" as-menu-recentf "recentf")
    ("s" save-buffer "save buffer")
    (";" comment-or-uncomment-region "comment/uncomment")
    ("0" delete-window "delete window")
    ("1" delete-other-windows "delete other windows")
    ("2" split-window-below "split below")
    ("3" split-window-right "split right")
    ("5" x-5/body "frame")
    ("a" as-hydra-embark-act "act")
    ("d" as-hydra-embark-dwim "dwim")
    ("e" as-hydra-embark-export "export")
    )

  (defhydra helm-like-unite (:hint nil
                              :color pink)
    "
Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
^^^^^^^^^^------------^^----------------^^----------------------
_K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
_J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
"
    ;; arrows
    ("h" helm-beginning-of-buffer)
    ("j" helm-next-line)
    ("k" helm-previous-line)
    ("l" helm-end-of-buffer)
    ;; beginning/end
    ("g" helm-beginning-of-buffer)
    ("G" helm-end-of-buffer)
    ;; scroll
    ("K" helm-scroll-other-window-down)
    ("J" helm-scroll-other-window)
    ;; mark
    ("m" helm-toggle-visible-mark)
    ("t" helm-toggle-all-marks)
    ("U" helm-unmark-all)
    ;; exit
    ("<escape>" keyboard-escape-quit "" :exit t)
    ("o" keyboard-escape-quit :exit t)
    ("i" nil)
    ;; sources
    ("}" helm-next-source)
    ("{" helm-previous-source)
    ;; rest
    ("H" helm-help)
    ("v" helm-execute-persistent-action)
    ("d" helm-persistent-delete-marked)
    ("f" helm-follow-mode))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "<f12>") 'helm-like-unite/body))

  )

;; Helm's command palette (or its vertico-stack equivalent, see
;; as-emacs-vertico-setup.el) -- not a hydra itself, but lives here so `C-c'
;; is fully accounted for regardless of `as-emacs-completion-stack'.
(global-set-key (kbd "C-c h")
  (if (as-emacs-vertico-p) 'as-emacs-vertico-command-map 'helm-command-prefix))

(provide 'as-emacs-hydra-setup)
;;; as-emacs-hydra-setup.el ends here
