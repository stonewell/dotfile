;;; as-emacs-hydra-setup --- setup hydra -*- lexical-binding: t -*-
;;; Commentary:
;; Hydra
;;; Code:
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
    ("a" org-agenda)
    ("l" org-store-link)
    ("c" org-capture)
    )

  (defhydra hydra-isearch
    (
      :exit nil
      )
    ("n" isearch-repeat-forward)
    ("p" isearch-repeat-backward)
    )
  (define-key isearch-mode-map (kbd "<f12>") 'hydra-isearch/body)

  (defhydra search-replace
    (
      global-map "C-c s"
      :exit t
      :hint nil)
    ("s" isearch-forward)
    ("r" replace-string)
    ("R" replace-regexp)
    ("o" helm-occur)
    )

  (defhydra vi-navi
    (
      global-map "C-c"
      :timeout 2)
    "vi navigate"
    ("d" delete-char)
    ("e" move-end-of-line)
    ("^" move-beginning-of-line)
    ("w" whole-line-or-region-kill-region)
    ("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("J" join-line)
    ("k" previous-line)
    ("n" (scroll-up-command))
    ("p" (scroll-up-command '-))
    ("SPC" set-mark-command)
    ("u" undo)
    ("y" yank)
    ("i" nil "quit")
    ("q" nil "quit")
    )

  ;; ------------------------ ;;
  ;;  vi-style bindings ;;
  ;; ------------------------ ;;
  (defhydra vi
    (
      global-map "C-c v"
      :body-pre (progn
                  (message "Enter hydra-vi")
                  )
      :post (progn
              (message "Leave hydra-vi")))
    "vi"
    ("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("J" join-line)
    ("k" previous-line)
    ("K" kill-buffer)
    ("n" (scroll-up-command))
    ("p" (scroll-up-command '-))
    ("f" forward-word)
    ("b" backward-word)
    ("x" delete-char)
    ("e" move-end-of-line)
    ("^" move-beginning-of-line)
    ("d" whole-line-or-region-kill-region)
    ("D" kill-rectangle)
    ("w" whole-line-or-region-copy-region-as-kill)
    ("W" copy-rectangle-as-kill)
    ("s" (progn (exchange-point-and-mark) (activate-mark)))
    ("o" qjp-open-new-line)
    ("O" (qjp-open-new-line 1))
    ("y" yank)
    ("Y" helm-show-kill-ring)
    ("u" undo)
    ("v" set-mark-command)
    ("V" rectangle-mark-mode)
    ("g" avy-goto-word-1)
    ("G" avy-goto-char-in-line)
    ("M-g" avy-goto-line)
    ("SPC f" helm-find-files)
    ("SPC b" (progn (hydra-keyboard-quit)
               (helm-mini)))
    ("SPC s" save-buffer)
    ("i" nil "quit")
    ("q" nil "quit"))

  (defhydra x-5
    (
      :exit t
      )
    ("1" delete-other-frames)
    ("0" delete-frame)
    )

  (defhydra x
    (
      global-map "C-c x"
      :exit t
      )
    ("b" helm-mini)
    ("B" helm-buffers-list)
    ("c" save-buffers-kill-terminal)
    ("f" helm-find-files)
    ("o" other-window)
    ("r" helm-recentf)
    ("s" save-buffer)
    ("0" delete-windows)
    ("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)
    ("5" x-5/body "frame")
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

  (define-key helm-map (kbd "<f12>") 'helm-like-unite/body)
  )

(provide 'as-emacs-hydra-setup)
;;; as-emacs-hydra-setup.el ends here
