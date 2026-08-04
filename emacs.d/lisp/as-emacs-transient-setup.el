;;; as-emacs-transient-setup --- setup transient menus (replaces hydra) -*- lexical-binding: t; -*-
;;; Commentary:
;; Transient
;;; Code:
(use-package transient
  :ensure t)

(transient-define-prefix as-emacs-transient-avy ()
  "Avy jump commands."
  [["Line"
    ("y" "yank" avy-copy-line)
    ("m" "move" avy-move-line)
    ("k" "kill" avy-kill-whole-line)]
   ["Region"
    ("Y" "yank" avy-copy-region)
    ("M" "move" avy-move-region)
    ("K" "kill" avy-kill-region)]
   ["Goto"
    ("c" "timed char" avy-goto-char-timer)
    ("C" "char" avy-goto-char)
    ("w" "word" avy-goto-word-1)
    ("W" "any word" avy-goto-word-0)
    ("l" "line" avy-goto-line)
    ("L" "end of line" avy-goto-end-of-line)
    ("f" "char (2)" avy-goto-char-2)
    ("j" "resume" avy-resume)]])
(global-set-key (kbd "C-c a") #'as-emacs-transient-avy)

(transient-define-prefix as-emacs-transient-org ()
  "Org commands."
  ["Org"
   ("a" "agenda" org-agenda)
   ("l" "store link" org-store-link)
   ("c" "capture" org-capture)])
(global-set-key (kbd "C-c o") #'as-emacs-transient-org)

;; `:transient t' keeps the menu open after n/p, mirroring the old
;; `hydra-isearch' (:exit nil).
(transient-define-prefix as-emacs-transient-isearch ()
  "Repeat isearch."
  ["Isearch"
   ("n" "repeat forward" isearch-repeat-forward :transient t)
   ("p" "repeat backward" isearch-repeat-backward :transient t)])
(define-key isearch-mode-map (kbd "<f12>") #'as-emacs-transient-isearch)

;; Merges the old `search-replace' hydra with project-find-file. The old
;; hydra bound `C-c s' to a plain command, which clobbered the `C-c s'
;; prefix keymap that as-emacs-helm-setup.el/as-emacs-vertico-setup.el used
;; to set up for `C-c s f' -- both of those bindings are now removed, so
;; there's no longer a conflict; project-find-file lives here instead.
(transient-define-prefix as-emacs-transient-search ()
  "Search, replace, and project navigation."
  ["Search/Replace"
   ("s" "isearch" isearch-forward)
   ("r" "replace string" replace-string)
   ("R" "replace regexp" replace-regexp)
   ("o" "occur" as-menu-occur)
   ("O" "occur, multi-buffer" as-menu-occur-multi-buffer)]
  ["Project"
   ("f" "find file in project" project-find-file)
   ;; Mirrors `helm-fd-project' (still separately bound at `C-c h /' under
   ;; the helm stack); `consult-fd' has no other binding, this is its only
   ;; path under vertico.
   ("/" "find files (fd)" as-menu-find-files-fd)])
(global-set-key (kbd "C-c s") #'as-emacs-transient-search)

(transient-define-prefix as-emacs-transient-x-frame ()
  "Frame commands."
  ["Frame"
   ("1" "delete other frames" delete-other-frames)
   ("0" "delete frame" delete-frame)])

(transient-define-prefix as-emacs-transient-x ()
  "Buffer, window, and frame management."
  ["Buffer"
   ("b" "switch" as-menu-switch-buffer)
   ("B" "list" as-menu-list-buffers)
   ("f" "find file" as-menu-find-file)
   ("r" "recentf" as-menu-recentf)
   ("h" "mark whole buffer" mark-whole-buffer)
   ("k" "kill buffer" kill-buffer)
   ("s" "save buffer" save-buffer)
   ("c" "save buffers & kill emacs" save-buffers-kill-terminal)
   (";" "comment/uncomment" comment-or-uncomment-region)]
  ["Window"
   ("o" "other window" other-window)
   ("0" "delete window" delete-window)
   ("1" "delete other windows" delete-other-windows)
   ("2" "split below" split-window-below)
   ("3" "split right" split-window-right)]
  ["Frame"
   ("5" "frame..." as-emacs-transient-x-frame)]
  ;; Helm has its own action-selection built into `helm-map' already, so
  ;; there's no helm equivalent to dispatch to here -- just hide the group
  ;; entirely under that stack instead.
  ["Embark"
   :if #'as-emacs-vertico-p
   ("a" "act" embark-act)
   ("d" "dwim" embark-dwim)
   ("e" "export" embark-export)])
(global-set-key (kbd "C-c x") #'as-emacs-transient-x)

;; Vi-like navigation overlay for an active helm session, replacing the old
;; `helm-like-unite' (:color pink) hydra. `:transient t' on the nav/mark/
;; other suffixes matches pink's default non-exit behavior; `o' (quit) and
;; `i' (cancel, a `nil'-command head in the old hydra) stay exiting.
(with-eval-after-load 'helm
  (transient-define-prefix as-emacs-transient-helm-nav ()
    "Vi-like navigation within a helm session."
    ["Nav"
     ("h" "beginning" helm-beginning-of-buffer :transient t)
     ("j" "next" helm-next-line :transient t)
     ("k" "previous" helm-previous-line :transient t)
     ("l" "end" helm-end-of-buffer :transient t)
     ("g" "beginning" helm-beginning-of-buffer :transient t)
     ("G" "end" helm-end-of-buffer :transient t)
     ("K" "scroll other down" helm-scroll-other-window-down :transient t)
     ("J" "scroll other up" helm-scroll-other-window :transient t)]
    ["Mark"
     ("m" "toggle mark" helm-toggle-visible-mark :transient t)
     ("t" "toggle all" helm-toggle-all-marks :transient t)
     ("U" "unmark all" helm-unmark-all :transient t)
     ("d" "delete marked" helm-persistent-delete-marked :transient t)]
    ["Other"
     ("v" "view" helm-execute-persistent-action :transient t)
     ("H" "help" helm-help :transient t)
     ("f" "toggle follow" helm-follow-mode :transient t)
     ("}" "next source" helm-next-source :transient t)
     ("{" "prev source" helm-previous-source :transient t)]
    ["Quit"
     ("o" "quit" keyboard-escape-quit)
     ("i" "cancel" transient-quit-one)])
  (define-key helm-map (kbd "<f12>") #'as-emacs-transient-helm-nav))

(provide 'as-emacs-transient-setup)
;;; as-emacs-transient-setup.el ends here
