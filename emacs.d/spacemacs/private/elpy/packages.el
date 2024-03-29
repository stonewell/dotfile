;;; packages.el --- elpy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(setq elpy-packages
      '(
        elpy
        flycheck
        ))

(setq elpy-excluded-packages '())

(defun elpy/init-elpy ()
  (use-package elpy
               :diminish elpy-mode
               :config

               ;; Elpy removes the modeline lighters. Let's override this
               (defun elpy-modules-remove-modeline-lighter (mode-name))

               (setq elpy-modules '(elpy-module-sane-defaults
                                     elpy-module-eldoc
                                     elpy-module-pyvenv))
               :init
               (elpy-enable)
               )
  )

(defun elpy/post-init-flycheck ()
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )
