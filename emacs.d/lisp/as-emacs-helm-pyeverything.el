;;; as-emacs-helm-pyeverything.el --- helm interface for pyeverything command line tool. -*- lexical-binding: t -*-

;;; Code:
(require 'helm)
(require 'helm-types)

(declare-function ansi-color-apply "ansi-color.el")

(defvar helm-pyeverything-executable "pyeverything"
  "The pyeverything command executable.")

(defcustom helm-pyeverything-switches '("helm-files")
  "A list of options to pass to pyeverything command."
  :type '(repeat string)
  :group 'helm-files)

(defface helm-pyeverything-finish
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Green"))
  "Face used in mode line when pyeverything process ends."
  :group 'helm-grep-faces)

(defvar helm-pyeverything-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    map))

(defclass helm-pyeverything-class (helm-source-async)
  ((candidates-process :initform 'helm-pyeverything-process)
   (requires-pattern :initform 2)
   (candidate-number-limit :initform 20000)
   (nohighlight :initform t)
   (help-message :initform 'helm-pyeverything-help-message)
   (filtered-candidate-transformer :initform 'helm-pyeverything-fct)
   (action :initform 'helm-type-file-actions)
   (keymap :initform 'helm-pyeverything-map)))

(defun helm-pyeverything-process ()
  "Initialize pyeverything process in an helm async source."
  (let* (process-connection-type
         (cmd (append helm-pyeverything-switches (split-string helm-pattern " ")))
         (proc (apply #'start-process "pyeverything" nil helm-pyeverything-executable cmd))
         (start-time (float-time))
         (pyeverything-version (replace-regexp-in-string
				"\n" ""
				(shell-command-to-string (concat helm-pyeverything-executable " --version")))))
    (helm-log "pyeverything command:\npyeverything %s" (mapconcat 'identity cmd " "))
    (helm-log "VERSION: %s" pyeverything-version)
    (prog1
        proc
      (set-process-sentinel
       proc (lambda (_process event)
              (if (string= event "finished\n")
                  (with-helm-window
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished in %.2fs - (%s results)] "
                                     ,pyeverything-version
                                     ,(- (float-time) start-time)
                                     (helm-get-candidate-number))
                                    'face 'helm-pyeverything-finish))))
                    (force-mode-line-update))
                (helm-log "Error: pyeverything %s"
                          (replace-regexp-in-string "\n" "" event)
			  )
		)
	      )
       )
      )
    )
  )

(defun helm-pyeverything-fct (candidates _source)
  "The filtered-candidate-transformer function for helm-pyeverything."
  (cl-loop for i in candidates
           collect (ansi-color-apply i)))

(defun helm-pyeverything-1 (directory)
  "Run pyeverything command on DIRECTORY with helm interface."
  (cl-assert (executable-find helm-pyeverything-executable) nil "Could not find pyeverything executable")
  (cl-assert (not (file-remote-p directory)) nil "pyeverything not supported on remote directories")
  (helm-log "pyeverything (%s)" (abbreviate-file-name default-directory))
  (let ((default-directory directory))
    (helm :sources (helm-make-source
                       (format "pyeverything (%s)"
                               (abbreviate-file-name default-directory))
                       'helm-pyeverything-class)
          :input (thing-at-point 'word 'no-properties)
          :buffer "*helm pyeverything*")))

(defun helm-ff-run-pyeverything ()
  "Run pyeverything command action."
  (interactive)
  (setq helm-pyeverything-switches '("helm-files"))
  (helm-pyeverything-1 default-directory))

(defun helm-ag-run-pyeverything ()
  "Run pyeverything command action."
  (interactive)
  (setq helm-pyeverything-switches '("helm-ag"))
  (helm-pyeverything-1 default-directory))

(provide 'as-emacs-helm-pyeverything)

;;; as-emacs-helm-pyeverything.el ends here
