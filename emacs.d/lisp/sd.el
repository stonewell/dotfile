(defun sd-edit-file ()
  "*Use sd to edit file, call sd edit <buffer name>."
  (interactive)
  (setq fname (or buffer-file-name
		   dired-directory))
  (setq cmd "sd edit")
  (message 
    (shell-command-to-string 
      (concat cmd " " fname)))
  (find-alternate-file fname))

(defun sd-revert-file ()
  "*Use sd to revert file, call sd revert <buffer name>."
  (interactive)
  (setq fname (or buffer-file-name
		   dired-directory))
  (setq cmd "sd revert")
  (message 
    (shell-command-to-string 
      (concat cmd " " fname)))
  (find-alternate-file fname))

(defun sd-revert-all ()
  "*Use sd to revert all unchanged file, call sd revert -a"
  (interactive)
  (setq cmd "sd revert -a")
  (message 
    (shell-command-to-string 
      cmd))
  )
