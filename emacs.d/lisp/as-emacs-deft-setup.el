;;; as-emacs-deft-setup ------- config org mode
;;; Commentary:
;;; Code:

(use-package deft
  :ensure t
  :commands (deft)
  :bind (
	 ("<f8>" . deft)
	 :map deft-mode-map
	 ("C-p" . widget-backward)
	 ("C-n" . widget-forward)
	 )
  :config
  (setq my-org-directory "~/org")
  (setq deft-directory my-org-directory)
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((nospace . "-")))
  :init
  (defun my-deft/strip-quotes (str)
    (cond ((string-match "\"\\(.+\\)\"" str) (match-string 1 str))
	  ((string-match "'\\(.+\\)'" str) (match-string 1 str))
	  (t str)))

  (defun my-deft/parse-title-from-front-matter-data (str)
    (if (string-match "^title: \\(.+\\)" str)
	(let* ((title-text (my-deft/strip-quotes (match-string 1 str)))
	       (is-draft (string-match "^draft: true" str)))
	  (concat (if is-draft "[DRAFT] " "") title-text))))

  (defun my-deft/deft-file-relative-directory (filename)
    (file-name-directory (file-relative-name filename deft-directory)))

  (defun my-deft/title-prefix-from-file-name (filename)
    (let ((reldir (my-deft/deft-file-relative-directory filename)))
      (if reldir
	  (concat (directory-file-name reldir) " > "))))

  (defun my-deft/parse-title-with-directory-prepended (orig &rest args)
    (let ((str (nth 1 args))
	  (filename (car args)))
      (concat
       (my-deft/title-prefix-from-file-name filename)
       (let ((nondir (file-name-nondirectory filename)))
	 (if (or (string-prefix-p "README" nondir)
		 (string-suffix-p ".txt" filename))
	     nondir
	   (if (string-prefix-p "---\n" str)
	       (my-deft/parse-title-from-front-matter-data
		(car (split-string (substring str 4) "\n---\n")))
	     (apply orig args)))))))

  (advice-add 'deft-parse-title :around #'my-deft/parse-title-with-directory-prepended)
  )


(provide 'as-emacs-deft-setup)
;;; as-emacs-deft-setup ends here
