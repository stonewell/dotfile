;;; as-emacs-org-setup ------- config org mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :quelpa (org
	   :fetcher git
	   :url "https://code.orgmode.org/bzg/org-mode.git"
	   :files ("lisp/*.el" "contrib/lisp/*.el" "doc/dir" "doc/*.texi")
	   )
  :ensure t
  :init
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post. See `org-capture-templates' for more information."
    (let* (
           (date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: "  ":summary \"summary\"")
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n"))
    )
  :config
  (setq org-capture-templates
	'(
	  ("h" "Hugo post")
	  ("hg" "OpenGL"
	   entry (file+olp "blog/opengl.org" "OpenGL")
	   (function org-hugo-new-subtree-post-capture-template)
	   :clock-in t :clock-resume t)
	  ("hl" "Life"
	   entry (file+olp "blog/life.org" "Life")
	   (function org-hugo-new-subtree-post-capture-template)
	   :clock-in t :clock-resume t)
	  ("ho" "Other"
	   entry (file+olp "blog/other.org" "Other")
	   (function org-hugo-new-subtree-post-capture-template)
	   :clock-in t :clock-resume t)
	  ("hd" "Programming"
	   entry (file+olp "blog/programming.org" "Programming")
	   (function org-hugo-new-subtree-post-capture-template)
	   :clock-in t :clock-resume t)
	  ("hc" "Computer"
	   entry (file+olp "blog/computer.org" "Computer")
	   (function org-hugo-new-subtree-post-capture-template)
	   :clock-in t :clock-resume t)
	  )
	)
  )

(use-package ox-hugo
  :ensure t
  :after org
  )


(provide 'as-emacs-org-setup)
;;; as-emacs-org-setup ends here
