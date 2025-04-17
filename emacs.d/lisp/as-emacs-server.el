;; -*- lexical-binding: t; -*-
(defun intelligent-make-frame ()
  "Create and raise a frame when their is no frame exists
   reference to  http://www.elliotglaysher.org/emacs/."
  ;;(interactive)
  (if (< (length (visible-frame-list)) 100)
  	;;create and select the new frame
        ((select-frame (make-frame))
	(raise-frame (selected-frame)))
  )
)
