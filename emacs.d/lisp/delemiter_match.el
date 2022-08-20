(defun bc-bounce-cpp ()
(interactive)
(block nil
(let ((counter (save-excursion
(beginning-of-line)
(cond ((looking-at "^\s*#\s*if") (bc-bounce-cpp-if))
((looking-at "^\s*#\s*endif") (bc-bounce-cpp-endif))
(t (message "Not on a preprocessor conditional")
(return))))))
(if (eq counter 0)
(goto-char (match-beginning 0))
(message "Unbalanced conditional")))))

(defun bc-bounce-cpp-if ()
(let ((counter 1)
(limit (1+ (save-excursion
(goto-char (point-max))
(or (re-search-backward "^[ \t]**#[ \t]*endif" nil t 1)
(point))))))
(next-line)
(while (not (or (eq (point) limit) (eq counter 0)))
(re-search-forward "^\s*#" nil t 1)
(setq counter (cond ((looking-at "if")
(1+ counter))
((looking-at "endif")
(1- counter))
(t counter)))) ;eg, else
(re-search-backward "^\s*#" nil t 1) ; return to bol
counter))

(defun bc-bounce-cpp-endif ()
(let ((counter 1)
(limit (1+ (save-excursion
(goto-char (point-min))
(or (re-search-forward "^[ \t]**#[ \t]*if" nil t 1)
(point))))))
;;(forward-line -1)
(while (not (or (eq (point) limit) (eq counter 0)))
(re-search-backward "^\s*#" nil t 1)
(setq counter (cond ((looking-at "#endif")
(1+ counter))
((looking-at "#if")
(1- counter))
(t counter)))) ;eg, else
counter))

(defun ben-bounce-sexp ()
"Bounce between matching parens."
(interactive)
(let ((pc (char-to-string (preceding-char)))
(fc (char-to-string (following-char))))
(cond ((string-match "[[{(<]" fc)
(forward-sexp 1))

((string-match "[\]})>]" pc)
(backward-sexp 1))

((string-match "[[{(<]" pc)
(backward-char)
(forward-sexp 1)
(backward-char))

((string-match "[\]})>]" fc)
(forward-char)
(backward-sexp 1)
(forward-char))

(t (message "Not on a paren, brace, or bracket")))))

(defadvice ben-bounce-sexp (around do-bc-bounce-cpp activate)
"In CC modes, try bc-bounce-cpp if ben-bounce-sexp didn't match anything."
(let ((there (point)))
ad-do-it
(when (and (eq there (point))
c-buffer-is-cc-mode
(save-excursion (beginning-of-line)
(eq (char-after) ?#)))
(bc-bounce-cpp))))

