;;; cc-cmds.el --- user level commands for CC Mode 

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Authors:  1992-1997 Barry A. Warsaw
;;           1987 Dave Detlefs and Stewart Clamen
;;           1985 Richard M. Stallman
;; Created:  22-Apr-1997 (split from cc-mode.el)
;; Version:  5.00
;; Keywords: c languages oop

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



;; Useful helpers

(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in
  ;; XEmacs. ignore byte-compiler warnings you might see.  This is not
  ;; needed for Emacs.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  (setq c-auto-hungry-string
	(if c-auto-newline
	    (if c-hungry-delete-key "/ah" "/a")
	  (if c-hungry-delete-key "/h" nil)))
  (force-mode-line-update))

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))


;; Auto-newline and hungry-delete

(defun c-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a' or
`/ah' on the modeline after the mode name) newlines are automatically
inserted after special characters such as brace, comma, semi-colon,
and colon."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h' or `/ah' on the modeline after the mode name) the delete key
gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `c-toggle-auto-state' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))


;; Electric keys

(defun c-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-delete-function' is called."
  (interactive "P")
  (if (or (not c-hungry-delete-key)
	  arg
	  (c-in-literal))
      (funcall c-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c-delete-function 1)
	))))

(defun c-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "P")
  (if (or (c-in-literal)
	  arg
	  (not (memq 'alignleft c-electric-pound-behavior)))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-electric-brace (arg)
  "Insert a brace.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after braces based on the value of `c-hanging-braces-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the brace, or
the brace is inserted inside a literal."
  (interactive "P")
  (let* ((c-state-cache (c-parse-state))
	 (safepos (c-safe-position (point) c-state-cache))
	 (literal (c-in-literal safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal arg
;	    (not c-auto-newline)
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      (let* ((syms '(class-open class-close defun-open defun-close 
		     inline-open inline-close brace-list-open brace-list-close
		     brace-list-intro brace-list-entry block-open block-close
		     substatement-open statement-case-open
		     extern-lang-open extern-lang-close))
	    ;; we want to inhibit blinking the paren since this will
	    ;; be most disruptive. we'll blink it ourselves later on
	    (old-blink-paren (if (boundp 'blink-paren-function)
				 blink-paren-function
			       blink-paren-hook))
	    blink-paren-function	; emacs19
	    blink-paren-hook		; emacs18
	    (insertion-point (point))
	    delete-temp-newline
	    (preserve-p (= 32 (char-syntax (preceding-char))))
	    ;; shut this up too
	    (c-echo-syntactic-information-p nil)
	    (syntax (progn
		      ;; only insert a newline if there is
		      ;; non-whitespace behind us
		      (if (save-excursion
			    (skip-chars-backward " \t")
			    (not (bolp)))
			  (progn (newline)
				 (setq delete-temp-newline t)))
		      (self-insert-command (prefix-numeric-value arg))
		      ;; state cache doesn't change
		      (c-guess-basic-syntax)))
	    (newlines (and
		       c-auto-newline
		       (or (c-lookup-lists syms syntax c-hanging-braces-alist)
			   '(ignore before after)))))
	;; If syntax is a function symbol, then call it using the
	;; defined semantics.
	(if (and (not (consp (cdr newlines)))
		 (c-functionp (cdr newlines)))
	    (let ((c-syntactic-context syntax))
	      (setq newlines
		    (funcall (cdr newlines) (car newlines) insertion-point))))
	;; does a newline go before the open brace?
	(if (memq 'before newlines)
	    ;; we leave the newline we've put in there before,
	    ;; but we need to re-indent the line above
	    (let ((pos (- (point-max) (point)))
		  (here (point))
		  (c-state-cache c-state-cache))
	      (forward-line -1)
	      ;; we may need to update the cache. this should still be
	      ;; faster than recalculating the state in many cases
	      (save-excursion
		(save-restriction
		  (narrow-to-region here (point))
		  (if (and (c-safe (progn (backward-up-list -1) t))
			   (memq (preceding-char) '(?\) ?}))
			   (progn (widen)
				  (c-safe (progn (forward-sexp -1) t))))
		      (setq c-state-cache
			    (c-hack-state (point) 'open c-state-cache))
		    (if (and (car c-state-cache)
			     (not (consp (car c-state-cache)))
			     (<= (point) (car c-state-cache)))
			(setq c-state-cache (cdr c-state-cache))
		      ))))
	      (let ((here (point))
		    (shift (c-indent-line)))
		(setq c-state-cache (c-adjust-state (c-point 'bol) here
						    (- shift) c-state-cache)))
	      (goto-char (- (point-max) pos))
	      ;; if the buffer has changed due to the indentation, we
	      ;; need to recalculate syntax for the current line, but
	      ;; we won't need to update the state cache.
	      (if (/= (point) here)
		  (setq syntax (c-guess-basic-syntax))))
	  ;; must remove the newline we just stuck in (if we really did it)
	  (and delete-temp-newline
	       (save-excursion
		 ;; if there is whitespace before point, then preserve
		 ;; at least one space.
		 (delete-indentation)
		 (just-one-space)
		 (if (not preserve-p)
		     (delete-char -1))))
	  ;; since we're hanging the brace, we need to recalculate
	  ;; syntax.  Update the state to accurately reflect the
	  ;; beginning of the line.  We punt if we cross any open or
	  ;; closed parens because its just too hard to modify the
	  ;; known state.  This limitation will be fixed in v5.
	  (save-excursion
	    (let ((bol (c-point 'bol)))
	      (if (zerop (car (parse-partial-sexp bol (1- (point)))))
		  (setq c-state-cache (c-whack-state bol c-state-cache)
			syntax (c-guess-basic-syntax))
		;; gotta punt. this requires some horrible kludgery
		(beginning-of-line)
		(makunbound 'c-state-cache)
		(setq c-state-cache (c-parse-state)
		      syntax nil))))
	  )
	;; now adjust the line's indentation. don't update the state
	;; cache since c-guess-basic-syntax isn't called when the
	;; syntax is passed to c-indent-line
	(let ((here (point))
	      (shift (c-indent-line syntax)))
	  (setq c-state-cache (c-adjust-state (c-point 'bol) here
					      (- shift) c-state-cache)))
	;; Do all appropriate clean ups
	(let ((here (point))
	      (pos (- (point-max) (point)))
	      mbeg mend)
	  ;; clean up empty defun braces
	  (if (and c-auto-newline
		   (memq 'empty-defun-braces c-cleanup-list)
		   (= last-command-char ?\})
		   (c-intersect-lists '(defun-close class-close inline-close)
				      syntax)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (= (preceding-char) ?\{))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal)))
	      (delete-region (point) (1- here)))
	  ;; clean up brace-else-brace
	  (if (and c-auto-newline
		   (memq 'brace-else-brace c-cleanup-list)
		   (= last-command-char ?\{)
		   (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		   (progn
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (= mend here))
		   (not (c-in-literal)))
	      (progn
		(delete-region mbeg mend)
		(insert "} else {")))
	  ;; clean up brace-elseif-brace
	  (if (and c-auto-newline
		   (memq 'brace-elseif-brace c-cleanup-list)
		   (= last-command-char ?\{)
		   (re-search-backward "}[ \t\n]*else[ \t\n]+if[ \t\n]*" nil t)
		   (save-excursion
		     (goto-char (match-end 0))
		     (c-safe (forward-sexp 1))
		     (skip-chars-forward " \t\n")
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (= here (1+ (point))))
		   (not (c-in-literal)))
	      (progn
		(delete-region mbeg mend)
		(insert "} else if ")))
	  (goto-char (- (point-max) pos))
	  )
	;; does a newline go after the brace?
	(if (memq 'after newlines)
	    (progn
	      (newline)
	      ;; update on c-state-cache
	      (let* ((bufpos (- (point) 2))
		     (which (if (= (char-after bufpos) ?{) 'open 'close))
		     (c-state-cache (c-hack-state bufpos which c-state-cache)))
		(c-indent-line))))
	;; blink the paren
	(and (= last-command-char ?\})
	     old-blink-paren
	     (save-excursion
	       (c-backward-syntactic-ws safepos)
	       (if (boundp 'blink-paren-function)
		   (funcall old-blink-paren)
		 (run-hooks old-blink-paren))))
	))))
      
(defun c-electric-slash (arg)
  "Insert a slash character.
If slash is second of a double-slash C++ style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (= (preceding-char) ?/)
		      (= last-command-char ?/)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; if we are in a literal, or if arg is given do not re-indent the
  ;; current line, unless this star introduces a comment-only line.
  (if (and (not arg)
	   (memq (c-in-literal) '(c))
	   (= (preceding-char) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (= (preceding-char) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      ;; shut this up
      (let (c-echo-syntactic-information-p)
	(c-indent-line))
    ))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline might be inserted.  See
the variable `c-hanging-semi&comma-criteria' for how newline insertion
is determined.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon."
  (interactive "P")
  (let* ((lim (c-most-enclosing-brace (c-parse-state)))
	 (literal (c-in-literal lim))
	 (here (point))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups, reindentations, and newline insertions, but
      ;; only if c-auto-newline is turned on
      (if (not c-auto-newline) nil
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(= last-command-char ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(= last-command-char ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (= (preceding-char) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal lim)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	(c-indent-line)
	;; check to see if a newline should be added
	(let ((criteria c-hanging-semi&comma-criteria)
	      answer add-newline-p)
	  (while criteria
	    (setq answer (funcall (car criteria)))
	    ;; only nil value means continue checking
	    (if (not answer)
		(setq criteria (cdr criteria))
	      (setq criteria nil)
	      ;; only 'stop specifically says do not add a newline
	      (setq add-newline-p (not (eq answer 'stop)))
	      ))
	  (if add-newline-p
	      (progn (newline)
		     (c-indent-line)))
	  )))))

(defun c-electric-colon (arg)
  "Insert a colon.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after colons based on the value of `c-hanging-colons-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal.

This function cleans up double colon scope operators based on the
value of `c-cleanup-list'."
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 syntax newlines
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; insert the colon, then do any specified cleanups
      (self-insert-command (prefix-numeric-value arg))
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and c-auto-newline
		 (memq 'scope-operator c-cleanup-list)
		 (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?:))
		 (not (c-in-literal))
		 (not (= (char-after (- (point) 2)) ?:)))
	    (delete-region (point) (1- here)))
	(goto-char (- (point-max) pos)))
      ;; lets do some special stuff with the colon character
      (setq syntax (c-guess-basic-syntax)
	    ;; some language elements can only be determined by
	    ;; checking the following line.  Lets first look for ones
	    ;; that can be found when looking on the line with the
	    ;; colon
	    newlines
	    (and c-auto-newline
		 (or (c-lookup-lists '(case-label label access-label)
				     syntax c-hanging-colons-alist)
		     (c-lookup-lists '(member-init-intro inher-intro)
				     (prog2
					 (insert "\n")
					 (c-guess-basic-syntax)
				       (delete-char -1))
				     c-hanging-colons-alist))))
      ;; indent the current line
      (c-indent-line syntax)
      ;; does a newline go before the colon?  Watch out for already
      ;; non-hung colons.  However, we don't unhang them because that
      ;; would be a cleanup (and anti-social).
      (if (and (memq 'before newlines)
	       (save-excursion
		 (skip-chars-backward ": \t")
		 (not (bolp))))
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-line)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (c-indent-line)))
      )))

(defun c-electric-lt-gt (arg)
  "Insert a less-than, or greater-than character.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, the line will be re-indented if
the character inserted is the second of a C++ style stream operator
and the buffer is in C++ mode.

The line will also not be re-indented if a numeric argument is
supplied, or point is inside a literal."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (= (preceding-char) last-command-char)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))



;; set up electric character functions to work with pending-del,
;; (a.k.a. delsel) mode.  All symbols get the t value except
;; c-electric-delete which gets 'supersede.
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for delsel (Emacs)
    (put sym 'pending-delete t)))	; for pending-del (XEmacs)
 '(c-electric-pound
   c-electric-brace
   c-electric-slash
   c-electric-star
   c-electric-semi&comma
   c-electric-lt-gt
   c-electric-colon))
(put 'c-electric-delete 'delete-selection 'supersede) ; delsel
(put 'c-electric-delete 'pending-delete   'supersede) ; pending-del


(provide 'cc-cmds)
;;; cc-cmds.el ends here
