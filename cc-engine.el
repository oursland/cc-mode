;;; cc-engine.el --- core syntax guessing engine for CC mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-langs)

;; Silence the compiler.
(cc-bytecomp-defun buffer-syntactic-context) ; XEmacs


(defvar c-in-literal-cache t)

;; KLUDGE ALERT: c-maybe-labelp is used to pass information between
;; c-crosses-statement-barrier-p and c-beginning-of-statement-1.  A
;; better way should be implemented, but this will at least shut up
;; the byte compiler.
(defvar c-maybe-labelp nil)

;; WARNING WARNING WARNING
;;
;; Be *exceptionally* careful about modifications to this function!
;; Much of CC Mode depends on this Doing The Right Thing.  If you
;; break it you will be sorry.  If you think you know how this works,
;; you probably don't.  No human on Earth does! :-)
;;
;; WARNING WARNING WARNING

(defun c-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  ;;
  ;; Note: This function might skip past lim.  Several calls in
  ;; c-guess-basic-syntax gives it a too narrow limit, so they depend
  ;; on that. :P
  (let ((firstp t)
	(substmt-p t)
	donep c-in-literal-cache saved
	(last-begin (point)))
    ;; first check for bare semicolon
    (if (and (progn (c-backward-syntactic-ws lim)
		    (eq (char-before) ?\;))
	     (c-safe (progn (forward-char -1)
			    (setq saved (point))
			    t))
	     (progn (c-backward-syntactic-ws lim)
		    (memq (char-before) '(?\; ?{ ?:)))
	     )
	(setq last-begin saved)
      (goto-char last-begin)
      (while (not donep)
	;; stop at beginning of buffer
	(if (bobp)
	    (setq donep t)
	  ;; go backwards one balanced expression, but be careful of
	  ;; unbalanced paren being reached
	  (c-backward-syntactic-ws)	; To skip any macros.
	  (if (not (c-safe (progn (c-backward-sexp 1) t)))
	      (progn
		(if firstp
		    (backward-up-list 1)
		  (goto-char last-begin))
		;; skip over any unary operators, or other special
		;; characters appearing at front of identifier
		(save-excursion
		  (while (progn
			   (c-backward-syntactic-ws lim)
			   (/= (skip-chars-backward "-+!*&:.~@") 0)))
		  (if (eq (char-before) ?\()
		      (setq last-begin (point))))
		(goto-char last-begin)
		(setq donep t)))

	  (setq c-maybe-labelp nil)
	  ;; see if we're in a literal. if not, then this bufpos may be
	  ;; a candidate for stopping
	  (cond
	   ;; CASE 0: did we hit the error condition above?
	   (donep)
	   ;; CASE 1: Some kind of literal?
	   ((c-literal-limits lim))
	   ;; CASE 2: A line continuation?
	   ((looking-at "\\\\$"))
	   ;; CASE 3: A macro start?
	   ((save-excursion
	      (skip-chars-backward " \t")
	      (when (and (eq (char-before) ?#)
			 (eq (c-point 'boi) (1- (point)))
			 (looking-at "[ \t]*define\\>[^_]")
			 (c-safe (c-forward-sexp 2) t))
		;; Note that if we weren't in the macro to begin with,
		;; we'd never get here.
		(setq saved (point))))
	    (goto-char saved)
	    (if (eq (char-after) ?\()
		(c-safe (c-forward-sexp 1)))
	    (c-forward-syntactic-ws)
	    ;; Treate the first token inside the macro as the
	    ;; statement start.
	    (setq donep t
		  last-begin (point)))
	   ;; CASE 4: are we looking at a conditional keyword?
	   ((or (and c-conditional-key (looking-at c-conditional-key))
		(and (eq (char-after) ?\()
		     (save-excursion
		       (c-forward-sexp 1)
		       (c-forward-syntactic-ws)
		       (not (eq (char-after) ?\;)))
		     (let ((here (point))
			   (foundp (progn
				     (c-backward-syntactic-ws lim)
				     (forward-word -1)
				     (and lim
					  (<= lim (point))
					  (not (c-in-literal lim))
					  (not (eq (char-before) ?_))
					  c-conditional-key
					  (looking-at c-conditional-key)
					  ))))
		       ;; did we find a conditional?
		       (if (not foundp)
			   (goto-char here))
		       foundp)))
	    ;; are we in the middle of an else-if clause?
	    (if (save-excursion
		  (and (not substmt-p)
		       (c-safe (progn (c-forward-sexp -1) t))
		       (looking-at (concat "\\<else"
					   "\\([ \t\n]\\|\\\\\n\\)+"
					   "if\\>[^_]"))
		       (not (c-in-literal lim))))
		(progn
		  (c-forward-sexp -1)
		  (c-backward-to-start-of-if lim)))
	    ;; are we sitting at an else clause, that we are not a
	    ;; substatement of?
	    (if (and (not substmt-p)
		     (looking-at "\\<else\\>[^_]"))
		(c-backward-to-start-of-if lim))
	    ;; a finally or a series of catches?
	    (if (not substmt-p)
		(while (looking-at "\\<\\(catch\\|finally\\)\\>[^_]")
		  (c-safe (c-backward-sexp 2))
		  (if (eq (char-after) ?\()
		      (c-safe (c-backward-sexp)))))
	    ;; are we sitting at the while of a do-while?
	    (if (and (looking-at "\\<while\\>[^_]")
		     (c-backward-to-start-of-do lim))
		(progn
		  (setq last-begin (point))
		  (c-backward-syntactic-ws lim)
		  (setq donep (eq (char-before) ?\;)))
	      (setq last-begin (point)
		    donep substmt-p)))
	   ;; CASE 5: are we looking at a label?  (But we handle
	   ;; switch labels later.)
	   ((and (looking-at c-label-key)
		 (not (looking-at "default\\>[^_]"))
		 (not (and (c-major-mode-is 'pike-mode)
			   (save-excursion
			     ;; Not inside a Pike type declaration?
			     (and (c-safe (backward-up-list 1) t)
				  (eq (char-after) ?\()))))))
	   ;; CASE 6: is this the first time we're checking?
	   (firstp
	    (setq firstp nil
		  substmt-p (save-excursion
			      ;; Move over in-expression blocks before
			      ;; checking the barrier
			      (if (or (memq (char-after) '(?\( ?\[))
				      (and (eq (char-after) ?{)
					   (c-looking-at-inexpr-block lim)))
				  (c-forward-sexp 1))
			      (not (c-crosses-statement-barrier-p
				    (point) last-begin)))
		  last-begin (point)))
	   ;; CASE 7: have we crossed a statement barrier?
	   ((save-excursion
	      ;; Move over in-expression blocks before checking the
	      ;; barrier
	      (if (or (memq (char-after) '(?\( ?\[))
		      (and (eq (char-after) ?{)
			   (c-looking-at-inexpr-block lim)))
		  (c-forward-sexp 1))
	      (c-crosses-statement-barrier-p (point) last-begin))
	    (setq donep t))
	   ;; CASE 8: ignore labels
	   ((and c-maybe-labelp
		 (or (and c-access-key (looking-at c-access-key))
		     ;; with switch labels, we have to go back further
		     ;; to try to pick up the case or default
		     ;; keyword. Potential bogosity alert: we assume
		     ;; `case' or `default' is first thing on line
		     (let ((here (point)))
		       (beginning-of-line)
		       (c-forward-syntactic-ws here)
		       (if (looking-at c-switch-label-key)
			   t
			 (goto-char here)
			 nil)))))
	   ;; CASE 9: ObjC or Java method def
	   ((and c-method-key
		 (setq last-begin (c-in-method-def-p)))
	    (setq donep t))
	   ;; CASE 10: Normal token.  At bob, we can end up at ws or a
	   ;; comment, and last-begin shouldn't be updated then.
	   ((not (looking-at "\\s \\|/[/*]"))
	    (setq last-begin (point)))
	   ))))
    (goto-char last-begin)
    ;; We always want to skip over the non-whitespace modifier
    ;; characters that can start a statement.
    (let ((lim (point)))
      (skip-chars-backward "-+!*&~@`# \t" (c-point 'boi))
      (skip-chars-forward " \t" lim))))

(defun c-end-of-statement-1 ()
  (condition-case nil
      (let (beg end found)
	(while (and (not (eobp))
		    (progn
		      (setq beg (point))
		      (c-forward-sexp 1)
		      (setq end (point))
		      (goto-char beg)
		      (setq found nil)
		      (while (and (not found)
				  (re-search-forward "[;{}]" end t))
			(if (not (c-in-literal beg))
			    (setq found t)))
		      (not found)))
	  (goto-char end))
	(re-search-backward "[;{}]")
	(forward-char 1))
    (error
     (let ((beg (point)))
       (c-safe (backward-up-list -1))
       (let ((end (point)))
	 (goto-char beg)
	 (search-forward ";" end 'move)))
     )))


(defun c-crosses-statement-barrier-p (from to)
  ;; Does buffer positions FROM to TO cross a C statement boundary?
  (let ((here (point))
	(lim from)
	crossedp)
    (condition-case ()
	(progn
	  (goto-char from)
	  (while (and (not crossedp)
		      (< (point) to))
	    (skip-chars-forward "^;{}:" (1- to))
	    (if (not (c-in-literal lim))
		(progn
		  (if (memq (char-after) '(?\; ?{ ?}))
		      (setq crossedp t)
		    (if (eq (char-after) ?:)
			(setq c-maybe-labelp t))
		    (forward-char 1))
		  (setq lim (point)))
	      (forward-char 1))))
      (error (setq crossedp nil)))
    (goto-char here)
    crossedp))


;; This is a dynamically bound cache used together with
;; c-query-macro-start and c-query-and-set-macro-start.  It only works
;; as long as point doesn't cross a macro boundary.
(defvar c-macro-start 'unknown)

(defsubst c-query-and-set-macro-start ()
  (if (symbolp c-macro-start)
      (setq c-macro-start (save-excursion
			    (and (c-beginning-of-macro)
				 (point))))
    c-macro-start))

(defsubst c-query-macro-start ()
  (if (symbolp c-macro-start)
      (save-excursion
	(and (c-beginning-of-macro)
	     (point)))
    c-macro-start))

(defun c-beginning-of-macro (&optional lim)
  ;; Go to the beginning of a cpp macro definition.  Leaves point at
  ;; the beginning of the macro and returns t if in a cpp macro
  ;; definition, otherwise returns nil and leaves point unchanged.
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
	(forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
	       (looking-at "#[ \t]*[a-zA-Z0-9!]"))
	  t
	(goto-char here)
	nil))))

;; Skipping of "syntactic whitespace", defined as lexical whitespace,
;; C and C++ style comments, and preprocessor directives.  Search no
;; farther back or forward than optional LIM.

(defun c-forward-syntactic-ws (&optional lim)
  ;; Forward skip of syntactic whitespace.
  (let ((here (point-max)))
    (or lim (setq lim here))
    (while (/= here (point))
      ;; If forward-comment in at least XEmacs 21 is given a large
      ;; positive value, it'll loop all the way through if it hits eob.
      (while (c-forward-comment 5))
      (setq here (point))
      (if (looking-at "\\\\$")
	  (forward-char)
	;; skip preprocessor directives
	(when (and (looking-at "#[ \t]*[a-zA-Z0-9!]")
		   (progn (skip-chars-backward " \t")
			  (bolp)))
	  (end-of-line)
	  (while (and (<= (point) lim)
		      (eq (char-before) ?\\)
		      (= (forward-line 1) 0))
	    (end-of-line))
	  (when (> (point) lim)
	    ;; Don't move past the macro if that'd take us past the limit.
	    (goto-char here)))))
    (goto-char (min (point) lim))))

(defun c-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace.
  (let ((here (point-min))
	(line-cont 'maybe))
    (or lim (setq lim here))
    (while (/= here (point))
      ;; If forward-comment in Emacs 19.34 is given a large negative
      ;; value, it'll loop all the way through if it hits bob.
      (while (c-forward-comment -5))
      (setq here (point))
      (if (and (eq (char-before) ?\\)
	       (looking-at "$"))
	  (progn
	    (backward-char)
	    (setq line-cont t))
	(when (eq line-cont 'maybe)
	  (save-excursion
	    (end-of-line)
	    (setq line-cont (eq (char-before) ?\\))))
	(when (or line-cont
		  (and (c-beginning-of-macro)
		       (< (point) lim)))
	  ;; Don't move past the macro if we began inside it, or if
	  ;; that'd take us past the limit.
	  (goto-char here))
	(setq line-cont nil)))
    (goto-char (max (point) lim))))


;; Moving by tokens, where a token is defined as all symbols and
;; identifiers which aren't syntactic whitespace (note that e.g. "->" is
;; considered to be two tokens).  Point is always either left at the
;; beginning of a token or not moved at all.  COUNT specifies the
;; number of tokens to move; a negative COUNT moves in the opposite
;; direction.  A COUNT of 0 moves to the next token beginning only if
;; not already at one.  If BALANCED is true, move over balanced
;; parens, otherwise move into them.  Also, if BALANCED is true, never
;; move out of an enclosing paren.  LIM sets the limit for the
;; movement and defaults to the point limit.  Returns the number of
;; tokens left to move (positive or negative).  If BALANCED is true, a
;; move over a balanced paren counts as one.  Note that if COUNT is 0
;; and no appropriate token beginning is found, 1 will be returned.
;; Thus, a return value of 0 guarantees that point is at the requested
;; position and a return value less (without signs) than COUNT
;; guarantees that point is at the beginning of some token.

(defun c-forward-token-1 (&optional count balanced lim)
  (or count (setq count 1))
  (if (< count 0)
      (- (c-backward-token-1 (- count) balanced lim))
    (let ((jump-syntax (if balanced
			   '(?w ?_ ?\( ?\) ?\" ?\\ ?/ ?$ ?')
			 '(?w ?_ ?\" ?\\ ?/ ?')))
	  (last (point))
	  (prev (point)))
      (save-restriction
	(if lim (narrow-to-region (point-min) lim))
	(if (/= (point)
		(progn (c-forward-syntactic-ws) (point)))
	    ;; Skip whitespace.  Count this as a move if we did in fact
	    ;; move and aren't out of bounds.
	    (or (eobp)
		(setq count (max (1- count) 0))))
	(if (and (= count 0)
		 (or (and (memq (char-syntax (or (char-after) ? )) '(?w ?_))
			  (memq (char-syntax (or (char-before) ? )) '(?w ?_)))
		     (eobp)))
	    ;; If count is zero we should jump if in the middle of a
	    ;; token or if there is whitespace between point and the
	    ;; following token beginning.
	    (setq count 1))
	(if (eobp)
	    (goto-char last)
	  ;; Avoid having the limit tests inside the loop.
	  (condition-case nil
	      (while (> count 0)
		(setq prev last
		      last (point))
		(if (memq (char-syntax (char-after)) jump-syntax)
		    (goto-char (scan-sexps (point) 1))
		  (forward-char))
		(c-forward-syntactic-ws lim)
		(setq count (1- count)))
	    (error (goto-char last)))
	  (when (eobp)
	    (goto-char prev)
	    (setq count (1+ count)))))
      count)))

(defun c-backward-token-1 (&optional count balanced lim)
  (or count (setq count 1))
  (if (< count 0)
      (- (c-forward-token-1 (- count) balanced lim))
    (let ((jump-syntax (if balanced
			   '(?w ?_ ?\( ?\) ?\" ?\\ ?/ ?$ ?')
			 '(?w ?_ ?\" ?\\ ?/ ?')))
	  last)
      (if (and (= count 0)
	       (or (and (memq (char-syntax (or (char-after) ? )) '(?w ?_))
			(memq (char-syntax (or (char-before) ? )) '(?w ?_)))
		   (/= (point)
		       (save-excursion (c-forward-syntactic-ws) (point)))
		   (eobp)))
	  ;; If count is zero we should jump if in the middle of a
	  ;; token or if there is whitespace between point and the
	  ;; following token beginning.
	  (setq count 1))
      (save-restriction
	(if lim (narrow-to-region lim (point-max)))
	(or (bobp)
	    (progn
	      ;; Avoid having the limit tests inside the loop.
	      (condition-case nil
		  (while (progn
			   (setq last (point))
			   (> count 0))
		    (c-backward-syntactic-ws lim)
		    (if (memq (char-syntax (char-before)) jump-syntax)
			(goto-char (scan-sexps (point) -1))
		      (backward-char))
		    (setq count (1- count)))
		(error (goto-char last)))
	      (if (bobp) (goto-char last)))))
      count)))


(defun c-in-literal (&optional lim detect-cpp)
  ;; Return `c' if in a C-style comment, `c++' if in a C++ style
  ;; comment, `string' if in a string literal, `pound' if detect-cpp
  ;; is non-nil and on a preprocessor line, or nil if somewhere else.
  ;; Optional LIM is used as the backward limit of the search.  If
  ;; omitted, or nil, `beginning-of-defun' is used.  We cache the last
  ;; point calculated if the cache is enabled.
  (if (and (vectorp c-in-literal-cache)
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    (let ((rtn (save-excursion
		 (let* ((lim (or lim (c-point 'bod)))
			(state (parse-partial-sexp lim (point))))
		   (cond
		    ((nth 3 state) 'string)
		    ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		    ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
		    (t nil))))))
      ;; cache this result if the cache is enabled
      (if (not c-in-literal-cache)
	  (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))

;; XEmacs has a built-in function that should make this much quicker.
;; I don't think we even need the cache, which makes our lives more
;; complicated anyway.  In this case, lim is only used to detect
;; cpp directives.
(defun c-fast-in-literal (&optional lim detect-cpp)
  (let ((context (buffer-syntactic-context)))
    (cond
     ((eq context 'string) 'string)
     ((eq context 'comment) 'c++)
     ((eq context 'block-comment) 'c)
     ((and detect-cpp (save-excursion (c-beginning-of-macro lim))) 'pound))))

(if (fboundp 'buffer-syntactic-context)
    (defalias 'c-in-literal 'c-fast-in-literal))

(defun c-literal-limits (&optional lim near not-in-delimiter)
  ;; Returns a cons of the beginning and end positions of the comment
  ;; or string surrounding point (including both delimiters), or nil
  ;; if point isn't in one.  If LIM is non-nil, it's used as the
  ;; "safe" position to start parsing from.  If NEAR is non-nil, then
  ;; the limits of any literal next to point is returned.  "Next to"
  ;; means there's only [ \t] between point and the literal.  The
  ;; search for such a literal is done first in forward direction.  If
  ;; NOT-IN-DELIMITER is non-nil, the case when point is inside a
  ;; starting delimiter won't be recognized.  This only has effect for
  ;; comments, which have starting delimiters with more than one
  ;; character.
  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (c-point 'bod)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)
	     ;; String.  Search backward for the start.
	     (while (nth 3 state)
	       (search-backward (make-string 1 (nth 3 state)))
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 7 state)
	     ;; Line comment.  Search from bol for the comment starter.
	     (beginning-of-line)
	     (setq state (parse-partial-sexp lim (point))
		   lim (point))
	     (while (not (nth 7 state))
	       (search-forward "//")	; Should never fail.
	       (setq state (parse-partial-sexp
			    lim (point) nil nil state)
		     lim (point)))
	     (backward-char 2)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((nth 4 state)
	     ;; Block comment.  Search backward for the comment starter.
	     (while (nth 4 state)
	       (search-backward "/*")	; Should never fail.
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((and (not not-in-delimiter)
		  (not (nth 5 state))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at "/[/*]")	; Line or block comment.
	       (cons (point) (progn (c-forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))
		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(defun c-literal-limits-fast (&optional lim near not-in-delimiter)
  ;; Like c-literal-limits, but for emacsen whose `parse-partial-sexp'
  ;; returns the pos of the comment start.
  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (c-point 'bod)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)		; String.
	     (goto-char (nth 8 state))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 4 state)		; Comment.
	     (goto-char (nth 8 state))
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((and (not not-in-delimiter)
		  (not (nth 5 state))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at "/[/*]")	; Line or block comment.
	       (cons (point) (progn (c-forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))
		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(if (c-safe (> (length (save-excursion (parse-partial-sexp 1 1))) 8))
    (defalias 'c-literal-limits 'c-literal-limits-fast))

(defun c-collect-line-comments (range)
  ;; If the argument is a cons of two buffer positions (such as
  ;; returned by c-literal-limits), and that range contains a C++
  ;; style line comment, then an extended range is returned that
  ;; contains all adjacent line comments (i.e. all comments that
  ;; starts in the same column with no empty lines or non-whitespace
  ;; characters between them).  Otherwise the argument is returned.
  (save-excursion
    (condition-case nil
	(if (and (consp range) (progn
				 (goto-char (car range))
				 (looking-at "//")))
	    (let ((col (current-column))
		  (beg (point))
		  (bopl (c-point 'bopl))
		  (end (cdr range)))
	      ;; Got to take care in the backward direction to handle
	      ;; comments which are preceded by code.
	      (while (and (c-forward-comment -1)
			  (>= (point) bopl)
			  (looking-at "//")
			  (= col (current-column)))
		(setq beg (point)
		      bopl (c-point 'bopl)))
	      (goto-char end)
	      (while (and (progn (skip-chars-forward " \t")
				 (looking-at "//"))
			  (= col (current-column))
			  (prog1 (zerop (forward-line 1))
			    (setq end (point)))))
	      (cons beg end))
	  range)
      (error range))))

(defun c-literal-type (range)
  ;; Convenience function that given the result of c-literal-limits,
  ;; returns nil or the type of literal that the range surrounds.
  ;; It's much faster than using c-in-literal and is intended to be
  ;; used when you need both the type of a literal and its limits.
  (if (consp range)
      (save-excursion
	(goto-char (car range))
	(cond ((eq (char-syntax (or (char-after) ?\ )) ?\") 'string)
	      ((looking-at "//") 'c++)
	      (t 'c)))			; Assuming the range is valid.
    range))



;; utilities for moving and querying around syntactic elements

(defvar c-parsing-error nil)

(defvar c-state-cache nil)
(make-variable-buffer-local 'c-state-cache)
;; The state cache used by `c-parse-state' to cut down the amount of
;; searching.  It's the result from some earlier `c-parse-state' call.
;; The use of the cached info is more effective if the next
;; `c-parse-state' call is on a line close by the one the cached state
;; was made at; the cache can actually slow down a little if the
;; cached state was made very far back in the buffer.  The cache is
;; most effective if `c-parse-state' is used on each line while moving
;; forward.

(defun c-parse-state ()
  ;; Finds and records all noteworthy parens between some good point
  ;; earlier in the file and point.  That good point is at least the
  ;; beginning of the top-level construct we are in, or the beginning
  ;; of the preceding top-level construct if we aren't in one.
  ;;
  ;; The returned value is a list of the noteworthy parens with the
  ;; last one first.  If an element in the list is an integer, it's
  ;; the position of an open paren which have not been closed before
  ;; point.  If an element is a cons, it gives the position of a
  ;; closed brace paren pair; the car is the start paren position and
  ;; the cdr is the position following the closing paren.  Only the
  ;; last closed brace paren pair before each open paren is recorded,
  ;; and thus the state never contains two cons elements in
  ;; succession.
  (save-restriction
    (let* ((here (point))
	   (c-macro-start (c-query-macro-start))
	   (in-macro-start (or c-macro-start (point)))
	   state-tail-intact
	   (state-head (list nil))	; First part of the state.
	   (state-ptr state-head)	; Last cons in state-head.
	   (state-foot			; Last part of the state.
	    (and c-state-cache
		 ;; Try to use cached info before point, but back up
		 ;; before any macro first, since the cached state
		 ;; might contain braces inside macros which we should
		 ;; ignore if we've exited the macro.
		 (save-excursion
		   (when (<= (point) in-macro-start)
		     (c-backward-syntactic-ws)
		     (setq here (point)))
		   (let ((newstate (c-whack-state-after here c-state-cache)))
		     (and newstate
			  (or (cdr newstate)
			      (eq (car newstate)
				  (let ((elem c-state-cache))
				    (while (cdr elem)
				      (setq elem (cdr elem)))
				    (car elem))))
			  ;; The last element in the cached state was
			  ;; not changed by the whack.
			  (setq state-tail-intact t))
		     newstate))))
	   (pos here)
	   placeholder last-pos sexp-pos)
      (narrow-to-region
       ;; Narrow out the first part of the buffer that should be
       ;; outside the search.  If we got a cached state, we continue
       ;; at its end, otherwise we go back two bods.
       (or (when (setq last-pos
		       ;; Get the latest position we know are directly
		       ;; inside the closest containing paren on the
		       ;; cached state.
		       (and state-foot
			    (if (consp (car state-foot))
				(cdr (car state-foot))
			      (1+ (car state-foot)))))
	     ;; There's a cached state with a containing paren.  Pop
	     ;; off the stale containing sexps from it by going
	     ;; forward out of parens as far as possible.
	     (narrow-to-region (point-min) (point))
	     (while (and state-foot
			 (setq placeholder (c-up-list-forward last-pos)))
	       (setq last-pos placeholder)
	       (if (consp (car state-foot))
		   (setq sexp-pos (car-safe (cdr state-foot))
			 state-foot (cdr-safe (cdr state-foot)))
		 (setq sexp-pos (car state-foot)
		       state-foot (cdr state-foot))))
	     (when (and sexp-pos (eq (char-after sexp-pos) ?{))
	       ;; The last paren pair we moved out from was a brace
	       ;; pair.  Modify the state to record this as a closed
	       ;; pair now.
	       (if (consp (car-safe state-foot))
		   (setq state-foot (cdr state-foot)))
	       (setq state-foot (cons (cons sexp-pos last-pos)
				      state-foot)))
	     (when state-foot
	       (if (or state-tail-intact
		       (consp (car state-foot))
		       (cdr state-foot))
		   ;; If the whack above didn't touch the last element
		   ;; of the state, we know it comes from an earlier
		   ;; point and is thus complete (i.e. no risk there's
		   ;; some closed sexp earlier that we should
		   ;; include).  Otherwise we only use it if it still
		   ;; got at least one closed sexp or two open ones.
		   last-pos
		 (setq state-foot nil))))
	   (when state-foot
	     ;; The cached state didn't provide any containing parens,
	     ;; but it has a closed one before the current point.
	     ;; Let's continue after that.
	     (cdr (car state-foot)))
	   (save-excursion
	     ;; go back 2 bods, but ignore any bogus positions
	     ;; returned by beginning-of-defun (i.e. open paren in
	     ;; column zero)
	     (goto-char here)
	     (let ((cnt 2))
	       (while (not (or (bobp) (zerop cnt)))
		 (c-beginning-of-defun-1)
		 (if (and
		      (eq (char-after) ?\{)
		      ;; The following catches an obscure special case
		      ;; where the brace is preceded by an open paren.
		      ;; That can only legally occur with blocks
		      ;; inside expressions and in Pike special brace
		      ;; lists.  Even so, this test is still bogus
		      ;; then, but hopefully good enough.  (We don't
		      ;; want to use up-list here since it might be
		      ;; slow.)
		      (save-excursion
			(c-backward-syntactic-ws)
			(not (eq (char-before) ?\())))
		     (setq cnt (1- cnt)))))
	     (point)))
       (point-max))
      (while pos
	;; Search backward for the closest preceding balanced brace pair.
	(while (and
		(setq last-pos (c-down-list-backward pos))
		(progn
		  (unless (setq pos (c-up-list-backward last-pos))
		    ;; Found a close paren without a corresponding
		    ;; opening one.
		    (when (not state-foot)
		      ;; Maybe we've narrowed too much.  If we're
		      ;; building on a cached state that can't happen,
		      ;; assuming the cache is correct.
		      (narrow-to-region 1 (point-max))
		      (setq pos (c-up-list-backward last-pos)))
		    (unless pos
		      (save-excursion
			(goto-char last-pos)
			(beginning-of-line)
			(setq c-parsing-error
			      (format "Unbalanced close paren at line %d"
				      (1+ (count-lines 1 (point))))))))
		  (when pos
		    (if (and (eq (char-after pos) ?\{)
			     ;; If both we and the pair is inside
			     ;; the same macro then it's
			     ;; interesting, otherwise it's only
			     ;; interesting if not inside a macro.
			     (or (>= pos in-macro-start)
				 (not (save-excursion
					(goto-char pos)
					(c-beginning-of-macro)))))
			(progn
			  (setcdr state-ptr
				  (list (cons pos (1+ last-pos))))
			  (setq state-ptr (cdr state-ptr))
			  nil)
		      t)))))
	;; Search backward for the closest containing open paren.
	(when (and pos (setq pos (c-up-list-backward pos)))
	  ;; NB: We don't check if the paren is in a macro here;
	  ;; macros containing unbalanced parens will confuse CC Mode.
	  ;; Could perhaps handle it here, but there are still
	  ;; countless other places that trip up.
	  (setcdr state-ptr (list pos))
	  (setq state-ptr (cdr state-ptr))))
      (setcdr state-ptr (if (and (consp (car state-ptr))
				 (consp (car state-foot)))
			    ;; Avoid two closed sexps in a row.
			    (cdr state-foot)
			  state-foot))
      (setq c-state-cache (cdr state-head)))))

(defun c-check-state-cache (beg end old-length)
  ;; Used on `after-change-functions' to adjust `c-state-cache'.
  ;; Prefer speed to finesse here, since there will be many more calls
  ;; to this function than times `c-state-cache' is used.
  (and c-state-cache
       (if (consp (car c-state-cache))
	   (< beg (cdr (car c-state-cache)))
	 (<= beg (car c-state-cache)))
       (setq c-state-cache (c-whack-state-after beg c-state-cache))))

(defun c-whack-state-before (bufpos state)
  ;; Whack off any state information from STATE which lies before
  ;; BUFPOS.
  (let* ((newstate (list nil))
	 (ptr newstate)
	 car)
    (while state
      (setq car (car state)
	    state (cdr state))
      (if (< (if (consp car) (car car) car) bufpos)
	  (setq state nil)
	(setcdr ptr (list car))
	(setq ptr (cdr ptr))))
    (cdr newstate)))

(defun c-whack-state-after (bufpos state)
  ;; Whack off any state information from STATE which lies at or after
  ;; BUFPOS.
  (catch 'done
    (while state
      (let ((car (car state)))
	(if (consp car)
	    ;; just check the car, because in a balanced brace
	    ;; expression, it must be impossible for the corresponding
	    ;; close brace to be before point, but the open brace to
	    ;; be after.
	    (if (<= bufpos (car car))
		nil			; whack it off
	      (if (< bufpos (cdr car))
		  ;; its possible that the open brace is before
		  ;; bufpos, but the close brace is after.  In that
		  ;; case, convert this to a non-cons element.  The
		  ;; rest of the state is before bufpos, so we're
		  ;; done.
		  (throw 'done (cons (car car) (cdr state)))
		;; we know that both the open and close braces are
		;; before bufpos, so we also know that everything else
		;; on state is before bufpos.
		(throw 'done state)))
	  (if (<= bufpos car)
	      nil			; whack it off
	    ;; it's before bufpos, so everything else should too.
	    (throw 'done state)))
	(setq state (cdr state)))
      nil)))

(defun c-whack-state (bufpos state)
  (c-whack-state-after bufpos state))
(make-obsolete 'c-whack-state 'c-whack-state-after)

(defun c-hack-state (bufpos which state)
  ;; Using BUFPOS buffer position, and WHICH (must be 'open or
  ;; 'close), hack the c-parse-state STATE and return the results.
  ;;
  ;; From code inspection, "hack" above is found out to mean:
  ;;
  ;; o  If WHICH is 'open: Prepend BUFPOS to the state if it's not
  ;;    there already.  If the state has a cons as the first element,
  ;;    its car is _not_ examined to see whether it's equal to BUFPOS.
  ;;
  ;; o  If WHICH is 'close: If the first element of the state isn't a
  ;;    cons, make it a cons with BUFPOS as the cdr.  If the first
  ;;    element is a cons, remove it from the state and check whether
  ;;    the same thing can be done to the second element (which now
  ;;    has become the first).
  ;;
  ;;    The state is assumed to not contain two or more cons in
  ;;    succession, and the function also ensures that holds when a
  ;;    cons is added to it.
  (if (eq which 'open)
      (let ((car (car state)))
	(if (or (null car)
		(consp car)
		(/= bufpos car))
	    (cons bufpos state)
	  state))
    ;; 'close brace
    (let ((car (car state))
	  (cdr (cdr state)))
      (if (consp car)
	  (setq car (car cdr)
		cdr (cdr cdr)))
      ;; TBD: is this test relevant???
      (if (consp car)
	  (error "Got two cons in succession in state: %s" state)
	;; watch out for balanced expr already on cdr of list
	(cons (cons car bufpos)
	      (if (consp (car cdr))
		  (cdr cdr) cdr))
	))))

(defun c-adjust-state (from to shift state)
  ;; Adjust all points in state that lie in the region FROM..TO by
  ;; SHIFT amount.
  (mapcar
   (function
    (lambda (e)
      (if (consp e)
	  (let ((car (car e))
		(cdr (cdr e)))
	    (if (and (<= from car) (< car to))
		(setcar e (+ shift car)))
	    (if (and (<= from cdr) (< cdr to))
		(setcdr e (+ shift cdr))))
	(if (and (<= from e) (< e to))
	    (setq e (+ shift e))))
      e))
   state))


(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let* ((lim (or lim (c-point 'bod))))
    (c-with-syntax-table c++-template-syntax-table
      (c-backward-token-1 0 t lim)
      (while (and (looking-at "[_a-zA-Z<,]")
		  (= (c-backward-token-1 1 t lim) 0)))
      (skip-chars-forward "^:"))))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  (save-excursion
    (beginning-of-line)
    (and c-method-key
	 (looking-at c-method-key)
	 (point))
    ))

(defun c-at-toplevel-p ()
  "Return a determination as to whether point is at the `top-level'.
Being at the top-level means that point is either outside any
enclosing block (such function definition), or inside a class
definition, but outside any method blocks.

If point is not at the top-level (e.g. it is inside a method
definition), then nil is returned.  Otherwise, if point is at a
top-level not enclosed within a class definition, t is returned.
Otherwise, a 2-vector is returned where the zeroth element is the
buffer position of the start of the class declaration, and the first
element is the buffer position of the enclosing class's opening
brace."
  (let ((state (c-parse-state)))
    (or (not (c-most-enclosing-brace state))
	(c-search-uplist-for-classkey state))))

(defun c-forward-to-cpp-expression ()
  "Assuming point is at the \"#\" that introduces a preprocessor
directive, it's moved forward to the start of the expression it may
contain, i.e. the definition of a \"#define\", or the test expression
of an \"#if\", or the macro argument of an \"#ifdef\", etc. Non-nil is
returned in this case, in all other cases nil is returned and point
isn't moved."
  (when (and (looking-at
	      (concat "#[ \t]*\\("
		      "define[ \t]+\\(\\sw\\|_\\)+\\(\([^\)]*\)\\)?"
		      "\\|"
		      "\\(if\\|elif\\|ifdef\\)[ \t]"
		      "\\)\\([ \t]\\|\\\\\n\\)*"))
	     (not (= (match-end 0) (c-point 'eol))))
    (goto-char (match-end 0))))

(defun c-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (c-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (eq (char-before) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>[^_]"))
	  (c-backward-syntactic-ws)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(while (eq (char-before) ?,)
	  ;; this will catch member inits with multiple
	  ;; line arglists
	  (forward-char -1)
	  (c-backward-syntactic-ws (c-point 'bol))
	  (if (eq (char-before) ?\))
	      (c-backward-sexp 2)
	    (c-backward-sexp 1))
	  (c-backward-syntactic-ws))
	(if (and (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws)
		   (looking-at "\\([ \t\n]\\|\\\\\n\\)*:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (and (eq (char-before) ?\))
	   ;; check if we are looking at a method def
	   (or (not c-method-key)
	       (progn
		 (c-forward-sexp -1)
		 (forward-char -1)
		 (c-backward-syntactic-ws)
		 (not (or (memq (char-before) '(?- ?+))
			  ;; or a class category
			  (progn
			    (c-forward-sexp -2)
			    (looking-at c-class-key))
			  )))))
      )))

;; defuns to look backwards for things
(defun c-backward-to-start-of-do (&optional lim)
  ;; Move to the start of the last "unbalanced" do expression.
  ;; Optional LIM is the farthest back to search.  If none is found,
  ;; nil is returned and point is left unchanged, otherwise t is returned.
  (let ((do-level 1)
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(here (point))
	foundp)
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case nil
	  (progn
	    (c-backward-sexp 1)
	    (cond
	     ;; break infloop for illegal C code
	     ((bobp) (setq do-level 0))
	     ((memq (c-in-literal lim) '(c c++)))
	     ((looking-at "while\\b[^_]")
	      (setq do-level (1+ do-level)))
	     ((looking-at "do\\b[^_]")
	      (if (zerop (setq do-level (1- do-level)))
		  (setq foundp t)))
	     ((<= (point) lim)
	      (setq do-level 0)
	      (goto-char lim))))
	(error
	 (goto-char lim)
	 (setq do-level 0))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-backward-to-start-of-if (&optional lim)
  ;; Move to the start of the last "unbalanced" if and return t.  If
  ;; none is found, and we are looking at an if clause, nil is
  ;; returned.
  (let ((if-level 1)
	(here (c-point 'bol))
	(case-fold-search nil)
	(lim (or (and lim (>= (point) lim) lim)
		 (c-point 'bod)))
	(at-if (looking-at "if\\b[^_]")))
    (or (catch 'orphan-if
	  (while (and (not (bobp))
		      (not (zerop if-level)))
	    (c-backward-syntactic-ws lim)
	    (condition-case nil
		(c-backward-sexp 1)
	      (error
	       (unless at-if
		 (throw 'orphan-if nil))))
	    (cond
	     ((looking-at "else\\b[^_]")
	      (setq if-level (1+ if-level)))
	     ((looking-at "if\\b[^_]")
	      ;; check for else if... skip over
	      (let ((here (point)))
		(c-safe (c-forward-sexp -1))
		(if (looking-at (concat "\\<else"
					"\\([ \t\n]\\|\\\\\n\\)+"
					"if\\>[^_]"))
		    nil
		  (setq if-level (1- if-level))
		  (goto-char here))))
	     ((looking-at "while\\b[^_]")
	      (unless (c-backward-to-start-of-do lim)
		(throw 'orphan-if nil)))
	     ((< (point) lim)
	      (setq if-level 0)
	      (goto-char lim))
	     ))
	  t)
	(progn
	  (goto-char here)
	  (c-beginning-of-statement-1)
	  (or (< (point) here)
	      (c-safe (backward-up-list 1) t)
	      (goto-char lim))
	  (setq c-parsing-error
		(format "No matching `if' found for `else' on line %d"
			(1+ (count-lines 1 here))))
	  nil))))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  (c-with-syntax-table c-no-escape-syntax-table
    (c-forward-sexp (cond
		     ;; else if()
		     ((looking-at (concat "\\<else"
					  "\\([ \t\n]\\|\\\\\n\\)+"
					  "if\\>\\([^_]\\|$\\)"))
		      3)
		     ;; do, else, try, finally
		     ((looking-at (concat "\\<\\("
					  "do\\|else\\|try\\|finally"
					  "\\)\\>\\([^_]\\|$\\)"))
		      1)
		     ;; for, if, while, switch, catch, synchronized, foreach
		     (t 2)))))

(defun c-beginning-of-closest-statement (&optional lim)
  ;; Go back to the closest preceding statement start.
  (let ((start (point))
	(label-re (concat c-label-key "\\|"
			  c-switch-label-key))
	stmtbeg)
    (if c-access-key
	(setq label-re (concat label-re "\\|" c-access-key)))
    (c-beginning-of-statement-1 lim)
    (while (and (when (<= (point) start)
		  (setq stmtbeg (point)))
		(cond
		 ((looking-at label-re)
		  ;; Skip a label.
		  (goto-char (match-end 0))
		  t)
		 ((looking-at c-conditional-key)
		  ;; Skip a conditional statement.
		  (c-safe (c-skip-conditional) t))
		 (t nil)))
      (c-forward-syntactic-ws start))
    (if stmtbeg
	(goto-char stmtbeg))))

(defun c-beginning-of-member-init-list (&optional limit)
  ;; Goes to the beginning of a member init list (i.e. just after the
  ;; ':') if inside one. Returns t in that case, nil otherwise.
  (or limit
      (setq limit (point-min)))
  (skip-chars-forward " \t")
  (if (eq (char-after) ?,)
      (forward-char 1)
    (c-backward-syntactic-ws limit))
  (while (and (< limit (point))
	      (eq (char-before) ?,))
    ;; this will catch member inits with multiple
    ;; line arglists
    (forward-char -1)
    (c-backward-syntactic-ws limit)
    (if (eq (char-before) ?\))
	(c-backward-sexp 1))
    (c-backward-syntactic-ws limit)
    ;; Skip over any template arg to the class.
    (if (eq (char-before) ?>)
	(c-with-syntax-table c++-template-syntax-table
	  (c-backward-sexp 1)))
    (c-backward-sexp 1)
    (c-backward-syntactic-ws limit)
    ;; Skip backwards over a fully::qualified::name.
    (while (and (eq (char-before) ?:)
		(save-excursion
		  (forward-char -1)
		  (eq (char-before) ?:)))
      (backward-char 2)
      (c-backward-sexp 1))
    ;; now continue checking
    (c-backward-syntactic-ws limit))
  (and (< limit (point))
       (eq (char-before) ?:)))

(defun c-skip-case-statement-forward (state &optional lim)
  ;; skip forward over case/default bodies, with optional maximal
  ;; limit. if no next case body is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp bufpos
	(safepos (point))
	(balanced (car state)))
    ;; search until we've passed the limit, or we've found our match
    (while (and (< (point) lim)
		(not donep))
      (setq safepos (point))
      ;; see if we can find a case statement, not in a literal
      (if (and (re-search-forward c-switch-label-key lim 'move)
	       (setq bufpos (match-beginning 0))
	       (not (c-in-literal safepos))
	       (/= bufpos here))
	  ;; if we crossed into a balanced sexp, we know the case is
	  ;; not part of our switch statement, so just bound over the
	  ;; sexp and keep looking.
	  (if (and (consp balanced)
		   (> bufpos (car balanced))
		   (< bufpos (cdr balanced)))
	      (goto-char (cdr balanced))
	    (goto-char bufpos)
	    (setq donep t
		  foundp t))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-search-uplist-for-classkey (brace-state)
  ;; search for the containing class, returning a 2 element vector if
  ;; found. aref 0 contains the bufpos of the boi of the class key
  ;; line, and aref 1 contains the bufpos of the open brace.
  (if (null brace-state)
      ;; no brace-state means we cannot be inside a class
      nil
    (let ((carcache (car brace-state))
	  search-start search-end)
      (if (consp carcache)
	  ;; a cons cell in the first element means that there is some
	  ;; balanced sexp before the current bufpos. this we can
	  ;; ignore. the nth 1 and nth 2 elements define for us the
	  ;; search boundaries
	  (setq search-start (nth 2 brace-state)
		search-end (nth 1 brace-state))
	;; if the car was not a cons cell then nth 0 and nth 1 define
	;; for us the search boundaries
	(setq search-start (nth 1 brace-state)
	      search-end (nth 0 brace-state)))
      ;; search-end cannot be a cons cell
      (and (consp search-end)
	   (error "consp search-end: %s" search-end))
      ;; if search-end is nil, or if the search-end character isn't an
      ;; open brace, we are definitely not in a class
      (if (or (not search-end)
	      (< search-end (point-min))
	      (not (eq (char-after search-end) ?{)))
	  nil
	;; now, we need to look more closely at search-start.  if
	;; search-start is nil, then our start boundary is really
	;; point-min.
	(if (not search-start)
	    (setq search-start (point-min))
	  ;; if search-start is a cons cell, then we can start
	  ;; searching from the end of the balanced sexp just ahead of
	  ;; us
	  (if (consp search-start)
	      (setq search-start (cdr search-start))))
	;; now we can do a quick regexp search from search-start to
	;; search-end and see if we can find a class key.  watch for
	;; class like strings in literals
	(save-excursion
	  (save-restriction
	    (goto-char search-start)
	    (let ((search-key (concat c-class-key "\\|" c-extra-toplevel-key))
		  foundp class match-end)
	      (if c-inexpr-class-key
		  (setq search-key (concat search-key "\\|"
					   c-inexpr-class-key)))
	      (while (and (not foundp)
			  (progn
			    (c-forward-syntactic-ws search-end)
			    (> search-end (point)))
			  (re-search-forward search-key search-end t))
		(setq class (match-beginning 0)
		      match-end (match-end 0))
		(if (c-in-literal search-start)
		    nil			; its in a comment or string, ignore
		  (goto-char class)
		  (c-skip-ws-forward)
		  (setq foundp (vector (c-point 'boi) search-end))
		  (cond
		   ;; check for embedded keywords
		   ((let ((char (char-after (1- class))))
		      (and char
			   (memq (char-syntax char) '(?w ?_))))
		    (goto-char match-end)
		    (setq foundp nil))
		   ;; make sure we're really looking at the start of a
		   ;; class definition, and not a forward decl, return
		   ;; arg, template arg list, or an ObjC or Java method.
		   ((and c-method-key
			 (re-search-forward c-method-key search-end t)
			 (not (c-in-literal class)))
		    (setq foundp nil))
		   ;; Check if this is an anonymous inner class.
		   ((and c-inexpr-class-key
			 (looking-at c-inexpr-class-key))
		    (while (and (= (c-forward-token-1 1 t) 0)
				(looking-at "(\\|\\w\\|\\s_\\|\\.")))
		    (if (eq (point) search-end)
			;; We're done.  Just trap this case in the cond.
			nil
		      ;; False alarm; all conditions aren't satisfied.
		      (setq foundp nil)))
		   ;; Its impossible to define a regexp for this, and
		   ;; nearly so to do it programmatically.
		   ;;
		   ;; ; picks up forward decls
		   ;; = picks up init lists
		   ;; ) picks up return types
		   ;; > picks up templates, but remember that we can
		   ;;   inherit from templates!
		   ((let ((skipchars "^;=)"))
		      ;; try to see if we found the `class' keyword
		      ;; inside a template arg list
		      (save-excursion
			(skip-chars-backward "^<>" search-start)
			(if (eq (char-before) ?<)
			    (setq skipchars (concat skipchars ">"))))
		      (while (progn
			       (skip-chars-forward skipchars search-end)
			       (c-in-literal class))
			(forward-char))
		      (/= (point) search-end))
		    (setq foundp nil))
		   )))
	      foundp))
	  )))))

(defun c-inside-bracelist-p (containing-sexp brace-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  BRACE-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; places in inconvenient locations.  Its a trade-off we make for
  ;; speed.
  (or
   ;; this will pick up enum lists
   (c-safe
    (save-excursion
      (goto-char containing-sexp)
      (c-forward-sexp -1)
      (let (bracepos)
	(if (and (or (looking-at "enum[\t\n ]+")
		     (progn (c-forward-sexp -1)
			    (looking-at "enum[\t\n ]+")))
		 (setq bracepos (c-down-list-forward (point)))
		 (not (c-crosses-statement-barrier-p (point)
						     (- bracepos 2))))
	    (point)))))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
	    ;; Pike can have class definitions anywhere, so we must
	    ;; check for the class key here.
	    (and (c-major-mode-is 'pike-mode)
		 (concat c-class-key "\\|" c-extra-toplevel-key)))
	   bufpos lim braceassignp)
       (while (and (not bufpos)
		   containing-sexp)
	 (if (consp containing-sexp)
	     (setq containing-sexp (car brace-state)
		   brace-state (cdr brace-state))
	   (goto-char containing-sexp)
	   (if (c-looking-at-inexpr-block)
	       ;; We're in an in-expression block of some kind.  Do
	       ;; not check nesting.
	       (setq containing-sexp nil)
	     ;; see if the open brace is preceded by = or [...] in
	     ;; this statement, but watch out for operator=
	     (setq lim (if (consp (car brace-state))
			   (cdr (car brace-state))
			 (car brace-state))
		   braceassignp 'dontknow)
	     (c-backward-token-1 1 t lim)
	     ;; Checks to do only on the first sexp before the brace.
	     (when (and (c-major-mode-is 'java-mode)
			(eq (char-after) ?\[))
	       ;; In Java, an initialization brace list may follow
	       ;; directly after "new Foo[]", so check for a "new"
	       ;; earlier.
	       (while (eq braceassignp 'dontknow)
		 (setq braceassignp
		       (cond ((/= (c-backward-token-1 1 t lim) 0) nil)
			     ((looking-at "new\\>[^_]") t)
			     ((looking-at "\\sw\\|\\s_\\|[.[]")
			      ;; Carry on looking if this is an
			      ;; identifier (may contain "." in Java)
			      ;; or another "[]" sexp.
			      'dontknow)
			     (t nil)))))
	     ;; Checks to do on all sexps before the brace, up to the
	     ;; beginning of the statement.
	     (while (eq braceassignp 'dontknow)
	       (cond ((eq (char-after) ?\;)
		      (setq braceassignp nil))
		     ((and class-key
			   (looking-at class-key))
		      (setq braceassignp nil))
		     ((eq (char-after) ?=)
		      ;; We've seen a =, but must check earlier tokens so
		      ;; that it isn't something that should be ignored.
		      (setq braceassignp 'maybe)
		      (while (and (eq braceassignp 'maybe)
				  (zerop (c-backward-token-1 1 t lim)))
			(setq braceassignp
			      (cond
			       ;; Check for operator =
			       ((looking-at "operator\\>") nil)
			       ;; Check for `<opchar>= in Pike.
			       ((and (c-major-mode-is 'pike-mode)
				     (or (eq (char-after) ?`)
					 ;; Special case for Pikes
					 ;; `[]=, since '[' is not in
					 ;; the punctuation class.
					 (and (eq (char-after) ?\[)
					      (eq (char-before) ?`))))
				nil)
			       ((looking-at "\\s.") 'maybe)
			       ;; make sure we're not in a C++ template
			       ;; argument assignment
			       ((and (c-major-mode-is 'c++-mode)
				     (save-excursion
				       (let ((here (point))
					     (pos< (progn
						     (skip-chars-backward "^<>")
						     (point))))
					 (and (eq (char-before) ?<)
					      (not (c-crosses-statement-barrier-p
						    pos< here))
					      (not (c-in-literal))
					      ))))
				nil)
			       (t t))))))
	       (if (and (eq braceassignp 'dontknow)
			(/= (c-backward-token-1 1 t lim) 0))
		   (setq braceassignp nil)))
	     (if (not braceassignp)
		 (if (eq (char-after) ?\;)
		     ;; Brace lists can't contain a semicolon, so we're done.
		     (setq containing-sexp nil)
		   ;; lets see if we're nested. find the most nested
		   ;; containing brace
		   (setq containing-sexp (car brace-state)
			 brace-state (cdr brace-state)))
	       ;; we've hit the beginning of the aggregate list
	       (c-beginning-of-statement-1
		(c-most-enclosing-brace brace-state))
	       (setq bufpos (point))))
	   ))
       bufpos))
   ))

(defun c-looking-at-special-brace-list (&optional lim)
  ;; If we're looking at the start of a pike-style list, ie `({ })',
  ;; `([ ])', `(< >)' etc, a cons of a cons of its starting and ending
  ;; positions and its entry in c-special-brace-lists is returned, nil
  ;; otherwise.  The ending position is nil if the list is still open.
  ;; LIM is the limit for forward search.  The point may either be at
  ;; the `(' or at the following paren character.  Tries to check the
  ;; matching closer, but assumes it's correct if no balanced paren is
  ;; found (i.e. the case `({ ... } ... )' is detected as _not_ being
  ;; a special brace list).
  (if c-special-brace-lists
      (condition-case ()
	  (save-excursion
	    (let ((beg (point))
		  end type)
	      (c-forward-syntactic-ws)
	      (if (eq (char-after) ?\()
		  (progn
		    (forward-char 1)
		    (c-forward-syntactic-ws)
		    (setq type (assq (char-after) c-special-brace-lists)))
		(if (setq type (assq (char-after) c-special-brace-lists))
		    (progn
		      (c-backward-syntactic-ws)
		      (forward-char -1)
		      (setq beg (if (eq (char-after) ?\()
				    (point)
				  nil)))))
	      (if (and beg type)
		  (if (and (c-safe (goto-char beg)
				   (c-forward-sexp 1)
				   (setq end (point))
				   (= (char-before) ?\)))
			   (c-safe (goto-char beg)
				   (forward-char 1)
				   (c-forward-sexp 1)
				   ;; Kludges needed to handle inner
				   ;; chars both with and without
				   ;; paren syntax.
				   (or (/= (char-syntax (char-before)) ?\))
				       (= (char-before) (cdr type)))))
		      (if (or (/= (char-syntax (char-before)) ?\))
			      (= (progn
				   (c-forward-syntactic-ws)
				   (point))
				 (1- end)))
			  (cons (cons beg end) type))
		    (cons (list beg) type)))))
	(error nil))))

(defun c-looking-at-bos ()
  ;; Returns nil if inside a statement or declaration.
  (save-excursion
    (c-backward-syntactic-ws)
    (or (bobp)
	(memq (char-before) '(?\; ?}))
	(and (eq (char-before) ?{)
	     (not (and c-special-brace-lists
		       (progn (backward-char)
			      (c-looking-at-special-brace-list))))))))

(defun c-looking-at-inexpr-block (&optional lim)
  ;; Returns non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.  LIM limits the
  ;; backward search.
  (save-excursion
    (or lim (setq lim (point-min)))
    (let ((block-follows (eq (char-after) ?{)))
      ;; Look at the character after point only as a last resort when
      ;; we can't disambiguate.
      (if (and block-follows
	       (progn (c-backward-syntactic-ws) (> (point) lim))
	       (eq (char-before) ?\())
	  (if (and c-special-brace-lists
		   (c-looking-at-special-brace-list))
	      nil
	    (cons 'inexpr-statement (point)))
	(let ((res 'maybe) passed-bracket)
	  (while (and (eq res 'maybe)
		      (= (c-backward-token-1 1 t lim) 0)
		      (>= (point) lim)
		      (looking-at "[\(\[.]\\|\\w\\|\\s_"))
	    (setq res
		  (cond ((and block-follows
			      c-inexpr-class-key
			      (looking-at c-inexpr-class-key))
			 (and (not passed-bracket)
			      (or (not (looking-at c-class-key))
				  (let ((prev (point)))
				    (while (and (= (c-backward-token-1 1 t lim)
						   0)
						(>= (point) lim)
						(eq (char-syntax (char-after))
						    ?w))
				      (setq prev (point)))
				    (goto-char prev)
				    (not (c-looking-at-bos))))
			      (cons 'inexpr-class (point))))
			((and c-inexpr-block-key
			      (looking-at c-inexpr-block-key))
			 (cons 'inexpr-statement (point)))
			((and c-lambda-key
			      (looking-at c-lambda-key))
			 (cons 'inlambda (point)))
			(t
			 (if (eq (char-after) ?\[)
			     (setq passed-bracket t))
			 'maybe))))
	  (if (eq res 'maybe)
	      (when (and block-follows
			 (save-restriction
			   (narrow-to-region lim (point))
			   (c-safe (goto-char (scan-lists (point) -1 1)) t))
			 (eq (char-after) ?\())
		(cons 'inexpr-statement (point)))
	    res))))))

(defun c-looking-at-inexpr-block-backward (&optional lim)
  ;; Returns non-nil if we're looking at the end of an in-expression
  ;; block, otherwise the same as `c-looking-at-inexpr-block'.
  (save-excursion
    (let ((lim (or lim (c-point 'bod))))
      (c-safe
       (c-backward-syntactic-ws lim)
       (if (eq (char-before) ?})	; Recognize only a block currently.
	   (progn
	     (c-forward-sexp -1)
	     (if (>= (point) lim)
		 (c-looking-at-inexpr-block lim))))))))

(defun c-on-identifier ()
  ;; Returns non-nil if we're on or directly after an identifier.
  (if (or (memq (char-syntax (or (char-after) ? )) '(?w ?_))
	  (memq (char-syntax (or (char-before) ? )) '(?w ?_)))
      (save-excursion
	(skip-syntax-backward "w_")
	(not (looking-at c-keywords)))
    (if (c-major-mode-is 'pike-mode)
	;; Handle the `<operator> syntax in Pike.
	(save-excursion
	  (if (eq (char-after) ?\`) (forward-char))
	  (skip-chars-backward "!%&*+\\-/<=>^|~")
	  (let ((pos (point)))
	    (cond ((memq (char-before) '(?\) ?\]))
		   (c-safe (backward-char 2)))
		  ((memq (char-before) '(?\( ?\[))
		   (c-safe (backward-char 1))))
	    (if (not (looking-at "()\\|\\[]"))
		(goto-char pos)))
	  (and (eq (char-before) ?\`)
	       (looking-at "[-!%&*+/<=>^|~]\\|()\\|\\[]"))))))


(defun c-most-enclosing-brace (state &optional bufpos)
  ;; Return the bufpos of the innermost enclosing brace before bufpos
  ;; that hasn't been narrowed out by any enclosing class, or nil if
  ;; none was found.
  (let (enclosingp)
    (or bufpos (setq bufpos 134217727))
    (while state
      (setq enclosingp (car state)
	    state (cdr state))
      (if (or (consp enclosingp)
	      (>= enclosingp bufpos))
	  (setq enclosingp nil)
	(if (< enclosingp (point-min))
	    (setq enclosingp nil))
	(setq state nil)))
    enclosingp))

(defun c-least-enclosing-brace (state &optional bufpos)
  ;; Return the bufpos of the outermost enclosing brace before bufpos
  ;; that hasn't been narrowed out by any enclosing class, or nil if
  ;; none was found.  Note: Destructive on the passed list.
  (c-most-enclosing-brace (nreverse state) bufpos))

(defun c-safe-position (bufpos state)
  ;; return the closest known safe position higher up than bufpos
  (let ((c-macro-start (c-query-macro-start)) safepos)
    (if (and c-macro-start
	     (< c-macro-start bufpos))
	;; Make sure bufpos is outside the macro we might be in.
	(setq bufpos c-macro-start))
    (while state
      (setq safepos
	    (if (consp (car state))
		(cdr (car state))
	      (car state)))
      (if (< safepos bufpos)
	  (setq state nil)
	(setq state (cdr state))))
    ;; Normally bufpos is inside the state, but if we backed up
    ;; outside the macro it might not be.  We know the macro is at the
    ;; top level in this case, so we can use the macro start as the
    ;; safe position.
    (or c-macro-start
	safepos)))

(defun c-narrow-out-enclosing-class (state lim)
  ;; narrow the buffer so that the enclosing class is hidden
  (setq state (c-whack-state-after (point) state))
  (let (inclass-p)
    (and state
	 (setq inclass-p (c-search-uplist-for-classkey state))
	 (narrow-to-region
	  (progn
	    (goto-char (1+ (aref inclass-p 1)))
	    (c-skip-ws-forward lim)
	    ;; if point is now left of the class opening brace, we're
	    ;; hosed, so try a different tact
	    (if (<= (point) (aref inclass-p 1))
		(progn
		  (goto-char (1+ (aref inclass-p 1)))
		  (c-forward-syntactic-ws lim)))
	    (point))
	  ;; end point is the end of the current line
	  (progn
	    (goto-char lim)
	    (c-point 'eol))))
    ;; return the class vector
    inclass-p))


(defvar syntax)
(defvar syntactic-relpos)

(defun c-guess-continued-construct (indent-point
				    char-after-ip
				    placeholder
				    after-cond-placeholder
				    containing-sexp
				    lim)
  ;; This function contains the decision tree reached through both
  ;; CASE 18 and CASE 10.  It's a continued statement or top level
  ;; construct of some kind.
  (let (special-brace-list)
    (goto-char indent-point)
    (skip-chars-forward " \t")
    (cond
     ;; (CASE A removed.)
     ;; CASE B: open braces for class or brace-lists
     ((setq special-brace-list
	    (or (and c-special-brace-lists
		     (c-looking-at-special-brace-list))
		(eq char-after-ip ?{)))
      (cond
       ;; CASE B.1: class-open
       ((save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t{")
	  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
	    (and decl
		 (setq placeholder (aref decl 0)))
	    ))
	(c-add-syntax 'class-open placeholder))
       ;; CASE B.2: brace-list-open
       ((or (consp special-brace-list)
	    (save-excursion
	      (goto-char placeholder)
	      (looking-at "enum\\>[^_]"))
	    (save-excursion
	      (goto-char indent-point)
	      (while (and (> (point) placeholder)
			  (= (c-backward-token-1 1 t) 0)
			  (/= (char-after) ?=)))
	      (eq (char-after) ?=)))
	;; The most semantically accurate symbol here is
	;; brace-list-open, but we report it simply as a statement-cont.
	;; The reason is that one normally adjusts brace-list-open for
	;; brace lists as top-level constructs, and brace lists inside
	;; statements is a completely different context.
	(goto-char indent-point)
	(c-beginning-of-closest-statement)
	(c-add-syntax 'statement-cont (c-point 'boi)))
       ;; CASE B.3: The body of a function declared inside a normal
       ;; block.  This can only occur in Pike.
       ((and (c-major-mode-is 'pike-mode)
	     (progn
	       (goto-char indent-point)
	       (not (c-looking-at-bos))))
	(c-beginning-of-closest-statement)
	(c-add-syntax 'defun-open (c-point 'boi)))
       ;; CASE B.4: catch-all for unknown construct.
       (t
	;; Can and should I add an extensibility hook here?  Something
	;; like c-recognize-hook so support for unknown constructs could
	;; be added.  It's probably a losing proposition, so I dunno.
	(goto-char placeholder)
	(c-add-syntax 'statement-cont (c-point 'boi))
	(c-add-syntax 'block-open))
       ))
     ;; CASE C: iostream insertion or extraction operator
     ((looking-at "<<\\|>>")
      (goto-char (or after-cond-placeholder placeholder))
      (while (and (re-search-forward "<<\\|>>" indent-point 'move)
		  (c-in-literal placeholder)))
      ;; if we ended up at indent-point, then the first streamop is on a
      ;; separate line. Indent the line like a statement-cont instead
      (if (/= (point) indent-point)
	  (c-add-syntax 'stream-op (c-point 'boi))
	(c-backward-syntactic-ws lim)
	(c-add-syntax 'statement-cont (c-point 'boi))))
     ;; CASE D: continued statement. find the accurate beginning of
     ;; statement or substatement
     (t
      (c-beginning-of-statement-1 after-cond-placeholder)
      ;; KLUDGE ALERT!  c-beginning-of-statement-1 can leave
      ;; us before the lim we're passing in.  It should be
      ;; fixed, but I'm worried about side-effects at this
      ;; late date.  Fix for v5.
      (goto-char (or (and after-cond-placeholder
			  (max after-cond-placeholder (point)))
		     (point)))
      (c-add-syntax 'statement-cont (point)))
     )))

;; This function implements the main decision tree for determining the
;; syntactic analysis of the current line of code.  Yes, it's huge and
;; bloated!

(defun c-guess-basic-syntax ()
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (fullstate (c-parse-state))
 	     (state fullstate)
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache inswitch-p
	     tmpsymbol keyword injava-inher special-brace-list
	     ;; narrow out any enclosing class or extern "C" block
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     ;; c-state-cache is shadowed here.  That means we must
	     ;; not do any changes during the execution of this
	     ;; function, since c-check-state-cache then would change
	     ;; this local variable and leave a bogus value in the
	     ;; global one.
	     (c-state-cache (if inclass-p
				(c-whack-state-before (point-min) fullstate)
			      fullstate))
	     inenclosing-p macro-start
	     ;; There's always at most one syntactic element which got
	     ;; a relpos.  It's stored in syntactic-relpos.
	     syntactic-relpos)
	;; check for meta top-level enclosing constructs, possible
	;; extern language definitions, possibly (in C++) namespace
	;; definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (if (and inclass-p
		     (progn
		       (goto-char (aref inclass-p 0))
		       (looking-at (concat c-extra-toplevel-key "[^_]"))))
		(let ((enclosing (match-string 1)))
		  (cond
		   ((string-equal enclosing "extern")
		    (setq inenclosing-p 'extern))
		   ((string-equal enclosing "namespace")
		    (setq inenclosing-p 'namespace))
		   )))))
	;; get the buffer position of the most nested opening brace,
	;; if there is one, and it hasn't been narrowed out
	(when state
	  (setq containing-sexp (car state)
		state (cdr state))
	  (if (consp containing-sexp)
	      ;; Ignore balanced paren.  The next entry can't be
	      ;; another one.
	      (setq containing-sexp (car-safe state)
		    state (cdr-safe state)))
	  ;; Ignore the bufpos if it has been narrowed out by the
	  ;; containing class.
	  (if (and containing-sexp
		   (< containing-sexp (point-min)))
	      (setq containing-sexp nil)))

	;; set the limit on the farthest back we need to search
	(setq lim (or containing-sexp
		      (if (consp (car fullstate))
			  (cdr (car fullstate))
			nil)
		      (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((eq literal 'string)
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  (c-add-syntax literal (car (c-literal-limits lim))))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (save-excursion
		 (when (c-beginning-of-macro)
		   (setq macro-start (point))))
	       (/= macro-start (c-point 'boi))
	       (or (not c-syntactic-indentation-in-macros)
		   (save-excursion
		     (goto-char macro-start)
		     (and (c-forward-to-cpp-expression)
			  (= (point) (c-point 'boi indent-point))))))
	  (c-add-syntax 'cpp-macro-cont macro-start)
	  (setq macro-start nil))
	 ;; CASE 4: In-expression statement.
	 ((and (or c-inexpr-class-key c-inexpr-block-key c-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block)))
	  (setq tmpsymbol (assq (car placeholder)
				'((inexpr-class . class-open)
				  (inexpr-statement . block-open))))
	  (if tmpsymbol
	      ;; It's a statement block or an anonymous class.
	      (setq tmpsymbol (cdr tmpsymbol))
	    ;; It's a Pike lambda.  Check whether we are between the
	    ;; lambda keyword and the argument list or at the defun
	    ;; opener.
	    (setq tmpsymbol (if (eq char-after-ip ?{)
				'inline-open
			      'lambda-intro-cont)))
	  (goto-char (cdr placeholder))
	  (back-to-indentation)
	  (c-add-syntax tmpsymbol (point))
	  (unless (eq (point) (cdr placeholder))
	    (c-add-syntax (car placeholder))))
	 ;; CASE 11: an else clause?
	 ((looking-at "else\\>[^_]")
	  (c-backward-to-start-of-if lim)
	  (c-add-syntax 'else-clause (c-point 'boi)))
	 ;; CASE 12: while closure of a do/while construct?
	 ((and (looking-at "while\\>[^_]")
	       (save-excursion
		 (c-backward-to-start-of-do lim)
		 (setq placeholder (point))
		 (looking-at "do\\>[^_]")))
	  (goto-char placeholder)
	  (c-add-syntax 'do-while-closure (c-point 'boi)))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\(catch\\|finally\\)\\>[^_]")))
		 (c-safe (c-backward-sexp) t)
		 (eq (char-after) ?{)
		 (c-safe (c-backward-sexp) t)
		 (if (eq (char-after) ?\()
		     (c-safe (c-backward-sexp) t)
		   t)
		 (looking-at "\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (c-point 'boi))))
	  (c-add-syntax 'catch-clause placeholder))
	 ;; CASE 18: A substatement we can recognize by keyword.
	 ((save-excursion
	    (and c-conditional-key
		 (not (memq char-before-ip '(?\; ?:)))
		 (or (not (eq char-before-ip ?}))
		     (c-looking-at-inexpr-block-backward containing-sexp))
		 (> (point)
		    (progn
		      ;; Ought to cache the result here.
		      (c-beginning-of-statement-1 lim)
		      ;;(c-forward-syntactic-ws)
		      (setq placeholder (point))))
		 (if (looking-at c-block-stmt-2-kwds)
		     ;; Require a parenthesis after these keywords.
		     ;; Necessary to catch e.g. synchronized in Java,
		     ;; which can be used both as statement and
		     ;; modifier.
		     (and (= (c-forward-token-1 1 nil) 0)
			  (eq (char-after) ?\())
		   (looking-at c-conditional-key))))
	  (let ((after-cond-placeholder
		 (save-excursion
		   (goto-char placeholder)
		   (c-safe (c-skip-conditional))
		   (c-forward-syntactic-ws)
		   (if (eq (char-after) ?\;)
		       (progn
			 (forward-char 1)
			 (c-forward-syntactic-ws)))
		   (point))))
	    (if (>= after-cond-placeholder indent-point)
		;; CASE 18A: Simple substatement.
		(progn
		  (goto-char placeholder)
		  (if (eq char-after-ip ?{)
		      (c-add-syntax 'substatement-open (c-point 'boi))
		    (c-add-syntax 'substatement (c-point 'boi))))
	      ;; CASE 18B: Some other substatement.  This is shared
	      ;; with CASE 10.
	      (c-guess-continued-construct indent-point
					   char-after-ip
					   placeholder
					   after-cond-placeholder
					   lim
					   lim))))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, brace list, class,
	   ;; or inline-inclass method opening brace
	   ((setq special-brace-list
		  (or (and c-special-brace-lists
			   (c-looking-at-special-brace-list))
		      (eq char-after-ip ?{)))
	    (cond
	     ;; CASE 5A.1: extern language or namespace construct
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (progn (c-backward-sexp 2) t))
		     (looking-at (concat c-extra-toplevel-key "[^_]"))
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (or (and (string-equal keyword "namespace")
			      (setq tmpsymbol 'namespace-open))
			 (and (string-equal keyword "extern")
			      (progn
				(c-forward-sexp 1)
				(c-forward-syntactic-ws)
				(eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open)))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		;; c-b-o-s could have left us at point-min
		(and (bobp)
		     (c-forward-syntactic-ws indent-point))
		(if (looking-at "typedef[^_]")
		    (progn (c-forward-sexp 1)
			   (c-forward-syntactic-ws indent-point)))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (= (c-backward-token-1 1 t) 0)
					   (/= (char-after) ?=))
				 (if (and (not tmpsymbol)
					  (looking-at "new\\>[^_]"))
				     (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at "enum\\>[^_]"))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (= (c-forward-token-1 1 t) 0)
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case 10B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-forward-syntactic-ws)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if inclass-p
		  (c-add-syntax 'defun-open (c-point 'boi))
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (eq char-before-ip ?:)
		  (eq char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (eq char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (eq (char-before) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (eq (char-before) ?\))
		  (c-backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (c-backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (c-backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p (c-add-class-syntax 'inclass inclass-p)))
	     ;; CASE 5B.3: Inside a member init list.
	     ((c-beginning-of-member-init-list lim)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5B.4: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and c-baseclass-key
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-baseclass-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (progn (forward-char 1) t))
				(not (eq (char-after) ?:))
				)))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (eq char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key)))
		;; for Java
		(and (c-major-mode-is 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-Java-special-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (c-forward-sexp -1) t))
				      (<= (point) fence))
				  (setq done t))
				 )
			   (setq cont t)))
		       injava-inher)
		     (not (c-crosses-statement-barrier-p (cdr injava-inher)
							 (point)))
		     ))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((eq char-after-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p (c-add-class-syntax 'inclass inclass-p)))
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws\\>[^_]")
		       (c-add-syntax 'func-decl-cont
				     (progn (c-beginning-of-statement-1 lim)
					    (c-point 'boi))))
		      (cont (c-add-syntax 'inher-cont where))
		      (t (c-add-syntax 'inher-intro
				       (progn (goto-char (cdr injava-inher))
					      (c-beginning-of-statement-1 lim)
					      (point))))
		      )))
	     ;; CASE 5C.4: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level compound statement, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		(while (and (= (c-backward-token-1 1 t lim) 0)
			    (not (looking-at "[;{<,]"))))
		(or (eq (char-after) ?,)
		    (and (= (c-backward-token-1 1 nil lim) 0)
			 (eq (char-after) ?<)))))
	    (goto-char indent-point)
	    (c-beginning-of-member-init-list lim)
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-1 1 t lim)
		     (and (eq (char-after) ?:)
			  (not (eq (char-before) ?:))))
		   (save-excursion
		     (goto-char placeholder)
		     (back-to-indentation)
		     (or
		      (/= (car (save-excursion
				 (parse-partial-sexp (point) placeholder)))
			  0)
		      (and
		       (if c-access-key (not (looking-at c-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-bitfield-key (not (looking-at c-bitfield-key)) t))
		      )))
	      (goto-char placeholder)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a template list continuation?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (save-restriction
		       (c-with-syntax-table c++-template-syntax-table
			 (goto-char indent-point)
			 (setq placeholder (c-up-list-backward (point)))
			 (and placeholder
			      (eq (char-after placeholder) ?<))))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a multiple inheritance line?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (c-beginning-of-statement-1 lim)
		     (setq placeholder (point))
		     (if (looking-at "\\<static\\>")
			 (c-forward-token-1 1 nil indent-point))
		     (and (looking-at c-class-key)
			  (= (c-forward-token-1 2 nil indent-point) 0)
			  (if (eq (char-after) ?<)
			      (c-with-syntax-table c++-template-syntax-table
				(= (c-forward-token-1 1 t indent-point) 0))
			    t)
			  (eq (char-after) ?:))))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement-1 lim)
	      ;; skip over any access-specifiers
	      (and inclass-p c-access-key
		   (while (looking-at c-access-key)
		     (forward-line 1)))
	      ;; skip over comments, whitespace
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-access-key
		 (looking-at c-access-key))
	    (c-add-syntax 'access-label (c-point 'bonl))
	    (c-add-class-syntax 'inclass inclass-p))
	   ;; CASE 5F: extern-lang-close or namespace-close?
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (setq tmpsymbol (if (eq inenclosing-p 'extern)
				'extern-lang-close
			      'namespace-close))
	    (c-add-syntax tmpsymbol (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (progn (c-backward-sexp 1) t))
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 ;; here we essentially use the hack that is used in
		 ;; Emacs' c-mode.el to limit how far back we should
		 ;; look.  The assumption is made that argdecls are
		 ;; indented at least one space and that function
		 ;; headers are not indented.
		 (let ((limit (save-excursion
				(re-search-backward "^[^ \^L\t\n#]" nil 'move)
				(point))))
		   (save-excursion
		     (c-backward-syntactic-ws limit)
		     (setq placeholder (point))
		     (while (and (memq (char-before) '(?\; ?,))
				 (> (point) limit))
		       (beginning-of-line)
		       (setq placeholder (point))
		       (c-backward-syntactic-ws limit))
		     (and (eq (char-before) ?\))
			  (or (not c-method-key)
			      (progn
				(c-forward-sexp -1)
				(forward-char -1)
				(c-backward-syntactic-ws)
				(not (or (memq (char-before) '(?- ?+))
					 ;; or a class category
					 (progn
					   (c-forward-sexp -2)
					   (looking-at c-class-key))
					 )))))
		     ))
		 (save-excursion
		   (c-beginning-of-statement-1)
		   (not (looking-at "typedef\\>[^_]"))))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (c-point 'boi)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-method-key
		 (looking-at c-method-key))
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
	   ;; CASE 5J: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  c-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (c-backward-sexp 1) t))
			    (looking-at c-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (char-before) '(?\; ?\}))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      ;; Using bol instead of boi above is highly bogus, and
	      ;; it makes our lives hard to remain compatible. :P
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (cond
		     ((eq inenclosing-p 'extern)
		      (c-add-syntax 'inextern-lang (c-point 'boi)))
		     ((eq inenclosing-p 'namespace)
		      (c-add-syntax 'innamespace (c-point 'boi)))
		     (t (c-add-class-syntax 'inclass inclass-p)))
		    ))
	      (when (and c-syntactic-indentation-in-macros
			 macro-start
			 (/= macro-start (c-point 'boi indent-point)))
		(c-add-syntax 'cpp-macro-cont)
		(setq macro-start nil))
	      ))
	   ;; CASE 5K: we are at an ObjC or Java method definition
	   ;; continuation line.
	   ((and c-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))				; end CASE 5
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (c-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren
	   ((and (or (c-major-mode-is 'pike-mode)
		     ;; Don't check this in Pike since it allows a
		     ;; comma after the last arg.
		     (not (eq char-before-ip ?,)))
		 (memq char-after-ip '(?\) ?\])))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-close placeholder))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.
	   ((eq char-after-ip ?{)
	    (goto-char indent-point)
	    (setq placeholder (c-point 'boi))
	    (goto-char containing-sexp)
	    (if (c-inside-bracelist-p placeholder
				      (cons containing-sexp state))
		(progn
		  (c-add-syntax 'brace-list-open (c-point 'boi))
		  (c-add-syntax 'inexpr-class))
	      (c-add-syntax 'block-open (c-point 'boi))
	      (c-add-syntax 'inexpr-statement)))
	   ;; CASE 7C: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-intro placeholder))
	   ;; CASE 7D: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((save-excursion
	      (goto-char containing-sexp)
	      (and (c-safe (progn (c-forward-sexp -1) t))
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (c-beginning-of-statement-1 containing-sexp)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued method call. This is the case
	   ;; when we are inside a [] bracketed exp, and what precede
	   ;; the opening bracket is not an identifier.
	   ((and c-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (save-excursion
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((save-excursion
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (not (eolp)))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-cont-nonempty placeholder))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-beginning-of-statement-1 containing-sexp)
	      (forward-char 1)
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and c-baseclass-key
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-baseclass-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 9: we are inside a brace-list
	 ((setq special-brace-list
		(or (and c-special-brace-lists
			 (save-excursion
			   (goto-char containing-sexp)
			   (c-looking-at-special-brace-list)))
		    (c-inside-bracelist-p containing-sexp state)))
	  (cond
	   ;; CASE 9A: In the middle of a special brace list opener.
	   ((and (consp special-brace-list)
		 (save-excursion
		   (goto-char containing-sexp)
		   (eq (char-after) ?\())
		 (eq char-after-ip (car (cdr special-brace-list))))
	    (goto-char (car (car special-brace-list)))
	    (skip-chars-backward " \t")
	    (if (and (bolp)
		     (assoc 'statement-cont
			    (setq placeholder (c-guess-basic-syntax))))
		(setq syntax placeholder)
	      (c-beginning-of-statement-1 lim)
	      (c-forward-token-1 0)
	      (if (looking-at "typedef\\>") (c-forward-token-1 1))
	      (c-add-syntax 'brace-list-open (c-point 'boi))))
	   ;; CASE 9B: brace-list-close brace
	   ((if (consp special-brace-list)
		;; Check special brace list closer.
		(progn
		  (goto-char (car (car special-brace-list)))
		  (save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (or
		     ;; We were between the special close char and the `)'.
		     (and (eq (char-after) ?\))
			  (eq (1+ (point)) (cdr (car special-brace-list))))
		     ;; We were before the special close char.
		     (and (eq (char-after) (cdr (cdr special-brace-list)))
			  (= (c-forward-token-1) 0)
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (progn (forward-char 1)
				  (c-backward-sexp 1)
				  t))
		   (= (point) containing-sexp)))
	    (c-add-syntax 'brace-list-close (c-point 'boi)))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-1 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (c-skip-ws-forward indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (goto-char containing-sexp)
	      (c-add-syntax 'brace-list-intro (c-point 'boi))
	      )				; end CASE 9C
	     ;; CASE 9D: this is just a later brace-list-entry or
	     ;; brace-entry-open
	     (t (if (or (eq char-after-ip ?{)
			(and c-special-brace-lists
			     (save-excursion
			       (goto-char indent-point)
			       (c-forward-syntactic-ws (c-point 'eol))
			       (c-looking-at-special-brace-list (point)))))
		    (c-add-syntax 'brace-entry-open (point))
		  (c-add-syntax 'brace-list-entry (point))
		  ))			; end CASE 9D
	     ))))			; end CASE 9
	 ;; CASE 10: A continued statement or top level construct.
	 ((and (not (memq char-before-ip '(?\; ?:)))
	       (or (not (eq char-before-ip ?}))
		   (c-looking-at-inexpr-block-backward containing-sexp))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    ;(c-forward-syntactic-ws)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  (let ((after-cond-placeholder
		 (save-excursion
		   (goto-char placeholder)
		   (if (and c-conditional-key (looking-at c-conditional-key))
		       (progn
			 (c-safe (c-skip-conditional))
			 (c-forward-syntactic-ws)
			 (if (eq (char-after) ?\;)
			     (progn
			       (forward-char 1)
			       (c-forward-syntactic-ws)))
			 (point))
		     nil))))
	    (when after-cond-placeholder
	      (error "after-cond-placeholder not nil")))
	  ;; This is shared with CASE 18.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       nil
				       containing-sexp
				       lim))
	 ;; CASE 14: A case or default label
	 ((looking-at c-switch-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (c-forward-sexp -1))
	  (c-add-syntax 'case-label (c-point 'boi)))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (c-forward-sexp -1))
	  (c-add-syntax 'label (c-point 'boi)))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  (let* ((lim (c-safe-position containing-sexp fullstate))
		 (relpos (save-excursion
			   (goto-char containing-sexp)
			   (if (/= (point) (c-point 'boi))
			       (c-beginning-of-statement-1 lim))
			   (c-point 'boi))))
	    (cond
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?
	     ((save-excursion
		(goto-char containing-sexp)
		(setq placeholder (c-looking-at-inexpr-block)))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-syntax tmpsymbol (point))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; an extern block or namespace?
	     ((progn
		(goto-char containing-sexp)
		(setq placeholder (c-search-uplist-for-classkey state)))
	      (goto-char (aref placeholder 0))
	      (if (looking-at (concat c-extra-toplevel-key "[^_]"))
		  (c-add-syntax 'defun-close relpos)
		(c-add-syntax 'inline-close relpos)))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close
	     ((and (not inenclosing-p)
		   (c-most-enclosing-brace state)
		   (or (not (c-major-mode-is 'pike-mode))
		       ;; In Pike it can be a defun-close of a
		       ;; function declared in a statement block.  Let
		       ;; it through to be handled below.
		       (or (c-looking-at-bos)
			   (progn
			     (c-beginning-of-statement-1)
			     (looking-at c-conditional-key)))))
	      (c-add-syntax 'block-close relpos))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl)
		    (c-add-syntax 'defun-close relpos)))))
	     )))
	 ;; CASE 17: Statement or defun catchall.
	 (t
	  ;; First statement in a block?
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (c-forward-syntactic-ws indent-point)
	  ;; now skip forward past any case/default clauses we might find.
	  (while (or (c-skip-case-statement-forward fullstate indent-point)
		     (and (looking-at c-switch-label-key)
			  (not inswitch-p)))
	    (setq inswitch-p t))
	  ;; we want to ignore non-case labels when skipping forward
	  (while (and (looking-at c-label-key)
		      (goto-char (match-end 0)))
	    (c-forward-syntactic-ws indent-point))
	  (cond
	   ;; CASE 17A: we are inside a case/default clause inside a
	   ;; switch statement.  find out if we are at the statement
	   ;; just after the case/default label.
	   ((and inswitch-p
		 (progn
		   (goto-char indent-point)
		   (c-beginning-of-statement-1 containing-sexp)
		   (setq placeholder (point))
		   (beginning-of-line)
		   (when (re-search-forward c-switch-label-key
					    (max placeholder (c-point 'eol)) t)
		     (setq placeholder (match-beginning 0)))))
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (if (eq (char-after) ?{)
		(c-add-syntax 'statement-case-open placeholder)
	      (c-add-syntax 'statement-case-intro placeholder)))
	   ;; CASE 17B: continued statement
	   ((eq char-before-ip ?,)
	    (goto-char indent-point)
	    (c-beginning-of-closest-statement)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 17C: a question/colon construct?  But make sure
	   ;; what came before was not a label, and what comes after
	   ;; is not a globally scoped function call!
	   ((or (and (memq char-before-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (c-backward-syntactic-ws lim)
		       (back-to-indentation)
		       (not (looking-at c-label-key))))
		(and (memq char-after-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (skip-chars-forward " \t")
		       ;; watch out for scope operator
		       (not (looking-at "::")))))
	    (goto-char indent-point)
	    (c-beginning-of-closest-statement)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 17D: any old statement
	   ((< (point) indent-point)
	    (let ((safepos (c-most-enclosing-brace fullstate indent-point))
		  relpos done)
	      (goto-char indent-point)
	      (c-beginning-of-statement-1 safepos)
	      ;; It is possible we're on the brace that opens a nested
	      ;; function.
	      (if (and (eq (char-after) ?{)
		       (save-excursion
			 (c-backward-syntactic-ws safepos)
			 (not (eq (char-before) ?\;))))
		  (c-beginning-of-statement-1 safepos))
	      (if (and inswitch-p
		       (looking-at c-switch-label-key))
		  (progn
		    (goto-char (match-end 0))
		    (c-forward-syntactic-ws)))
	      (setq relpos (c-point 'boi))
	      (while (and (not done)
			  (<= safepos (point))
			  (/= relpos (point)))
		(c-beginning-of-statement-1 safepos)
		(if (= relpos (c-point 'boi))
		    (setq done t))
		(setq relpos (c-point 'boi)))
	      (c-add-syntax 'statement relpos)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'block-open))))
	   ;; CASE 17E: first statement in an in-expression block
	   ((progn
	      ;; The following tests are all based on containing-sexp.
	      (goto-char containing-sexp)
	      (setq placeholder (c-looking-at-inexpr-block)))
	    (back-to-indentation)
	    (let ((block-intro (if (eq (car placeholder) 'inlambda)
				   'defun-block-intro
				 'statement-block-intro)))
	      (if (= containing-sexp (point))
		  (c-add-syntax block-intro (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-syntax block-intro (point))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here).
	   ;; However, we first check for statements that we can
	   ;; recognize by keywords.  That increases the robustness in
	   ;; cases where statements are used on the top level,
	   ;; e.g. in macro definitions.
	   ((and (not (save-excursion
			(and (= (c-backward-token-1 1 t) 0)
			     (or (looking-at c-block-stmt-1-kwds)
				 (and (eq (char-after) ?\()
				      (= (c-backward-token-1 1 nil) 0)
				      (looking-at c-block-stmt-2-kwds))))))
		 (save-excursion
		   (save-restriction
		     (widen)
		     (c-narrow-out-enclosing-class state containing-sexp)
		     (not (c-most-enclosing-brace state)))))
	    ;; if not at boi, then defun-opening braces are hung on
	    ;; right side, so we need a different relpos
	    (if (/= (point) (c-point 'boi))
		(progn
		  (c-backward-syntactic-ws)
		  (c-safe (c-forward-sexp (if (eq (char-before) ?\))
					      -1 -2)))
		  ;; looking at a Java throws clause following a
		  ;; method's parameter list
		  (c-beginning-of-statement-1)
		  ))
	    (c-add-syntax 'defun-block-intro (c-point 'boi)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can only occur in Pike.
	   ((and (c-major-mode-is 'pike-mode)
		 (save-excursion
		   (and (not (c-looking-at-bos))
			(progn
			  (c-beginning-of-statement-1)
			  (not (looking-at c-conditional-key)))
			(setq placeholder (point)))))
	    (c-add-syntax 'defun-block-intro (c-point 'boi placeholder)))
	   ;; CASE 17H: first statement in a block
	   (t
	    (if (/= (point) (c-point 'boi))
		(c-beginning-of-statement-1
		 (if (= (point) lim)
		     (c-safe-position (point) state) lim)))
	    (c-add-syntax 'statement-block-intro (c-point 'boi))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(when (and (looking-at c-comment-start-regexp)
		   (/= (c-forward-token-1 0 nil (c-point 'eol)) 0))
	  (c-add-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(when (and (c-major-mode-is 'c++-mode)
		   (looking-at c-C++-friend-key))
	  (c-add-syntax 'friend))
	;; Start of or a continuation of a preprocessor directive?
	(if (and macro-start
		 (eq macro-start (c-point 'boi))
		 (not (and (c-major-mode-is 'pike-mode)
			   (eq (char-after (1+ macro-start)) ?\"))))
	    (c-add-syntax 'cpp-macro)
	  (when (and c-syntactic-indentation-in-macros
		     macro-start
		     (eq macro-start syntactic-relpos))
	    (c-add-syntax 'cpp-macro-cont)))
	;; return the syntax
	syntax))))


(defun c-echo-parsing-error (&optional quiet)
  (when (and c-parsing-error (not quiet))
    (message "%s" c-parsing-error)
    (ding))
  c-parsing-error)

(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  (cond
   ((eq offset '+)         c-basic-offset)
   ((eq offset '-)         (- c-basic-offset))
   ((eq offset '++)        (* 2 c-basic-offset))
   ((eq offset '--)        (* 2 (- c-basic-offset)))
   ((eq offset '*)         (/ c-basic-offset 2))
   ((eq offset '/)         (/ (- c-basic-offset) 2))
   ((numberp offset)       offset)
   ((functionp offset)     (c-evaluate-offset
			    (funcall offset langelem) langelem symbol))
   ((vectorp offset)       offset)
   ((null offset)          nil)
   ((listp offset)
    (let (done)
      (while (and (not done) offset)
	(setq done (c-evaluate-offset (car offset) langelem symbol)
	      offset (cdr offset)))
      (if (and c-strict-syntax-p (not done))
	  (error "No offset found for syntactic symbol %s" symbol))
      done))
   (t (symbol-value offset))
   ))

(defun c-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; c-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(if c-strict-syntax-p
	    (error "No offset found for syntactic symbol %s" symbol)
	  (setq offset 0
		relpos 0))
      (setq offset (c-evaluate-offset offset langelem symbol)))
    (if (vectorp offset)
	offset
      (+ (if (and relpos
		  (< relpos (c-point 'bol)))
	     (save-excursion
	       (goto-char relpos)
	       (current-column))
	   0)
	 (or (and (numberp offset) offset)
	     (and (symbolp offset) (symbol-value offset))
	     0)))
    ))

(defun c-get-syntactic-indentation (langelems)
  ;; Apply c-get-offset to a list of langelem cells to get the total
  ;; syntactic indentation.  Special treatment is needed for vectors
  ;; containing absolute columns.
  (let ((indent 0))
    (catch 'done
      (while langelems
	(let ((res (c-get-offset (car langelems))))
	  (if (vectorp res)
	      (throw 'done (elt res 0))
	    (setq indent (+ indent res)
		  langelems (cdr langelems)))))
      indent)))


(cc-provide 'cc-engine)

;;; cc-engine.el ends here
