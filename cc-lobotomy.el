;;; cc-lobotomy.el --- excise portions of cc-mode in the name of speed

;; Copyright (C) Barry A. Warsaw

;; Author: 1995 Barry A. Warsaw <bwarsaw@cnri.reston.va.us>
;; Maintainer:    cc-mode-help@anthem.nlm.nih.gov
;; Created:       March 1995
;; Version:       $Revision: 1.2 $
;; Last Modified: $Date: 1995-03-29 23:11:57 $
;; Keywords: C++ C Objective-C cc-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Even though every effort is made to make cc-mode the fastest it can
;; be, by the nature of the C, C++, and Objective-C language
;; definitions, some performance enhancements simply require less than
;; accurate recognition of language constructs.  I believe it is
;; always best to be correct, and that the mode is currently fast
;; enough for most normal usage.  Others disagree.  I have no
;; intention of including these hacks in the main distribution.  When
;; cc-mode version 5 comes out, it will include a rewritten
;; indentation engine so it should be much faster automatically.
;;
;; Note that if you use the hacks in this file, don't complain to me
;; about incorrect indentation.  That's the price you pay for speed in
;; some circumstances so you'll have to live with it.

;; LCD Archive Entry:
;; cc-lobotomy|Barry A. Warsaw|cc-mode-help@anthem.nlm.nih.gov
;; |excise portions of cc-mode in the name of speed
;; |$Date: 1995-03-29 23:11:57 $|$Revision: 1.2 $|


;;; Code:

(require 'cc-mode)

(defun c-narrow-out-enclosing-class (dummy1 dummy2) nil)

(defun c-search-uplist-for-classkey (dummy) nil)

(defun c-inside-bracelist-p (dummy1 dummy2) nil)

(defun c-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (c-point 'bod)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      ;; this will not catch multiline macros
	      (if (= (char-after (c-point 'boi)) ?#)
		  (beginning-of-line))
	      )))
      )))

(defun c-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  (let ((firstp t)
	(substmt-p t)
	donep c-in-literal-cache
	;; KLUDGE ALERT: maybe-labelp is used to pass information
	;; between c-crosses-statement-barrier-p and
	;; c-beginning-of-statement-1.  A better way should be
	;; implemented.
	maybe-labelp
	(last-begin (point)))
    (while (not donep)
      ;; stop at beginning of buffer
      (if (bobp) (setq donep t)
	;; go backwards one balanced expression, but be careful of
	;; unbalanced paren being reached
	(if (not (c-safe (progn (backward-sexp 1) t)))
	    (progn
	      (if firstp
		  (backward-up-list 1)
		(goto-char last-begin))
	      ;; skip over any unary operators, or other special
	      ;; characters appearing at front of identifier
	      (save-excursion
		(c-backward-syntactic-ws lim)
		(skip-chars-backward "-+!*&:.~ \t\n")
		(if (= (preceding-char) ?\()
		    (setq last-begin (point))))
	      (goto-char last-begin)
	      (setq last-begin (point)
		    donep t)))

	(setq maybe-labelp nil)
	;; see if we're in a literal. if not, then this bufpos may be
	;; a candidate for stopping
	(cond
	 ;; CASE 0: did we hit the error condition above?
	 (donep)
	 ;; CASE 1: are we in a literal?
	 ((= (char-after (c-point 'boi)) ?#)
	  (beginning-of-line))
	 ;; CASE 2: some other kind of literal?
	 ((c-in-literal lim))
	 ;; CASE 3: are we looking at a conditional keyword?
	 ((or (looking-at c-conditional-key)
	      (and (= (following-char) ?\()
		   (save-excursion
		     (forward-sexp 1)
		     (c-forward-syntactic-ws)
		     (/= (following-char) ?\;))
		   (let ((here (point))
			 (foundp (progn
				   (c-backward-syntactic-ws lim)
				   (forward-word -1)
				   (and lim
					(<= lim (point))
					(not (c-in-literal lim))
					(looking-at c-conditional-key)))))
		     ;; did we find a conditional?
		     (if (not foundp)
			 (goto-char here))
		     foundp)))
	  ;; are we in the middle of an else-if clause?
	  (if (save-excursion
		(and (not substmt-p)
		     (c-safe (progn (forward-sexp -1) t))
		     (looking-at "\\<else\\>[ \t\n]+\\<if\\>")
		     (not (c-in-literal lim))))
	      (progn
		(forward-sexp -1)
		(c-backward-to-start-of-if lim)))
	  ;; are we sitting at an else clause, that we are not a
	  ;; substatement of?
	  (if (and (not substmt-p)
		   (looking-at "\\<else\\>[^_]"))
	      (c-backward-to-start-of-if lim))
	  ;; are we sitting at the while of a do-while?
	  (if (and (looking-at "\\<while\\>[^_]")
		   (c-backward-to-start-of-do lim))
	      (setq substmt-p nil))
	  (setq last-begin (point)
		donep substmt-p))
	 ;; CASE 4: is this the first time we're checking?
	 (firstp (setq firstp nil
		       substmt-p (not (c-crosses-statement-barrier-p
				       (point) last-begin))
		       last-begin (point)))
	 ;; CASE 5: have we crossed a statement barrier?
	 ((c-crosses-statement-barrier-p (point) last-begin)
	  (setq donep t))
	 ;; CASE 6: ignore labels
	 ((and maybe-labelp
	       (or (and c-access-key (looking-at c-access-key))
		   ;; with switch labels, we have to go back further
		   ;; to try to pick up the case or default
		   ;; keyword. Potential bogosity alert: we assume
		   ;; `case' or `default' is first thing on line
		   (let ((here (point)))
		     (beginning-of-line)
		     (c-forward-syntactic-ws)
		     (if (looking-at c-switch-label-key)
			 t
		       (goto-char here)
		       nil))
		   (looking-at c-label-key))))
	 ;; CASE 7: ObjC method def
	 ((and (eq major-mode 'objc-mode)
	       (setq last-begin (c-in-objc-method-def-p)))
	  (setq donep t))
	 ;; CASE 8: nothing special
	 (t (setq last-begin (point)))
	 )))
    (goto-char last-begin)
    ;; we always do want to skip over non-whitespace modifier
    ;; characters that didn't get skipped above
    (skip-chars-backward "-+!*&:.~" (c-point 'boi))))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on cc-mode."
  (interactive)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist)))
    (and
     (y-or-n-p "Do you want to submit a report on cc-mode? ")
     (require 'reporter)
     (reporter-submit-bug-report
      c-mode-help-address
      (concat "cc-mode " c-version " ("
	      (cond ((eq major-mode 'c++-mode)  "C++")
		    ((eq major-mode 'c-mode)    "C")
		    ((eq major-mode 'objc-mode) "ObjC"))
	      ")")
      (let ((vars (list
		   ;; report only the vars that affect indentation
		   'c-basic-offset
		   'c-offsets-alist
		   'c-block-comments-indent-p
		   'c-cleanup-list
		   'c-comment-only-line-offset
		   'c-backslash-column
		   'c-delete-function
		   'c-electric-pound-behavior
		   'c-hanging-braces-alist
		   'c-hanging-colons-alist
		   'c-hanging-comment-ender-p
		   'c-tab-always-indent
		   'c-recognize-knr-p
		   'defun-prompt-regexp
		   'tab-width
		   )))
	(if (not (boundp 'defun-prompt-regexp))
	    (delq 'defun-prompt-regexp vars)
	  vars))
      (function
       (lambda ()
	 (insert
	  (if c-special-indent-hook
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "c-special-indent-hook is set to '"
		      (format "%s" c-special-indent-hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n")
	  (format "c-emacs-features: %s\n" c-emacs-features)
	  )))
      (function
       (lambda ()
	 (insert
	  "You are using cc-lobotomy.el.  You realize that by doing\n"
	  "so you have already made the decision to trade off accuracy\n"
	  "for speed?  Don't set your hopes too high that your problem\n"
	  "will be fixed.\n\n"
	  )))
      "Dear Barry,"
      ))))

(provide 'cc-lobotomy)
;;; cc-lobotomy.el ends here
