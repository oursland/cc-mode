;;; cc-guess.el --- guess indentation values by scanning existing code

;; Copyright (C) 1994-1995 Free Software Foundation, Inc.

;; Author: 1994 Barry A. Warsaw <bwarsaw@cnri.reston.va.us>
;; Maintainer:    cc-mode-help@anthem.nlm.nih.gov
;; Created:       August 1994
;; Version:       $Revision: 1.2 $
;; Last Modified: $Date: 1995-02-28 22:53:42 $
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
;; This file contains routines that help guess the cc-mode style in a
;; particular region of C, C++, or Objective-C code.  This file is
;; completely unsupported.  Guessing styles is lossy.  Feel free to
;; improve upon this stuff if you want.

;; LCD Archive Entry:
;; cc-guess|Barry A. Warsaw|bwarsaw@cen.com
;; |guess cc-mode indentation variable values
;; |$Date: 1995-02-28 22:53:42 $|$Revision: 1.2 $|

;;; Code:

(defvar cc-guessed-style nil
  "Currently guessed style.")

(defvar cc-guess-conversions
  '((c . c-lineup-C-comments)
    (inher-cont . c-lineup-multi-inher)
    (string . -1000)
    (comment-intro . c-lineup-comment)
    (arglist-cont-nonempty . c-lineup-arglist)
    (cpp-macro . -1000))
  

(defun cc-guess-region (start end &optional reset)
  "Sets `c-offset-alist' indentation values based on region of code.
Every line of code in the region is examined and the indentation
values of the various syntactic symbols in `c-offset-alist' is
guessed.  The first such positively identified indentation is used, so
if an inconsistent style exists in the C code, the guessed indentation
may be incorrect.

Note that the larger the region to guess in, the slower the
guessing. Previous guesses can be concatenated together, unless the
optional RESET is provided.

See `cc-guess-write-style' to find out how to save the guessed style,
and `cc-guess-view-style' for viewing the guessed style."
  (interactive "r\nP")
  (if (consp reset)
      (setq cc-guessed-style nil))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let* ((syntax (c-guess-basic-syntax))
	     (relpos (cdr (car syntax)))
	     (symbol (car (car syntax)))
	     point-indent relpos-indent)
	;; TBD: for now I can't guess indentation when more than 1
	;; symbol is on the list, nor for symbols without relpos's
	(if (or (/= 1 (length syntax))
		(not (numberp relpos))
		;; also, don't try to reguess an already guessed
		;; symbol
		(assq symbol cc-guessed-style))
	    nil
	  (back-to-indentation)
	  (setq point-indent (current-column))
	  (goto-char relpos)
	  (setq relpos-indent (current-column))
	  ;; guessed indentation is the difference between point's and
	  ;; relpos's current-column indentation
	  (setq cc-guessed-style
		(cons (cons symbol (- point-indent relpos-indent))
		      cc-guessed-style))
	  ))
      (forward-line 1))))
