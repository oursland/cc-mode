;;; cc-lobotomy.el --- excise portions of cc-mode in the name of speed

;; Copyright (C) Barry A. Warsaw

;; Author: 1995 Barry A. Warsaw <bwarsaw@cnri.reston.va.us>
;; Maintainer:    cc-mode-help@anthem.nlm.nih.gov
;; Created:       March 1995
;; Version:       $Revision: 1.5 $
;; Last Modified: $Date: 1995-05-26 22:09:06 $
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
;; Every effort has been made to improve the performance of
;; cc-mode. However, due to the nature of the C, C++, and Objective-C
;; language definitions, a trade-off is often required between
;; accuracy of construct recognition and speed. I believe it is always
;; best to be correct, and that the mode is currently fast enough for
;; most normal usage.  Others disagree.  I have no intention of
;; including these hacks in the main distribution.  When cc-mode
;; version 5 comes out, it will include a rewritten indentation engine
;; so that performance will be greatly improved automatically, at the
;; expense of breaking Emacs 18 support.
;;
;; You can expect possibly incorrect indentation within class and
;; struct declarations and within brace lists.  There may be other
;; places where indentation breaks, so if you use the hacks in this
;; file, don't complain to me about incorrect indentation.  That's the
;; price you pay for speed in some circumstances so you'll have to
;; live with it!  Most incorrect indentation can probably be corrected
;; by hand though.
;;
;; To use this file, just `require' it by adding the following to your
;; .emacs file:
;;
;;   (require 'cc-lobotomy)
;;
;; This will redefine certain cc-mode functions.

;; LCD Archive Entry:
;; cc-lobotomy|Barry A. Warsaw|cc-mode-help@anthem.nlm.nih.gov
;; |excise portions of cc-mode in the name of speed
;; |$Date: 1995-05-26 22:09:06 $|$Revision: 1.5 $|


;;; Code:

(require 'cc-mode)

;; This is a faster version of c-in-literal.  It trades speed for one
;; approximation, namely that within other literals, the `#' character
;; cannot be the first non-whitespace on a line.
(defun c-in-literal (&optional lim)
  ;; first check the cache
  (if (and (boundp 'c-in-literal-cache)
	   c-in-literal-cache
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    ;; quickly check for cpp macro. this breaks if the `#' character
    ;; appears as the first non-whitespace on a line inside another
    ;; literal.
    (let* (state
	   (char-at-boi (char-after (c-point 'boi)))
	   (rtn (cond
		 ((and char-at-boi (= char-at-boi ?#))
		  'pound)
		 ((nth 3 (setq state (save-excursion
				       (parse-partial-sexp
					(or lim (c-point 'bod))
					(point)))))
		  'string)
		 ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		 (t nil))))
      ;; cache this result if the cache is enabled
      (and (boundp 'c-in-literal-cache)
	   (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))

(defun c-narrow-out-enclosing-class (dummy1 dummy2) nil)

(defun c-search-uplist-for-classkey (dummy) nil)

(defun c-inside-bracelist-p (dummy1 dummy2) nil)

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
