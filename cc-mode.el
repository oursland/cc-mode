;;; cc-mode.el --- major mode for editing C, C++, Objective-C, and Java code

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:  1992-1997 Barry A. Warsaw
;;           1987 Dave Detlefs and Stewart Clamen
;;           1985 Richard M. Stallman
;; Created:  a long, long, time ago. adapted from the original c-mode.el
;; Version:  5.00
;; Keywords: c languages oop

;; NOTE: Read the commentary below for the right way to submit bug reports!
;; NOTE: See the accompanying texinfo manual for details on using this mode!

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

;;; Commentary:

;; This package provides GNU Emacs major modes for editing C, C++,
;; Objective-C, and Java code.  As of the latest Emacs and XEmacs
;; releases, it is the default package for editing these languages.
;; This package is called "CC Mode", and should be spelled exactly
;; this way.  It supports K&R and ANSI C, ANSI C++, Objective-C, and
;; Java, with a consistent indentation model across all modes.  This
;; indentation model is intuitive and very flexible, so that almost
;; any desired style of indentation can be supported.  Installation,
;; usage, and programming details are contained in an accompanying
;; texinfo manual.

;; CC Mode's immediate ancestors were, c++-mode.el, cplus-md.el, and
;; cplus-md1.el..

;; NOTE: This mode does not perform font-locking (a.k.a syntactic
;; coloring, keyword highlighting, etc.) for any of the supported
;; modes.  Typically this is done by a package called font-lock.el
;; which I do *not* maintain.  You should contact the Emacs
;; maintainers for questions about coloring or highlighting in any
;; language mode.

;; To submit bug reports, type "C-c C-b".  These will be sent to
;; bug-gnu-emacs@prep.ai.mit.edu as well as cc-mode-help@python.org,
;; and I'll read about them there (the former is mirrored as the
;; Usenet newsgroup gnu.emacs.bug).  Questions can sent to
;; help-gnu-emacs@prep.ai.mit.edu (mirrored as gnu.emacs.help) and/or
;; cc-mode-help@python.org.  Please do not send bugs or questions to
;; my personal account.

;; YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS. They are the result of
;; the cross-Emacsen support.  GNU Emacs 19 (from the FSF), GNU XEmacs
;; 19 (formerly Lucid Emacs), and GNU Emacs 18 all do things
;; differently and there's no way to shut the byte-compiler up at the
;; necessary granularity.  Let me say this again: YOU CAN IGNORE ALL
;; BYTE-COMPILER WARNINGS (you'd be surprised at how many people don't
;; follow this advice :-).

;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, code contributions, and
;; encouragement CC Mode would be a far inferior package.

;; You can get the latest version of CC Mode, including PostScript
;; documentation and separate individual files from:
;;
;;     http://www.python.org/ftp/emacs/

;; Or if you don't have access to the World Wide Web, through
;; anonymous ftp from:
;;
;;    ftp://ftp.python.org/pub/emacs

;;; Code:



;; Figure out what features this Emacs has
(defconst c-emacs-features
  (let ((infodock-p (boundp 'infodock-version))
	(comments
	 ;; XEmacs 19 and beyond use 8-bit modify-syntax-entry flags.
	 ;; Emacs 19 uses a 1-bit flag.  We will have to set up our
	 ;; syntax tables differently to handle this.
	 (let ((table (copy-syntax-table))
	       entry)
	   (modify-syntax-entry ?a ". 12345678" table)
	   (cond
	    ;; XEmacs 19
	    ((vectorp table) (setq entry (aref table ?a)))
	    ;; XEmacs 20
	    ((fboundp 'get-char-table) (setq entry (get-char-table ?a table)))
	    ;; Emacs 19
	    ((and (fboundp 'char-table-p)
		  (char-table-p table))
	     (setq entry (car (char-table-range table [?a]))))
	    ;; incompatible
	    (t (error "CC Mode is incompatible with this version of Emacs")))
	   (if (= (logand (lsh entry -16) 255) 255)
	       '8-bit
	     '1-bit))))
    (if infodock-p
	(list comments 'infodock)
      (list comments)))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by CC Mode.  Here's the current
supported list, along with the values for this variable:

 XEmacs 19:                  (8-bit)
 XEmacs 20:                  (8-bit)
 Emacs 19:                   (1-bit)

Infodock (based on XEmacs) has an additional symbol on this list:
'infodock.")



(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  This is not needed
  ;; for Emacs.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))



;; defuns for submitting bug reports
(defconst c-version "5.00"
  "CC Mode version number.")

(defconst c-mode-help-address
  "bug-gnu-emacs@prep.ai.mit.edu, cc-mode-help@python.org"
  "Address for CC Mode bug reports.")

(defun c-version ()
  "Echo the current version of CC Mode in the minibuffer."
  (interactive)
  (message "Using CC Mode version %s" c-version)
  (c-keep-region-active))

;; Get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'cc-vars)
  (require 'reporter))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
  (require 'cc-vars)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist))
	(style c-indentation-style)
	(hook c-special-indent-hook)
	(c-features c-emacs-features))
    (and
     (if (y-or-n-p "Do you want to submit a report on CC Mode? ")
	 t (message "") nil)
     (require 'reporter)
     (reporter-submit-bug-report
      c-mode-help-address
      (concat "CC Mode " c-version " ("
	      (cond ((eq major-mode 'c++-mode)  "C++")
		    ((eq major-mode 'c-mode)    "C")
		    ((eq major-mode 'objc-mode) "ObjC")
		    ((eq major-mode 'java-mode) "Java")
		    )
	      ")")
      (let ((vars (list
		   ;; report only the vars that affect indentation
		   'c-basic-offset
		   'c-offsets-alist
		   'c-cleanup-list
		   'c-comment-only-line-offset
		   'c-backslash-column
		   'c-delete-function
		   'c-electric-pound-behavior
		   'c-hanging-braces-alist
		   'c-hanging-colons-alist
		   'c-hanging-comment-starter-p
		   'c-hanging-comment-ender-p
		   'c-indent-comments-syntactically-p
		   'c-tab-always-indent
		   'c-recognize-knr-p
		   'c-label-minimum-indentation
		   'defun-prompt-regexp
		   'tab-width
		   )))
	(if (not (boundp 'defun-prompt-regexp))
	    (delq 'defun-prompt-regexp vars)
	  vars))
      (function
       (lambda ()
	 (insert
	  "Buffer Style: " style "\n\n"
	  (if hook
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "c-special-indent-hook is set to '"
		      (format "%s" hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n")
	  (format "c-emacs-features: %s\n" c-features)
	  )))
      nil
      "Dear Barry,"
      ))))


(provide 'cc-mode)
;;; cc-mode.el ends here
