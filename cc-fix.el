;;; cc-fix.el --- compatibility library for old (X)Emacs versions

;; Copyright (C) 1985,1987,1992-2003 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1997 Barry A. Warsaw
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    03-Jul-1997 (as cc-mode-19.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is necessary in order to run CC Mode in older (X)Emacs
;; versions.  It's not needed at all for the latest versions of Emacs
;; and XEmacs.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

;; Silence the compiler (in case this file is compiled by other
;; Emacsen even though it isn't used by them).
(cc-bytecomp-obsolete-fun byte-code-function-p)
(cc-bytecomp-defun regexp-opt-depth)

(cc-external-require 'advice)


;; Emacs 19.34 requires the POS argument to char-after.  Emacs 20
;; makes it optional, as it has long been in XEmacs.
(eval-and-compile
  (condition-case nil
      (eval '(char-after))		; `eval' avoids argcount warnings
    (error
     (ad-define-subr-args 'char-after '(pos))
     (defadvice char-after (before c-char-after-advice
				   (&optional pos)
				   activate preactivate)
       "POS is optional and defaults to the position of point."
       (if (not pos)
	   (setq pos (point))))
     (if (and (featurep 'cc-bytecomp)
	      (cc-bytecomp-is-compiling))
	 (progn
	   ;; Since char-after is handled specially by the byte
	   ;; compiler, we need some black magic to make the compiler
	   ;; warnings go away.
	   (defun byte-compile-char-after (form)
	     (if (= (length form) 1)
		 (byte-compile-one-arg (append form '((point))))
	       (byte-compile-one-arg form)))
	   (byte-defop-compiler char-after))))))

(if (fboundp 'char-before)
    (condition-case nil
        (eval '(char-before))           ; `eval' avoids argcount warnings
      (error                            ; too few parameters
       ;; Will (probably) only trigger for Emacs 19.xx, which doesn't mind
       ;; being given a superfluous parameter, and "MULE Emacs 19" (a forked
       ;; version of Emacs) which must handle the extra paramter.  XEmacs will
       ;; never trigger this.

       ;; MULE based on Emacs 19.34 has a char-before function, but
       ;; it requires a position.  It also has a second optional
       ;; argument that we must pass on.
       (ad-define-subr-args 'char-before '(pos &optional byte-unit))
       (defadvice char-before (before c-char-before-advice
                                      (&optional pos byte-unit)
                                      activate preactivate)
         "POS is optional and defaults to the position of point."
         (if (not pos)
             (setq pos (point)))))))
  
;; The `eval' construct is necessary since later versions complain at
;; compile time on the defsubst for `char-before' since it has become
;; a built-in primitive.
(eval
 '(or (fboundp 'char-before)
      ;; Emacs 19.34 doesn't have a char-before function.
      (defsubst char-before (&optional pos)
	(char-after (1- (or pos (point)))))))

;; Emacs 19.34 doesn't have a functionp function.  Here's its Emacs
;; 20 definition.
(or (fboundp 'functionp)
    (defun functionp (object)
      "Non-nil if OBJECT is a type of object that can be called as a function."
      (or (subrp object) (byte-code-function-p object)
	  (eq (car-safe object) 'lambda)
	  (and (symbolp object) (fboundp object)))))

;; Emacs 19.34 doesn't have a when macro.  Here's its Emacs 20
;; definition.
(or (fboundp 'when)
    (defmacro when (cond &rest body)
      "(when COND BODY...): if COND yields non-nil, "
      "do BODY, else return nil."
      (list 'if cond (cons 'progn body))))

;; Emacs 19.34 doesn't have an unless macro.  Here's its Emacs 20
;; definition.
(or (fboundp 'unless)
    (defmacro unless (cond &rest body)
      "(unless COND BODY...): if COND yields nil, "
      "do BODY, else return nil."
      (cons 'if (cons cond (cons nil body)))))

(if (fboundp 'regexp-opt)
    (defalias 'c-regexp-opt 'regexp-opt)
  ;; (X)Emacs 19 doesn't have the regexp-opt package.
  (defun c-regexp-opt (strings &optional paren)
    ;; The regexp engine (in at least (X)Emacs 19) matches the
    ;; alternatives in order and fails to be greedy if a longer
    ;; alternative comes after a shorter one, so we sort the the
    ;; list with the longest alternatives first to get greediness
    ;; properly.
    (setq strings (sort (append strings nil)
			(lambda (a b) (> (length a) (length b)))))
    (if paren
	(concat "\\(" (mapconcat 'regexp-quote strings "\\|") "\\)")
      (mapconcat 'regexp-quote strings "\\|"))))

(if (and (fboundp 'regexp-opt-depth)
         (eq (regexp-opt-depth "\\(\\(\\)\\)") 2))
    (defalias 'c-regexp-opt-depth 'regexp-opt-depth)
  ;; (X)Emacs 19 doesn't have the regexp-opt package.
  ;; Emacs 21.1 has a buggy regexp-opt-depth which prevents CC Mode building.
  ;; Those in Emacs 21.[23] are not entirely accurate.
  ;; The following definition comes from Emacs's regexp-opt.el CVS version 1.25
  ;; and is believed to be a rigorously correct implementation.
  (defconst regexp-opt-not-groupie*-re
    (let* ((harmless-ch "[^\\\\[]")
           (esc-pair-not-lp "\\\\[^(]")
           (class-harmless-ch "[^][]")
           (class-lb-harmless "[^]:]")
           (class-lb-colon-maybe-charclass ":\\([a-z]+:]\\)?")
           (class-lb (concat "\\[\\(" class-lb-harmless
                             "\\|" class-lb-colon-maybe-charclass "\\)"))
           (class
            (concat "\\[^?]?"
                    "\\(" class-harmless-ch
                    "\\|" class-lb "\\)*"
                    "\\[?]"))       ; special handling for bare [ at end of re
           (shy-lp "\\\\(\\?:"))
      (concat "\\(" harmless-ch "\\|" esc-pair-not-lp
              "\\|" class "\\|" shy-lp "\\)*"))
    "Matches any part of a regular expression EXCEPT for non-shy \"\\\\(\"s")

  (defun c-regexp-opt-depth (regexp)
    "Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP."
    (save-match-data
      ;; Hack to signal an error if REGEXP does not have balanced parentheses.
      (string-match regexp "")
      ;; Count the number of open parentheses in REGEXP.
      (let ((count 0) start)
        (while
            (progn
              (string-match regexp-opt-not-groupie*-re regexp start)
              (setq start ( + (match-end 0) 2)) ; +2 for "\\(" after match-end.
              (<= start (length regexp)))
          (setq count (1+ count)))
        count)))
  )

;; Some XEmacs versions have a bug in which font-lock-compile-keywords
;; overwrites the variable font-lock-keywords with its result.  This causes
;; havoc when what the function is compiling is font-lock-SYNTACTIC-keywords,
;; hence....
(eval-after-load "font-lock"
  '(when (let (font-lock-keywords)
           (font-lock-compile-keywords '("\\<\\>"))
           font-lock-keywords)        ; did the previous call foul this up?
     (defun font-lock-compile-keywords (keywords)
       "Compile KEYWORDS (a list) and return the list of compiled keywords.
Each keyword has the form (MATCHER HIGHLIGHT ...).  See `font-lock-keywords'."
       (if (eq (car-safe keywords) t)
           keywords
         (cons t (mapcar 'font-lock-compile-keyword keywords))))
     (defadvice font-lock-fontify-keywords-region (before c-compile-font-lock-keywords
                                                          activate preactivate)
       (unless (eq (car-safe font-lock-keywords) t)
         (setq font-lock-keywords
               (font-lock-compile-keywords font-lock-keywords))))
     ))


(cc-provide 'cc-fix)
;;; cc-fix.el ends here
