;;; cc-menus.el --- menu and imenu support for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.08
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; imenu integration
(defvar cc-imenu-c++-generic-expression
  (` 
   ((nil
     (, 
      (concat
       "^"				      ; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?"   ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	      ; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	      ; more than 3 tokens, right?
        
       "\\("				      ; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)"     ; either ptr/ref sign or ws
       "\\)?"				      ; if there is a last type spec
       "\\("				      ; name, take into the imenu entry
       "[a-zA-Z0-9_:~]+"		      ; member func, ctor or dtor...
 					      ; (may not contain * because then
 					      ; "a::operator char*" would
					      ; become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"		      ; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*[^		;]"   ; require something other than
					      ; a `;' after the (...) to
					      ; avoid prototypes.  Can't
					      ; catch cases with () inside
					      ; the parentheses surrounding
					      ; the parameters.  e.g.:
					      ; "int foo(int a=bar()) {...}"
        
       )) 6)    
    ("Class" 
     (, (concat 
 	 "^"				      ; beginning of line is required
 	 "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
 	 "class[ \t]+"
 	 "\\([a-zA-Z0-9_]+\\)"		      ; the string we want to get
 	 "[ \t]*[:{]"
 	 )) 2)))
  "Imenu generic expression for C++ mode.  See `imenu-generic-expression'.")
 
(defvar cc-imenu-c-generic-expression
  cc-imenu-c++-generic-expression
  "Imenu generic expression for C mode.  See `imenu-generic-expression'.")

;(defvar cc-imenu-objc-generic-expression
;  ())
; Please contribute one!

(defvar cc-imenu-java-generic-expression
  (`
   ((nil
     (,
      (concat
       "^\\([ \t]\\)*"
       "\\([A-Za-z0-9_-]+[ \t]+\\)?"	      ; type specs; there can be
        "\\([A-Za-z0-9_-]+[ \t]+\\)?"	      ; more than 3 tokens, right?
       "\\([A-Za-z0-9_-]+[ \t]*[[]?[]]?\\)"
       "\\([ \t]\\)"
       "\\([A-Za-z0-9_-]+\\)"		      ; the string we want to get
       "\\([ \t]*\\)+("
       "\\([a-zA-Z,_1-9\n \t]*[[]?[]]?\\)*"   ; arguments
       ")[ \t]*"
       "[^;(]"
       "[,a-zA-Z_1-9\n \t]*{"               
       )) 6)))
  "Imenu generic expression for Java mode.  See `imenu-generic-expression'.")


;; menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(require 'easymenu)

(defvar c-c-menu nil)
(defvar c-c++-menu nil)
(defvar c-objc-menu nil)
(defvar c-java-menu nil)

(defun c-mode-menu (modestr)
  (let ((m
	 '(["Comment Out Region"     comment-region (mark)]
	   ["Macro Expand Region"    c-macro-expand (mark)]
	   ["Backslashify"           c-backslash-region (mark)]
	   ["Indent Expression"      c-indent-exp
	    (memq (following-char) '(?\( ?\[ ?\{))]
	   ["Indent Line"            c-indent-command t]
	   ["Fill Comment Paragraph" c-fill-paragraph t]
	   ["Up Conditional"         c-up-conditional t]
	   ["Backward Conditional"   c-backward-conditional t]
	   ["Forward Conditional"    c-forward-conditional t]
	   ["Backward Statement"     c-beginning-of-statement t]
	   ["Forward Statement"      c-end-of-statement t]
	   )))
    (cons modestr m)))

(eval-when-compile
  (load-file "./cc-langs.el"))

;; See the explanation in cc-mode.el, under c-emacs-features for why
;; this workaround is necessary.  In Emacs, we punt because Emacs will
;; put the mode name in the menu title of the popup, but we have to
;; put "CC Mode" in the menu title of the menubar entry.  Sigh.
(if (memq 'duplicate-menus c-emacs-features)
    ;; Emacs
    (easy-menu-define c-c-menu c-mode-map "CC Mode Commands"
		      (c-mode-menu "CC Mode"))
  ;; XEmacs
  (easy-menu-define c-c-menu c-mode-map "C Mode Commands"
		    (c-mode-menu "C"))
  (easy-menu-define c-c++-menu c++-mode-map "C++ Mode Commands"
		    (c-mode-menu "C++"))
  (easy-menu-define c-objc-menu objc-mode-map "ObjC Mode Commands"
		    (c-mode-menu "ObjC"))
  (easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		    (c-mode-menu "Java"))
  )


(provide 'cc-menus)
;;; cc-menus.el ends here
