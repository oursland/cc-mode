;;; cc-menus.el --- menu and imenu support for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.07
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


;; menus for XEmacs
(defun c-mode-menu (&optional is-popup)
  ;; create a named menu for XEmacs.  when is-popup is non-nil, extra
  ;; words are added to the menu title.  TBD: this should all be
  ;; converted to easymenu!
  (let ((title (if is-popup
		   (concat mode-name " Mode Commands")
		 mode-name)))
    (cons title c-mode-menu)))

(defconst c-mode-menu
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
    )
  "Basic XEmacs 19 menu for C/C++/ObjC/Java modes.")

(defun c-mode-fsf-menu (name map)
  ;; Add menu to a keymap, but don't add them for XEmacs.  This
  ;; feature test will fail on other than Emacs 19.
  (condition-case nil
      (progn
	(define-key map [menu-bar] (make-sparse-keymap))
	(define-key map [menu-bar c] (cons name (make-sparse-keymap name)))

	(define-key map [menu-bar c comment-region]
	  '("Comment Out Region" . comment-region))
	(define-key map [menu-bar c c-macro-expand]
	  '("Macro Expand Region" . c-macro-expand))
	(define-key map [menu-bar c c-backslash-region]
	  '("Backslashify" . c-backslash-region))
	(define-key map [menu-bar c indent-exp]
	  '("Indent Expression" . c-indent-exp))
	(define-key map [menu-bar c indent-line]
	  '("Indent Line" . c-indent-command))
	(define-key map [menu-bar c fill]
	  '("Fill Comment Paragraph" . c-fill-paragraph))
	(define-key map [menu-bar c separator2]
	  '("----")) 
	(define-key map [menu-bar c up]
	  '("Up Conditional" . c-up-conditional))
	(define-key map [menu-bar c backward]
	  '("Backward Conditional" . c-backward-conditional))
	(define-key map [menu-bar c forward]
	  '("Forward Conditional" . c-forward-conditional))
	(define-key map [menu-bar c backward-stmt]
	  '("Backward Statement" . c-beginning-of-statement))
	(define-key map [menu-bar c forward-stmt]
	  '("Forward Statement" . c-end-of-statement))

	;; RMS says: mouse-3 should not select this menu.  mouse-3's
	;; global definition is useful in C mode and we should not
	;; interfere with that.  The menu is mainly for beginners, and
	;; for them, the menubar requires less memory than a special
	;; click.
	t)
    (error nil)))


(provide 'cc-menus)
;;; cc-menus.el ends here
