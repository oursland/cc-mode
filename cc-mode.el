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


(defconst c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     )
    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("bsd"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))

     )
    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)))
     (c-offsets-alist . ((topmost-intro        . 0)
                         (topmost-intro-cont   . 0)
                         (substatement         . +)
			 (substatement-open    . 0)
                         (case-label           . +)
                         (access-label         . -)
                         (inclass              . ++)
                         (inline-open          . 0)
                         ))
     )
    ("linux"
     (c-basic-offset  . 8)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((brace-list-open)
				(substatement-open after)
				(block-close . c-snug-do-while)))
     (c-cleanup-list . (brace-else-brace))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro     . 0)
			 (substatement-open     . 0)
			 (label                 . 0)
			 (statement-cont        . +)
			 ))
     )
    ("python"
     (indent-tabs-mode . t)
     (fill-column      . 72)
     (c-basic-offset   . 8)
     (c-offsets-alist  . ((substatement-open . 0)
			  ))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(substatement-open after)
				(block-close . c-snug-do-while)
				))
     )
    ("java"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
 			 (substatement-open     . +)
 			 (label                 . 0)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro . c-lineup-arglist-intro-after-paren)
 			 (arglist-close . c-lineup-arglist)
 			 (access-label  . 0)
			 (inher-cont    . c-lineup-java-inher)
			 ))

     )
    )
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any CC Mode variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `c-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

Note that all styles inherit from the `cc-mode' style, which is
computed at the time the mode is loaded.")

(defvar c-file-style nil
  "*Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `c-set-style'.  When the file is visited, CC Mode
will set the style of the file to this value automatically.

Note that file style settings are applied before file offset settings
as designated in the variable `c-file-offsets'.")

(defvar c-file-offsets nil
  "*Variable interface for setting offsets via File Local Variables.
In a file's Local Variable section, you can set this variable to an
association list similar to the values allowed in `c-offsets-alist'.
When the file is visited, CC Mode will institute these offset settings
automatically.

Note that file offset settings are applied after file style settings
as designated in the variable `c-file-style'.")

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

;; Shut the byte-compiler up.  Leave this in for development version
;;(byte-compiler-options (warnings nil))

;; figure out what features this Emacs has
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

(defvar c-syntactic-context nil
  "Variable containing syntactic analysis list during indentation.")


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

(defsubst c-make-inherited-keymap ()
  (let ((map (make-sparse-keymap)))
    (cond
     ;; XEmacs 19 & 20
     ((fboundp 'set-keymap-parents)
      (set-keymap-parents c++-mode-map c-mode-map))
     ;; Emacs 19
     ((fboundp 'set-keymap-parent)
      (set-keymap-parent c++-mode-map c-mode-map))
     ;; incompatible
     (t (error "CC Mode is incompatible with this version of Emacs")))
    map))

(defun c-populate-syntax-table (table)
  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."     table)
  (modify-syntax-entry ?-  "."     table)
  (modify-syntax-entry ?=  "."     table)
  (modify-syntax-entry ?%  "."     table)
  (modify-syntax-entry ?<  "."     table)
  (modify-syntax-entry ?>  "."     table)
  (modify-syntax-entry ?&  "."     table)
  (modify-syntax-entry ?|  "."     table)
  (modify-syntax-entry ?\' "\""    table))

(defun c-setup-dual-comments (table)
  ;; Set up TABLE to handle block and line style comments
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"    table))
   ;; Emacs 19
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"   table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs"))
   ))


;; internal state variables
(defvar c-hungry-delete-key nil
  "Internal state of hungry delete key feature.")
(make-variable-buffer-local 'c-hungry-delete-key)

(defvar c-auto-newline nil
  "Internal state of auto newline feature.")
(make-variable-buffer-local 'c-auto-newline)

(defvar c-auto-hungry-string nil
  "Internal auto-newline/hungry-delete designation string for mode line.")
(make-variable-buffer-local 'c-auto-hungry-string)

(defvar c-comment-start-regexp nil
  "Buffer local variable describing how comment are introduced.")
(make-variable-buffer-local 'c-comment-start-regexp)

(defvar c-conditional-key nil
  "Buffer local language-specific conditional keyword regexp.")
(make-variable-buffer-local 'c-conditional-key)

(defvar c-access-key nil
  "Buffer local language-specific access key regexp.")
(make-variable-buffer-local 'c-access-key)

(defvar c-class-key nil
  "Buffer local language-specific class key regexp.")
(make-variable-buffer-local 'c-class-key)

(defvar c-method-key nil
  "Buffer local language-specific method regexp.")
(make-variable-buffer-local 'c-method-key)

(defvar c-double-slash-is-comments-p nil
  "Buffer local language-specific comment style flag.")
(make-variable-buffer-local 'c-double-slash-is-comments-p)

(defconst c-protection-key
  "\\<\\(public\\|protected\\|private\\)\\>"
  "Regexp describing protection keywords.")

(defconst c-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a C/C++/ObjC symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")

(defvar c-baseclass-key
  (concat
   ":?[ \t]*\\(virtual[ \t]+\\)?\\("
   c-protection-key "[ \t]+\\)" c-symbol-key)
  "Regexp describing C++ base classes in a derived class definition.")
(make-variable-buffer-local 'c-baseclass-key)

;; defconst'd instead of defvar'd to override any old pre-loaded versions
(defvar c-recognize-knr-p t
  "Non-nil means K&R style argument declarations are valid.")
(make-variable-buffer-local 'c-recognize-knr-p)

(defvar c-indentation-style c-site-default-style
  "Name of style installed in the current buffer.")


;; This comment was here before me:
;;
;;     cmacexp is lame because it uses no preprocessor symbols.  It
;;     isn't very extensible either -- hardcodes /lib/cpp.
;;
;; I add it here only because c-mode had it -- BAW
;;
(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)


;; constant regular expressions for looking at various constructs
(defconst c-C++-class-key "\\(class\\|struct\\|union\\)"
  "Regexp describing a C++ class declaration, including templates.")

(defconst c-C-class-key "\\(struct\\|union\\)"
  "Regexp describing a C struct declaration.")

(defconst c-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  c-C++-class-key "[ \t]+" c-symbol-key
	  "\\([ \t]*:[ \t]*\\)\\s *[^;]")
  "Regexp describing a class inheritance declaration.")

(defconst c-switch-label-key
  "\\(\\(case[( \t]+\\S .*\\)\\|default[ \t]*\\):"
  "Regexp describing a switch's case or default label")

(defconst c-C++-access-key
  (concat c-protection-key "[ \t]*:")
  "Regexp describing C++ access specification keywords.")

(defconst c-label-key
  (concat c-symbol-key ":\\([^:]\\|$\\)")
  "Regexp describing any label.")

(defconst c-C-conditionals '("for" "if" "do" "else" "while" "switch")
  "Shared conditional keywords for C-like languages.")

(defconst c-C-conditional-key
  (concat "\\b\\("
	  (mapconcat 'identity c-C-conditionals "\\|")
	  "\\)\\b[^_]")
  "Regexp describing a conditional control for C.")

(defconst c-C++-conditional-key
  (concat "\\b\\(" (mapconcat 'identity
			      (append '("try" "catch") c-C-conditionals) "\\|")
	  "\\)\\b[^_]")
  "Regexp describing a conditional control for C++.")

(defconst c-C++-friend-key
  "friend[ \t]+\\|template[ \t]*<.+>[ \t]*friend[ \t]+"
  "Regexp describing friend declarations in C++ classes.")

;; comment starter definitions for various languages.  the language
;; modes will set c-comment-start-regexp to this value.
(defconst c-C++-comment-start-regexp "/[/*]")
(defconst c-C-comment-start-regexp "/[*]")
;; We need to match all 3 Java style comments
;; 1) Traditional C block; 2) javadoc /** ...; 3) C++ style
(defconst c-Java-comment-start-regexp "/\\(/\\|[*][*]?\\)")

(defconst c-ObjC-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in objc syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key)
  "Regexp describing an Objective-C method intro.")
(defconst c-ObjC-access-key
  (concat "@" c-protection-key)
  "Regexp describing access specification keywords for Objective-C.")
(defconst c-ObjC-class-key
  (concat
   "@\\(interface\\|implementation\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *:\\s *" c-symbol-key "\\)?"	;maybe followed by the superclass
   "\\(\\s *<[^>]+>\\)?"		;and maybe the adopted protocols list
   )
  "Regexp describing a class or protocol declaration for Objective-C.")

(defconst c-Java-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in java syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key)
  "Regexp describing a Java method intro.")
(defconst c-Java-access-key nil
  "Regexp describing access labels for Java.")
(defconst c-Java-class-key
  (concat
   "\\(" c-protection-key "\\s +\\)?"
   "\\(interface\\|class\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass 
   ;;"\\(\\s *implements *[^{]+{\\)?"	;and maybe the adopted protocols list
   )
  "Regexp describing a class or protocol declaration for Java.")
(defconst c-Java-special-key "\\(implements\\|extends\\|throws\\)[^_]"
  "Regexp describing Java inheritance and throws clauses.")
(defconst c-Java-conditional-key
  (concat "\\b\\("
	  (mapconcat 'identity
		     (append '("try" "catch" "finally" "synchronized")
			     c-C-conditionals) "\\|")
	  "\\)\\b[^_]")
  "Regexp describing a conditional control for Java.")
(defconst c-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*"
  "Regexp describing the beginning of a Java top-level definition.")

;; KLUDGE ALERT.  We default these variables to their `C' values so
;; that non-cc-mode-ized modes that depend on c-mode will still work
;; out of the box.  The most glaring example is awk-mode.  There ought
;; to be a better way.
(setq-default c-conditional-key c-C-conditional-key
	      c-class-key c-C-class-key
	      c-comment-start-regexp c-C-comment-start-regexp)


;; main entry points for the modes
(defconst c-list-of-mode-names nil)

(defun c-use-java-style ()
  "Institutes `java' indentation style.
For use with the variable `java-mode-hook'."
  (c-set-style "java"))

(defun c-common-init ()
  ;; Common initializations for c++-mode and c-mode.
  ;;
  ;; these variables should always be buffer local; they do not affect
  ;; indentation style.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'imenu-generic-expression) ;set in the mode functions
  ;; Emacs 19.30 and beyond only, AFAIK
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'c-fill-paragraph)))
  ;; now set their values
  (setq paragraph-start (concat page-delimiter "\\|$")
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *"
	adaptive-fill-regexp nil)
  ;; we have to do something special for c-offsets-alist so that the
  ;; buffer local value has its own alist structure.
  (setq c-offsets-alist (copy-alist c-offsets-alist))
  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (if (boundp 'comment-indent-function)
      (progn
	   (make-local-variable 'comment-indent-function)
	   (setq comment-indent-function 'c-comment-indent))
    (make-local-variable 'comment-indent-hook)
    (setq comment-indent-hook 'c-comment-indent))
  ;; Put C menu into menubar and on popup menu for XEmacs 19. I think
  ;; this happens automatically for Emacs 19.  Skip it for Infodock.
  (if (and (not (memq 'infodock c-emacs-features))
	   (boundp 'current-menubar)
	   current-menubar
	   (not (assoc mode-name current-menubar)))
      ;; its possible that this buffer has changed modes from one of
      ;; the other CC Mode modes.  In that case, only the menubar
      ;; title of the menu changes.
      (let ((modes (copy-sequence c-list-of-mode-names))
	    changed-p)
	(setq modes (delete major-mode modes))
	(while modes
	  (if (not (assoc (car modes) current-menubar))
	      (setq modes (cdr modes))
	    (relabel-menu-item (list (car modes)) mode-name)
	    (setq modes nil
		  changed-p t)))
	(if (not changed-p)
	    (progn
	      (set-buffer-menubar (copy-sequence current-menubar))
	      (if (fboundp 'add-submenu)
		  (add-submenu nil (c-mode-menu))
		(add-menu nil mode-name c-mode-menu)
		)))))
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu (c-mode-menu)))
  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist))))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'."
  ;; apply file styles and offsets
  (and c-file-style
       (c-set-style c-file-style))
  (and c-file-offsets
       (mapcar
	(function
	 (lambda (langentry)
	   (let ((langelem (car langentry))
		 (offset (cdr langentry)))
	     (c-set-offset langelem offset)
	     )))
	c-file-offsets)))

(add-hook 'hack-local-variables-hook 'c-postprocess-file-styles)


(defsubst c-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  (let ((here (point)))
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'bod)
      (beginning-of-defun)
      ;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at
      ;; the open brace.
      (and defun-prompt-regexp
	   (looking-at defun-prompt-regexp)
	   (goto-char (match-end 0)))
      )
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl)
      (forward-line -1)
      (back-to-indentation))
     ((eq position 'ionl)
      (forward-line 1)
      (back-to-indentation))
     (t (error "unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))

(defsubst c-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  (and c-auto-newline
       (not (c-in-literal))
       (not (newline))))

(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defsubst c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defsubst c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  (assq (car (c-intersect-lists list alist1)) alist2))


;; This is used by indent-for-comment to decide how much to indent a
;; comment in C code based on its context.
(defun c-comment-indent ()
  (if (looking-at (concat "^\\(" c-comment-start-regexp "\\)"))
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: A comment following a solitary close-brace should
	 ;; have only one space.
	 ((looking-at (concat "[ \t]*}[ \t]*\\($\\|"
			      c-comment-start-regexp
			      "\\)"))
	  (search-forward "}")
	  (1+ (current-column)))
	 ;; CASE 2: 2 spaces after #endif
	 ((or (looking-at "^#[ \t]*endif[ \t]*")
	      (looking-at "^#[ \t]*else[ \t]*"))
	  7)
	 ;; CASE 3: when comment-column is nil, calculate the offset
	 ;; according to c-offsets-alist.  E.g. identical to hitting
	 ;; TAB.
	 ((and c-indent-comments-syntactically-p
	       (save-excursion
		 (skip-chars-forward " \t")
		 (or (looking-at comment-start)
		     (eolp))))
	  (let ((syntax (c-guess-basic-syntax)))
	    ;; BOGOSITY ALERT: if we're looking at the eol, its
	    ;; because indent-for-comment hasn't put the comment-start
	    ;; in the buffer yet.  this will screw up the syntactic
	    ;; analysis so we kludge in the necessary info.  Another
	    ;; kludge is that if we're at the bol, then we really want
	    ;; to ignore any anchoring as specified by
	    ;; c-comment-only-line-offset since it doesn't apply here.
	    (if (save-excursion
		  (beginning-of-line)
		  (skip-chars-forward " \t")
		  (eolp))
		(c-add-syntax 'comment-intro))
	    (let ((c-comment-only-line-offset
		   (if (consp c-comment-only-line-offset)
		       c-comment-only-line-offset
		     (cons c-comment-only-line-offset
			   c-comment-only-line-offset))))
	      (apply '+ (mapcar 'c-get-offset syntax)))))
	 ;; CASE 4: use comment-column if previous line is a
	 ;; comment-only line indented to the left of comment-column
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at c-comment-start-regexp)
	      (setq placeholder (point))))
	  (goto-char placeholder)
	  (if (< (current-column) comment-column)
	      comment-column
	    (current-column)))
	 ;; CASE 5: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 6: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))

;; used by outline-minor-mode
(defun c-outline-level ()
  (save-excursion
    (skip-chars-forward "\t ")
    (current-column)))


;; Useful for c-hanging-semi&comma-criteria

(defun c-semi&comma-inside-parenlist ()
  "Determine if a newline should be added after a semicolon.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (/= last-command-char ?\;)
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (/= (following-char) ?\())
	  (error t))
	t
      'stop)))


(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations.

Optional prefix ARG means justify paragraph as well."
  (interactive "P")
  (let* (comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t\n")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 "\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"))
    (if (and c-double-slash-is-comments-p
	     (save-excursion
	       (beginning-of-line)
	       (looking-at ".*//")))
	(let (fill-prefix
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1))
	      (paragraph-separate (concat paragraph-separate re1)))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp))
 			(looking-at "[ \t]*//[ \t]*[^ \t\n]"))
	      (forward-line -1))
 	    (if (not (looking-at ".*//[ \t]*[^ \t\n]"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.
	    (setq fill-prefix (buffer-substring (match-beginning 0)
						(match-end 0)))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (c-point 'bol)
				(save-excursion
				  (forward-line 1)
				  (while (looking-at fill-prefix)
				    (forward-line 1))
				  (point)))
	      (fill-paragraph arg)
	      t)))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (eq (c-in-literal) 'c)
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point))))))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 ;; The prefix for each line of this paragraph
		 ;; is the appropriate part of the start of this line,
		 ;; up to the column at which text should be indented.
		 (save-excursion
		   (beginning-of-line)
		   (if (looking-at "[ \t]*/\\*.*\\*/")
		       (progn (re-search-forward comment-start-skip)
			      (make-string (current-column) ?\ ))
		     (if first-line (forward-line 1))

		     (let ((line-width (progn (end-of-line) (current-column))))
		       (beginning-of-line)
		       (prog1
			   (buffer-substring
			    (point)

			    ;; How shall we decide where the end of the
			    ;; fill-prefix is?
			    (progn
			      (beginning-of-line)
			      (skip-chars-forward " \t*" (c-point 'eol))
			      ;; kludge alert, watch out for */, in
			      ;; which case fill-prefix should *not*
			      ;; be "*"!
			      (if (and (= (following-char) ?/)
				       (= (preceding-char) ?*))
				  (forward-char -1))
			      (point)))

			 ;; If the comment is only one line followed
			 ;; by a blank line, calling move-to-column
			 ;; above may have added some spaces and tabs
			 ;; to the end of the line; the fill-paragraph
			 ;; function will then delete it and the
			 ;; newline following it, so we'll lose a
			 ;; blank line when we shouldn't.  So delete
			 ;; anything move-to-column added to the end
			 ;; of the line.  We record the line width
			 ;; instead of the position of the old line
			 ;; end because move-to-column might break a
			 ;; tab into spaces, and the new characters
			 ;; introduced there shouldn't be deleted.

			 ;; If you can see a better way to do this,
			 ;; please make the change.  This seems very
			 ;; messy to me.
			 (delete-region (progn (move-to-column line-width)
					       (point))
					(progn (end-of-line) (point))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (concat paragraph-start re1))
		(paragraph-separate (concat paragraph-separate re1))
		(chars-to-delete 0)
		)
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  (if (and (not c-hanging-comment-starter-p)
					   (looking-at
					    (concat c-comment-start-regexp
						    "[ \t]*$")))
				      (forward-line 1))
				  ;; Protect text before the comment
				  ;; start by excluding it.  Add
				  ;; spaces to bring back proper
				  ;; indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (forward-line 1)
				  (point)))
	      (fill-paragraph arg)
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of
		;; buffer, given the narrowing) and don't leave it on
		;; its own line, unless that's the style that's desired.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (and c-hanging-comment-ender-p
			 (looking-at "[ \t]*\\*/"))
		    ;(delete-indentation)))))
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max))))))
	    t)))))


(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.
When going backwards, `#elif' is treated like `#else' followed by
`#if'.  When going forwards, `#elif' is ignored."
  (interactive "p")
  (c-forward-conditional (- count) t)
  (c-keep-region-active))

(defun c-backward-conditional (count &optional up-flag)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional."
  (interactive "p")
  (c-forward-conditional (- count) up-flag)
  (c-keep-region-active))

(defun c-forward-conditional (count &optional up-flag)
  "Move forward across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward across a preprocessor conditional."
  (interactive "p")
  (let* ((forward (> count 0))
	 (increment (if forward -1 1))
	 (search-function (if forward 're-search-forward 're-search-backward))
	 (new))
    (save-excursion
      (while (/= count 0)
	(let ((depth (if up-flag 0 -1)) found)
	  (save-excursion
	    ;; Find the "next" significant line in the proper direction.
	    (while (and (not found)
			;; Rather than searching for a # sign that
			;; comes at the beginning of a line aside from
			;; whitespace, search first for a string
			;; starting with # sign.  Then verify what
			;; precedes it.  This is faster on account of
			;; the fastmap feature of the regexp matcher.
			(funcall search-function
				 "#[ \t]*\\(if\\|elif\\|endif\\)"
				 nil t))
	      (beginning-of-line)
	      ;; Now verify it is really a preproc line.
	      (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\)")
		  (let ((prev depth))
		    ;; Update depth according to what we found.
		    (beginning-of-line)
		    (cond ((looking-at "[ \t]*#[ \t]*endif")
			   (setq depth (+ depth increment)))
			  ((looking-at "[ \t]*#[ \t]*elif")
			   (if (and forward (= depth 0))
			       (setq found (point))))
			  (t (setq depth (- depth increment))))
		    ;; If we are trying to move across, and we find an
		    ;; end before we find a beginning, get an error.
		    (if (and (< prev 0) (< depth prev))
			(error (if forward
				   "No following conditional at this level"
				 "No previous conditional at this level")))
		    ;; When searching forward, start from next line so
		    ;; that we don't find the same line again.
		    (if forward (forward-line 1))
		    ;; If this line exits a level of conditional, exit
		    ;; inner loop.
		    (if (< depth 0)
			(setq found (point))))
		;; else
		(if forward (forward-line 1))
		)))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (push-mark)
    (goto-char new))
  (c-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional whole-exp)
  "Indent current line as C code, and/or insert some whitespace.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert some whitespace[*].  If
other than nil or t, then some whitespace[*] is inserted only within
literals (comments and strings) and inside preprocessor directives,
but the line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved.

  [*] The amount and kind of whitespace inserted is controlled by the
  variable `c-insert-tab-function', which is called to do the actual
  insertion of whitespace.  Normally the function in this variable
  just inserts a tab character, or the equivalent number of spaces,
  depending on the variable `indent-tabs-mode'."

  (interactive "P")
  (let ((bod (c-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c-indent-line))
	      beg end)
	  (save-excursion
	    (if (eq c-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end (- shift-amt) "#")))
      ;; No arg supplied, use c-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (funcall c-insert-tab-function)
	  (c-indent-line)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(c-indent-line))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-in-literal bod)
	    (funcall c-insert-tab-function))
	(c-indent-line)
	)))))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in balanced expression following point.
Optional SHUTUP-P if non-nil, inhibits message printing and error checking."
  (interactive "P")
  (let ((here (point))
	end progress-p)
    (unwind-protect
	(let ((c-echo-syntactic-information-p nil) ;keep quiet for speed
	      (start (progn
		       ;; try to be smarter about finding the range of
		       ;; lines to indent. skip all following
		       ;; whitespace. failing that, try to find any
		       ;; opening brace on the current line
		       (skip-chars-forward " \t\n")
		       (if (memq (following-char) '(?\( ?\[ ?\{))
			   (point)
			 (let ((state (parse-partial-sexp (point)
							  (c-point 'eol))))
			   (and (nth 1 state)
				(goto-char (nth 1 state))
				(memq (following-char) '(?\( ?\[ ?\{))
				(point)))))))
	  ;; find balanced expression end
	  (setq end (and (c-safe (progn (forward-sexp 1) t))
			 (point-marker)))
	  ;; sanity check
	  (and (not start)
	       (not shutup-p)
	       (error "Cannot find start of balanced expression to indent."))
	  (and (not end)
	       (not shutup-p)
	       (error "Cannot find end of balanced expression to indent."))
	  (c-progress-init start end 'c-indent-exp)
	  (setq progress-p t)
	  (goto-char start)
	  (beginning-of-line)
	  (while (< (point) end)
	    (if (not (looking-at "[ \t]*$"))
		(c-indent-line))
	    (c-progress-update)
	    (forward-line 1)))
      ;; make sure marker is deleted
      (and end
	   (set-marker end nil))
      (and progress-p
	   (c-progress-fini 'c-indent-exp))
      (goto-char here))))

(defun c-indent-defun ()
  "Re-indents the current top-level function def, struct or class declaration."
  (interactive)
  (let ((here (point-marker))
	(c-echo-syntactic-information-p nil)
	(brace (c-least-enclosing-brace (c-parse-state))))
    (if brace
	(goto-char brace)
      (beginning-of-defun))
    ;; if we're sitting at b-o-b, it might be because there was no
    ;; least enclosing brace and we were sitting on the defun's open
    ;; brace.
    (if (and (bobp) (not (= (following-char) ?\{)))
	(goto-char here))
    ;; if defun-prompt-regexp is non-nil, b-o-d might not leave us at
    ;; the open brace. I consider this an Emacs bug.
    (and (boundp 'defun-prompt-regexp)
	 defun-prompt-regexp
	 (looking-at defun-prompt-regexp)
	 (goto-char (match-end 0)))
    ;; catch all errors in c-indent-exp so we can 1. give more
    ;; meaningful error message, and 2. restore point
    (unwind-protect
	(c-indent-exp)
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (save-excursion
    (goto-char start)
    ;; Advance to first nonblank line.
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let (endmark)
      (unwind-protect
	  (let ((c-tab-always-indent t)
		;; shut up any echo msgs on indiv lines
		(c-echo-syntactic-information-p nil)
		fence)
	    (c-progress-init start end 'c-indent-region)
	    (setq endmark (copy-marker end))
	    (while (and (bolp)
			(not (eobp))
			(< (point) endmark))
	      ;; update progress
	      (c-progress-update)
	      ;; Indent one line as with TAB.
	      (let (nextline sexpend sexpbeg)
		;; skip blank lines
		(skip-chars-forward " \t\n")
		(beginning-of-line)
		;; indent the current line
		(c-indent-line)
		(setq fence (point))
		(if (save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*#"))
		    (forward-line 1)
		  (save-excursion
		    ;; Find beginning of following line.
		    (setq nextline (c-point 'bonl))
		    ;; Find first beginning-of-sexp for sexp extending past
		    ;; this line.
		    (beginning-of-line)
		    (while (< (point) nextline)
		      (condition-case nil
			  (progn
			    (forward-sexp 1)
			    (setq sexpend (point)))
			(error (setq sexpend nil)
			       (goto-char nextline)))
		      (c-forward-syntactic-ws))
		    (if sexpend
			(progn 
			  ;; make sure the sexp we found really starts on the
			  ;; current line and extends past it
			  (goto-char sexpend)
			  (setq sexpend (point-marker))
			  (c-safe (backward-sexp 1))
			  (setq sexpbeg (point))))
		    (if (and sexpbeg (< sexpbeg fence))
			(setq sexpbeg fence)))
		  ;; check to see if the next line starts a
		  ;; comment-only line
		  (save-excursion
		    (forward-line 1)
		    (skip-chars-forward " \t")
		    (if (looking-at c-comment-start-regexp)
			(setq sexpbeg (c-point 'bol))))
		  ;; If that sexp ends within the region, indent it all at
		  ;; once, fast.
		  (condition-case nil
		      (if (and sexpend
			       (> sexpend nextline)
			       (<= sexpend endmark))
			  (progn
			    (goto-char sexpbeg)
			    (c-indent-exp 'shutup)
			    (c-progress-update)
			    (goto-char sexpend)))
		    (error
		     (goto-char sexpbeg)
		     (c-indent-line)))
		  ;; Move to following line and try again.
		  (and sexpend
		       (markerp sexpend)
		       (set-marker sexpend nil))
		  (forward-line 1)
		  (setq fence (point))))))
	(set-marker endmark nil)
	(c-progress-fini 'c-indent-region)
	))))

(defun c-mark-function ()
  "Put mark at end of a C, C++, or Objective-C defun, point at beginning."
  (interactive)
  (let ((here (point))
	;; there should be a c-point position for 'eod
	(eod  (save-excursion (end-of-defun) (point)))
	(state (c-parse-state))
	brace)
    (while state
      (setq brace (car state))
      (if (consp brace)
	  (goto-char (cdr brace))
	(goto-char brace))
      (setq state (cdr state)))
    (if (= (following-char) ?{)
	(progn
	  (forward-line -1)
	  (while (not (or (bobp)
			  (looking-at "[ \t]*$")))
	    (forward-line -1)))
      (forward-line 1)
      (skip-chars-forward " \t\n"))
    (push-mark here)
    (push-mark eod nil t)))


;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  ;; start the progress update messages.  if this emacs doesn't have a
  ;; built-in timer, just be dumb about it
  (if (not (fboundp 'current-time))
      (message "indenting region... (this may take a while)")
    ;; if progress has already been initialized, do nothing. otherwise
    ;; initialize the counter with a vector of:
    ;; [start end lastsec context]
    (if c-progress-info
	()
      (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "indenting region..."))))

(defun c-progress-update ()
  ;; update progress
  (if (not (and c-progress-info c-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref c-progress-info 0))
	  (end (aref c-progress-info 1))
	  (lastsecs (aref c-progress-info 2)))
      ;; should we update?  currently, update happens every 2 seconds,
      ;; what's the right value?
      (if (< c-progress-interval (- now lastsecs))
	  (progn
	    (message "indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset c-progress-info 2 now)))
      )))

(defun c-progress-fini (context)
  ;; finished
  (if (or (eq context (aref c-progress-info 3))
	  (eq context t))
      (progn
	(set-marker (aref c-progress-info 1) nil)
	(setq c-progress-info nil)
	(message "indenting region...done"))))


;; indent via syntactic language elements
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
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if c-strict-syntax-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)         (setq offset c-basic-offset))
     ((eq offset '-)         (setq offset (- c-basic-offset)))
     ((eq offset '++)        (setq offset (* 2 c-basic-offset)))
     ((eq offset '--)        (setq offset (* 2 (- c-basic-offset))))
     ((eq offset '*)         (setq offset (/ c-basic-offset 2)))
     ((eq offset '/)         (setq offset (/ (- c-basic-offset) 2)))
     ((c-functionp offset)   (setq offset (funcall offset langelem)))
     ((not (numberp offset)) (setq offset (symbol-value offset)))
     )
    (+ (if (and relpos
		(< relpos (c-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

(defun c-indent-line (&optional syntax)
  ;; indent the current line as C/C++/ObjC code. Optional SYNTAX is the
  ;; syntactic information for the current line. Returns the amount of
  ;; indentation change
  (let* ((c-syntactic-context (or syntax (c-guess-basic-syntax)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'c-get-offset c-syntactic-context)))
	 (shift-amt  (- (current-indentation) indent)))
    (and c-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" c-syntactic-context indent))
    (if (zerop shift-amt)
	nil
      (delete-region (c-point 'bol) (c-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (c-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'c-special-indent-hook)
    shift-amt))

(defun c-show-syntactic-information (arg)
  "Show syntactic information for current line.
With universal argument, inserts the analysis as a comment on that line."
  (interactive "P")
  (let ((syntax (c-guess-basic-syntax)))
    (if (not (consp arg))
	(message "syntactic analysis: %s" (c-guess-basic-syntax))
      (indent-for-comment)
      (insert (format "%s" syntax))
      ))
  (c-keep-region-active))


;;; This page handles insertion and removal of backslashes for C macros.

(defun c-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify blank lines at the start of the region.
If the region ends at the start of a line, it always deletes the
backslash (if any) at the end of the previous line.
 
You can put the region around an entire macro definition and use this
command to conveniently insert and align the necessary backslashes."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (let ((column c-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if (not delete-flag)
          (while (< (point) to)
            (end-of-line)
            (if (= (preceding-char) ?\\)
                (progn (forward-char -1)
                       (skip-chars-backward " \t")))
            (setq column (max column (1+ (current-column))))
            (forward-line 1)))
      ;; Adjust upward to a tab column, if that doesn't push past the margin.
      (if (> (% column tab-width) 0)
          (let ((adjusted (* (/ (+ column tab-width -1) tab-width) tab-width)))
            (if (< adjusted (window-width))
                (setq column adjusted))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (< (point) endmark)
	(if (and (not delete-flag)
 		 ;; Un-backslashify the last line
 		 ;; if the region ends right at the start of the next line.
 		 (save-excursion
 		   (forward-line 1)
 		   (< (point) endmark)))
            (c-append-backslash column)
          (c-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil)))
  (c-keep-region-active))

(defun c-append-backslash (column)
  (end-of-line)
  ;; Note that "\\\\" is needed to get one backslash.
  (if (= (preceding-char) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column))
    (indent-to column)
    (insert "\\")))

(defun c-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))


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

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
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


;; menus for XEmacs
(defun c-mode-menu ()
  (cons mode-name c-mode-menu))

(defun c-popup-menu (e)
  "Pops up the C/C++/ObjC menu."
  (interactive "@e")
  (popup-menu (c-mode-menu))
  (c-keep-region-active))
    

;; Emacs/XEmacs Compatibility
;; XEmacs has these, Emacs does not

(if (fboundp 'functionp)
    (defalias 'c-functionp 'functionp)
  ;; Lift XEmacs 19.13's functionp from subr.el
  (defun c-functionp (obj)
    "Returns t if OBJ is a function, nil otherwise."
    (cond
     ((symbolp obj) (fboundp obj))
     ((subrp obj))
     ((compiled-function-p obj))
     ((consp obj)
      (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
     (t nil))))

(if (fboundp 'copy-tree)
    (defalias 'c-copy-tree 'copy-tree)
  ;; Lift XEmacs 19.12's copy-tree
  (defun c-copy-tree (tree)
    (if (consp tree)
	(cons (c-copy-tree (car tree))
	      (c-copy-tree (cdr tree)))
      (if (vectorp tree)
	  (let* ((new (copy-sequence tree))
		 (i (1- (length new))))
	    (while (>= i 0)
	      (aset new i (c-copy-tree (aref new i)))
	      (setq i (1- i)))
	    new)
	tree))))


;; Dynamically append the default value of most variables. This is
;; crucial because future c-set-style calls will always reset the
;; variables first to the `cc-mode' style before instituting the new
;; style.  Only do this once!
(or (assoc "cc-mode" c-style-alist)
    (progn
      (c-add-style "cc-mode"
		   (mapcar
		    (function
		     (lambda (var)
		       (let ((val (symbol-value var)))
			 (cons var (if (atom val) val
				     (c-copy-tree val)
				     ))
			 )))
		    '(c-backslash-column
		      c-basic-offset
		      c-cleanup-list
		      c-comment-only-line-offset
		      c-electric-pound-behavior
		      c-hanging-braces-alist
		      c-hanging-colons-alist
		      c-hanging-comment-starter-p
		      c-hanging-comment-ender-p
		      c-offsets-alist
		      )))
      ;; the default style is now GNU.  This can be overridden in
      ;; c-mode-common-hook or {c,c++,objc,java}-mode-hook.
      (c-set-style c-site-default-style)))

(if c-style-variables-are-local-p
    (progn
      ;; style variables
      (make-variable-buffer-local 'c-offsets-alist)
      (make-variable-buffer-local 'c-basic-offset)
      (make-variable-buffer-local 'c-file-style)
      (make-variable-buffer-local 'c-file-offsets)
      (make-variable-buffer-local 'c-comment-only-line-offset)
      (make-variable-buffer-local 'c-cleanup-list)
      (make-variable-buffer-local 'c-hanging-braces-alist)
      (make-variable-buffer-local 'c-hanging-colons-alist)
      (make-variable-buffer-local 'c-hanging-comment-starter-p)
      (make-variable-buffer-local 'c-hanging-comment-ender-p)
      (make-variable-buffer-local 'c-backslash-column)
      (make-variable-buffer-local 'c-label-minimum-indentation)
      (make-variable-buffer-local 'c-special-indent-hook)
      (make-variable-buffer-local 'c-indentation-style)))


;; fsets for compatibility with BOCM
(fset 'electric-c-brace      'c-electric-brace)
(fset 'electric-c-semi       'c-electric-semi&comma)
(fset 'electric-c-sharp-sign 'c-electric-pound)
;; there is no CC Mode equivalent for electric-c-terminator
(fset 'mark-c-function       'c-mark-function)
(fset 'indent-c-exp          'c-indent-exp)
;;;###autoload (fset 'set-c-style           'c-set-style)
;; Lucid Emacs 19.9 + font-lock + CC Mode - c++-mode lossage
(fset 'c++-beginning-of-defun 'beginning-of-defun)
(fset 'c++-end-of-defun 'end-of-defun)

;; set up bc warnings for obsolete variables, but for now lets not
;; worry about obsolete functions.  maybe later some will be important
;; to flag
(let* ((na "Nothing appropriate.")
       (vars
	(list
	 (cons 'c++-c-mode-syntax-table 'c-mode-syntax-table)
	 (cons 'c++-tab-always-indent 'c-tab-always-indent)
	 (cons 'c++-always-arglist-indent-p na)
	 (cons 'c++-block-close-brace-offset 'c-offsets-alist)
	 (cons 'c++-paren-as-block-close-p na)
	 (cons 'c++-continued-member-init-offset 'c-offsets-alist)
	 (cons 'c++-member-init-indent 'c-offsets-alist)
	 (cons 'c++-friend-offset na)
	 (cons 'c++-access-specifier-offset 'c-offsets-alist)
	 (cons 'c++-empty-arglist-indent 'c-offsets-alist)
	 (cons 'c++-comment-only-line-offset 'c-comment-only-line-offset)
	 (cons 'c++-C-block-comments-indent-p na)
	 (cons 'c++-cleanup-list 'c-cleanup-list)
	 (cons 'c++-hanging-braces 'c-hanging-braces-alist)
	 (cons 'c++-hanging-member-init-colon 'c-hanging-colons-alist)
	 (cons 'c++-auto-hungry-initial-state
	       "Use `c-auto-newline' and `c-hungry-delete-key' instead.")
	 (cons 'c++-auto-hungry-toggle na)
	 (cons 'c++-relative-offset-p na)
	 (cons 'c++-special-indent-hook 'c-special-indent-hook)
	 (cons 'c++-delete-function 'c-delete-function)
	 (cons 'c++-electric-pound-behavior 'c-electric-pound-behavior)
	 (cons 'c++-hungry-delete-key 'c-hungry-delete-key)
	 (cons 'c++-auto-newline 'c-auto-newline)
	 (cons 'c++-match-header-strongly na)
	 (cons 'c++-defun-header-strong-struct-equivs na)
	 (cons 'c++-version 'c-version)
	 (cons 'c++-mode-help-address 'c-mode-help-address)
	 (cons 'c-indent-level 'c-basic-offset)
	 (cons 'c-brace-imaginary-offset na)
	 (cons 'c-brace-offset 'c-offsets-alist)
	 (cons 'c-argdecl-indent 'c-offsets-alist)
	 (cons 'c-label-offset 'c-offsets-alist)
	 (cons 'c-continued-statement-offset 'c-offsets-alist)
	 (cons 'c-continued-brace-offset 'c-offsets-alist)
	 (cons 'c-default-macroize-column 'c-backslash-column)
	 (cons 'c++-default-macroize-column 'c-backslash-column)
	 (cons 'c-block-comments-indent-p na)
	 )))
  (mapcar
   (function
    (lambda (elt)
      (make-obsolete-variable (car elt) (cdr elt))))
   vars))

(provide 'cc-mode)
;;; cc-mode.el ends here
