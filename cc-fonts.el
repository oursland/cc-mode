;;; cc-fonts.el --- custom indentation functions for CC Mode

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Authors:    2002- Martin Stjernholm
;;             jwz, then rms, then sm
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    07-Jan-2002
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

;; The use of standard faces:
;;
;; font-lock-comment-face	Comments.
;; font-lock-string-face	String and character literals.
;; font-lock-keyword-face	Keywords.
;; font-lock-builtin-face	?
;; font-lock-function-name-face	Functions and classes in function contexts
;;				(i.e. definition and instantiation).  Also
;;				used for preprocessor defines with arguments.
;; font-lock-variable-name-face	Variables, and other identifiers in variable
;;				contexts.  Also used for preprocessor defines
;;				without arguments.
;; font-lock-type-face		Types (both pre- and user defined) and classes
;;				in type contexts.
;; font-lock-reference-face	Labels and name qualifiers.
;;
;; Face aliases, mapped to different faces depending on (X)Emacs flavor:
;;
;; c-doc-face			Documentation comments (like Javadoc).
;; c-preprocessor-face		Preprocessor directives.
;; c-invalid-face		Invalid syntax.
;;
;; Special faces:
;;
;; c-doc-markup-face		Special markup in doc comments.
;;				Overlaid over c-doc-face.
;;
;; Some comments on the use of faces:
;;
;; o  Emacs deprecates `font-lock-reference-face' and makes it an alias
;;    for `font-lock-constant-face'.  However, CC Mode uses
;;    `font-lock-reference-face' anyway for two reasons: Firstly it's
;;    defined in Emacs 19.34 and XEmacs.  Secondly it's more accurate
;;    for the actual use in CC Mode; a face for constants should
;;    highlight constant identifiers, i.e. those with const or similar
;;    in their definitions.  CC Mode doesn't do that, and even if it
;;    would this would be the wrong face, since those constants are
;;    closer related to variable and function identifiers.
;;
;; o  `c-doc-face' is an alias for `font-lock-doc-string-face' in
;;    XEmacs, `font-lock-doc-face' in Emacs 21 and later, or
;;    `font-lock-string-face' in older Emacs.  FIXME: Doc comment
;;    fontification currently only works with font-lock packages that
;;    have `font-lock-syntactic-face-function', i.e Emacs 21 and
;;    later.
;;
;; o  `c-preprocessor-face' is an alias for
;;    `font-lock-preprocessor-face' in XEmacs and - in lack of a
;;    closer equivalent - `font-lock-builtin-face' or
;;    `font-lock-reference-face' in Emacs.
;;
;; o  `c-invalid-face' is an alias for `font-lock-warning-face' in
;;    Emacs.  In XEmacs there's no corresponding standard face, so
;;    there it's defined as a face that stands out sharply.
;;
;;    This face is not used for the #error directive, since that's not
;;    a syntactic error in itself.  A parallel can be drawn to other
;;    error raising facilities, such as throw, which don't use this
;;    face either.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-langs)
(cc-require 'cc-vars)

(require 'font-lock)


;;; Common things between the languages.

(defvar c-doc-face
  (cond ((facep 'font-lock-doc-string-face)
	 ;; XEmacs.
	 'font-lock-doc-string-face)
	((facep 'font-lock-doc-face)
	 ;; Emacs 21 and later.
	 'font-lock-doc-face)
	(t
	 'font-lock-string-face))
  "Face name used for preprocessor directives.")

(defvar c-preprocessor-face
  (cond ((facep 'font-lock-preprocessor-face)
	 ;; XEmacs has a font-lock-preprocessor-face.
	 'font-lock-preprocessor-face)
	((facep 'font-lock-builtin-face)
	 ;; In Emacs 20 and later font-lock-builtin-face has
	 ;; traditionally been used for preprocessor directives.
	 'font-lock-builtin-face)
	(t
	 'font-lock-reference-face))
  "Face name used for preprocessor directives.")

(defvar c-invalid-face
  (if (facep 'font-lock-warning-face)
      ;; Emacs 20 and later has a font-lock-warning-face.
      'font-lock-warning-face
    ;; Otherwise we provide a face.
    'c-invalid-face)
  "Face name used for invalid syntax.")

(unless (facep c-invalid-face)
  (defface c-invalid-face
    '((((class color) (background light)) (:foreground "red"))
      (((class color)) (:foreground "hotpink"))
      (((background light)) (:foreground "white" :background "black"))
      (t (:foreground "black" :background "white")))
    "Face used to highlight invalid syntax."
    :group 'c-fonts))

(defun c-font-lock-syntactic-face-function (state)
  (save-excursion
    (if (nth 3 state)
        ;; Check whether the string is properly terminated.
	(let ((nstate (parse-partial-sexp (point) (c-point 'eol)
                                          nil nil state 'syntax-table)))
	  (if (and (eolp)
		   (nth 3 nstate)
		   ;; We're at eol inside a string.
		   (if (c-major-mode-is '(c-mode c++-mode objc-mode pike-mode))
		       ;; There's no \ before the newline.
		       (not (nth 5 nstate))
		     ;; Quoted newlines aren't supported.
		     t)
		   (if (c-major-mode-is 'pike-mode)
		       ;; There's no # before the string, so newlines
		       ;; aren't allowed.
		       (not (eq (char-before (nth 8 state)) ?#))
		     t))
              ;; We're inside a string, at EOL and there was no \.
	      c-invalid-face
            font-lock-string-face))
      (goto-char (nth 8 state))
      (if (looking-at c-doc-comment-start-regexp)
	  c-doc-face
	font-lock-comment-face))))

(c-lang-defconst c-cpp-keywords
  ;; Font lock matchers for preprocessor stuff.
  (c c++ pike)
  `(;; The stuff after #error and #warning is a message, so fontify it
    ;; as a string.
    (,(concat (c-lang-var c-cpp-prefix)
	      "\\(error\\|warning\\)\\>\\s *\\(.*\\)$")
     2 font-lock-string-face)

    ;; Fontify filenames in #include <...> as strings.
    (,(concat (c-lang-var c-cpp-prefix)
	      "\\(import\\|include\\)\\>\\s *\\(<[^>\n\r]*>?\\)")
     2 font-lock-string-face)

    ;; #define.
    (,(concat
       (c-lang-var c-cpp-prefix)
       "define\\s +"
       (concat "\\("			; 1
	       ;; Macro with arguments - a "function".
	       "\\(" (c-lang-var c-symbol-key) "\\)\(" ; 2
	       "\\|"
	       ;; Macro without arguments - a "variable".
	       "\\(" (c-lang-var c-symbol-key) ; 3 + c-symbol-key-depth
	       "\\)\\(\\s \\|$\\)"
	       "\\)"))
     (2 font-lock-function-name-face nil t)
     (,(+ 3 (c-lang-var c-symbol-key-depth))
      font-lock-variable-name-face nil t))

    ;; Fontify symbol names in #elif or #if ... defined preprocessor
    ;; directives.
    (,(concat (c-lang-var c-cpp-prefix) "\\(if\\|elif\\)\\>")
     (,(concat "\\<\\("			; 1
	       ;; Don't use regexp-opt here to avoid the depth hazzle.
	       ;; As it happens, it wouldn't currently have any effect
	       ;; anyway.
	       (mapconcat 'regexp-quote
			  (c-lang-var c-cpp-defined-fns) "\\|")
	       "\\)\\>"
	       "\\s *\(?\\(" (c-lang-var c-symbol-key) "\\)?") ; 2
      nil nil
      (1 ,c-preprocessor-face)
      (2 font-lock-variable-name-face nil t)))

    ;; Fontify symbols after #ifdef and #ifndef.
    (,(concat (c-lang-var c-cpp-prefix)
	      "ifn?def\\s +\\(" (c-lang-var c-symbol-key) "\\)")
     1 font-lock-variable-name-face)

    ;; Fontify the directive names.
    (,(concat (c-lang-var c-cpp-prefix) "[a-z]+")
     0 ,c-preprocessor-face)
    ))

(defvar c-primitive-types-depth nil)
;; `c-get-primitive-types' sets this to the parenthesis count of the
;; regexp it returns.

(defun c-get-primitive-types ()
  ;; Returns the union of `c-primitive-type-kwds' and the appropriate
  ;; *-font-lock-extra-types variable.
  (let ((re (concat c-primitive-type-key
		    "\\|"
		    "\\<\\("
		    (mapconcat 'identity
			       (cond ((c-major-mode-is 'c-mode)
				      c-font-lock-extra-types)
				     ((c-major-mode-is 'c++-mode)
				      c++-font-lock-extra-types)
				     ((c-major-mode-is 'objc-mode)
				      objc-font-lock-extra-types)
				     ((c-major-mode-is 'java-mode)
				      java-font-lock-extra-types))
			       "\\|")
		    "\\)\\([^_a-zA-Z0-9$]\\|$\\)")))
    (setq c-primitive-types-depth (c-regexp-opt-depth re))
    re))

(c-lang-defconst c-basic-keywords
  ;; Font lock matchers for basic type and keyword fontification.

  all `(;; Fontify all type names.
	(eval . (cons (c-get-primitive-types) 'font-lock-type-face))

	;; Fontify all keywords except the primitive types.
	,(c-make-keywords-re t
	   (set-difference (c-lang-var c-keywords)
			   (c-lang-var c-primitive-type-kwds)
			   :test 'string-equal)))

  c (append (c-lang-var c-basic-keywords)
	    `(;; Fontify goto targets, and case/goto labels.
	      ("\\<\\(case\\|goto\\)\\>"
	       ("\\(-[0-9]+\\|\\sw+\\)"
		;; Return limit of search.
		(save-excursion (skip-chars-forward "^:\n") (point))
		nil
		(1 font-lock-reference-face nil t)))
	      ;; Anders Lindgren <andersl@andersl.com> points out that
	      ;; it is quicker to use MATCH-ANCHORED to effectively
	      ;; anchor the regexp on the left.  This must come after
	      ;; the one for keywords and targets.  Note: the lack of
	      ;; `:' in the first char-range prevents `bar' from being
	      ;; highlighted in "foo: bar:".  But adding `:' would
	      ;; break cases like "test1 ? test2 ? foo : bar : baz".
	      (":" ("\\(?:^\\|[{};]\\)[ \t]*\\(\\sw+\\)[ \t]*:"
		    (beginning-of-line) (end-of-line)
		    (1 font-lock-reference-face))))))


;;; C.

(defconst c-font-lock-keywords-1
  (c-lang-var c-cpp-keywords c)
  "Subdued level highlighting for C mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c-font-lock-keywords-2
  (append c-font-lock-keywords-1
	  (c-lang-var c-basic-keywords c))
  "Medium level highlighting for C mode.
In addition to `c-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c-font-lock-extra-types'.")

(defconst c-font-lock-keywords-3
  c-font-lock-keywords-2
  "Gaudy level highlighting for C mode.
In addition to `c-font-lock-keywords-2', this adds fontification of
complex and user defined types and declarations that are hairy to
recognize.")

(defvar c-font-lock-keywords c-font-lock-keywords-3
  "Default expressions to highlight in C mode.
See also `c-font-lock-extra-types'.")


;;; C++.

(defconst c++-font-lock-keywords-1
  (c-lang-var c-cpp-keywords c++)
  "Subdued level highlighting for C++ mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c++-font-lock-keywords-2
  (append c++-font-lock-keywords-1
	  (c-lang-var c-basic-keywords c++))
  "Medium level highlighting for C++ mode.
In addition to `c++-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c++-font-lock-extra-types'.")

(defconst c++-font-lock-keywords-3
  c++-font-lock-keywords-2
  "Gaudy level highlighting for C++ mode.
In addition to `c++-font-lock-keywords-2', this adds fontification of
complex and user defined types and declarations that are hairy to
recognize.")

(defvar c++-font-lock-keywords c++-font-lock-keywords-3
  "Default expressions to highlight in C++ mode.
See also `c++-font-lock-extra-types'.")


;;; Objective-C.

(defconst objc-font-lock-keywords-1
  nil
  "Subdued level highlighting for ObjC mode.
Fontifies only compiler directives (in addition to the syntactic
fontification of strings and comments).")

(defconst objc-font-lock-keywords-2
  (append objc-font-lock-keywords-1
	  (c-lang-var c-basic-keywords objc))
  "Medium level highlighting for ObjC mode.
In addition to `objc-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `objc-font-lock-extra-types'.")

(defconst objc-font-lock-keywords-3
  objc-font-lock-keywords-2
  "Gaudy level highlighting for ObjC mode.
In addition to `objc-font-lock-keywords-2', this adds fontification of
complex types and declarations that are hairy to recognize.")

(defvar objc-font-lock-keywords objc-font-lock-keywords-3
  "Default expressions to highlight in ObjC mode.
See also `objc-font-lock-extra-types'.")


;;; Java.

(defconst java-font-lock-keywords-1
  nil
  "Subdued level highlighting for Java mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst java-font-lock-keywords-2
  (append java-font-lock-keywords-1
	  (c-lang-var c-basic-keywords java))
  "Medium level highlighting for Java mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `java-font-lock-extra-types'.")

(defconst java-font-lock-keywords-3
  java-font-lock-keywords-2
  "Gaudy level highlighting for Java mode.
In addition to `java-font-lock-keywords-2', this adds fontification of
complex and user defined types, Javadoc comments, and declarations
that are hairy to recognize.")

(defvar java-font-lock-keywords java-font-lock-keywords-3
  "Default expressions to highlight in Java mode.
See also `java-font-lock-extra-types'.")


;;; IDL.

;; Well..


;;; Pike.

(defconst pike-font-lock-keywords-1
  (c-lang-var c-cpp-keywords pike)
  "Subdued level highlighting for Pike mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst pike-font-lock-keywords-2
  (append pike-font-lock-keywords-1
	  (c-lang-var c-cpp-keywords pike))
  "Medium level highlighting for Pike mode.
In addition to `pike-font-lock-keywords-1', this adds fontification of
keywords, simple types and declarations that are easy to recognize.")

(defconst pike-font-lock-keywords-3
  pike-font-lock-keywords-2
  "Gaudy level highlighting for Pike mode.
In addition to `pike-font-lock-keywords-2', this adds fontification of
complex and user defined types and declarations that are hairy to
recognize.")

(defconst pike-font-lock-keywords-4
  pike-font-lock-keywords-3
  "Extra gaudy level highlighting for Pike mode.
In addition to `pike-font-lock-keywords-3', this adds fontification of
refdoc comments and the markup inside them.")

(defvar pike-font-lock-keywords pike-font-lock-keywords-4
  "Default expressions to highlight in Pike mode.
See also `pike-font-lock-extra-types'.")


(cc-provide 'cc-fonts)

;;; cc-fonts.el ends here
