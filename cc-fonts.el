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
;; font-lock-reference-face	Name qualifiers.
;;
;; Face aliases, mapped to different faces depending on (X)Emacs flavor:
;;
;; c-doc-face			Documentation comments (like Javadoc).
;; c-label-face			Label identifiers.
;; c-preprocessor-face		Preprocessor directives.
;; c-invalid-face		Invalid syntax.
;;
;; Special faces:
;;
;; c-doc-markup-face		Special markup in doc comments.
;;				Overlaid over `c-doc-face'.
;;
;; Some comments on the use of faces:
;;
;; o  `c-doc-face' is an alias for `font-lock-doc-string-face' in
;;    XEmacs, `font-lock-doc-face' in Emacs 21 and later, or
;;    `font-lock-string-face' in older Emacs.  FIXME: Doc comment
;;    fontification currently only works with font-lock packages that
;;    have `font-lock-syntactic-face-function', i.e Emacs 21 and
;;    later.
;;
;; o  `c-label-face' is an alias for either `font-lock-constant-face'
;;    (in Emacs 20 and later), or `font-lock-reference-face'
;;    otherwise.
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
(cc-require 'cc-engine)

;; Avoid repeated loading through the eval-after-load directive in
;; cc-mode.el.
(provide 'cc-fonts)

(require 'font-lock)


;; Note that XEmacs doesn't expand face names as variables, so we have
;; to use the (eval . FORM) in the font lock matchers wherever we use
;; these alias variables.

(defvar c-doc-face
  (cond ((c-face-name-p 'font-lock-doc-string-face)
	 ;; XEmacs.
	 'font-lock-doc-string-face)
	((c-face-name-p 'font-lock-doc-face)
	 ;; Emacs 21 and later.
	 'font-lock-doc-face)
	(t
	 'font-lock-string-face))
  "Face name used for preprocessor directives.")

(defvar c-preprocessor-face
  (cond ((c-face-name-p 'font-lock-preprocessor-face)
	 ;; XEmacs has a font-lock-preprocessor-face.
	 'font-lock-preprocessor-face)
	((c-face-name-p 'font-lock-builtin-face)
	 ;; In Emacs 20 and later font-lock-builtin-face has
	 ;; traditionally been used for preprocessor directives.
	 'font-lock-builtin-face)
	(t
	 'font-lock-reference-face))
  "Face name used for preprocessor directives.")

(cc-bytecomp-defvar font-lock-constant-face)

(defvar c-label-face
  (cond ((and (c-face-name-p 'font-lock-constant-face)
	      (eq font-lock-constant-face 'font-lock-constant-face))
	 ;; Test both if font-lock-constant-face exists and that it's
	 ;; not an alias for something else.  This is important since
	 ;; `c-font-lock-find-label' compares already set faces.
	 'font-lock-constant-face)
	(t
	 'font-lock-reference-face)))

(defvar c-invalid-face
  (if (c-face-name-p 'font-lock-warning-face)
      ;; Emacs 20 and later has a font-lock-warning-face.
      'font-lock-warning-face
    ;; Otherwise we provide a face.
    'c-invalid-face)
  "Face name used for invalid syntax.")

(unless (c-face-name-p c-invalid-face)
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
		   (elt nstate 3)
		   ;; We're at eol inside a string.
		   (if (c-major-mode-is '(c-mode c++-mode objc-mode pike-mode))
		       ;; There's no \ before the newline.
		       (not (elt nstate 5))
		     ;; Quoted newlines aren't supported.
		     t)
		   (if (c-major-mode-is 'pike-mode)
		       ;; There's no # before the string, so newlines
		       ;; aren't allowed.
		       (not (eq (char-before (elt state 8)) ?#))
		     t))
              ;; We're inside a string, at EOL and there was no \.
	      c-invalid-face
            font-lock-string-face))
      (goto-char (elt state 8))
      (if (looking-at c-doc-comment-start-regexp)
	  c-doc-face
	font-lock-comment-face))))

(c-lang-defconst c-cpp-matchers
  ;; Font lock matchers for preprocessor stuff.
  all
  (when (c-lang-var c-cpp-prefix)
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
      (eval . (list
	       ,(concat (c-lang-var c-cpp-prefix) "\\(if\\|elif\\)\\>")
	       (list
		,(concat "\\<\\("	; 1
			 ;; Don't use regexp-opt here to avoid the
			 ;; depth hazzle.  As it happens, it currently
			 ;; wouldn't have any effect anyway.
			 (mapconcat 'regexp-quote
				    (c-lang-var c-cpp-defined-fns) "\\|")
			 "\\)\\>"
			 "\\s *\(?\\(" (c-lang-var c-symbol-key) "\\)?") ; 2
		nil nil
		(list 1 c-preprocessor-face)
		'(2 font-lock-variable-name-face nil t))))

      ;; Fontify symbols after #ifdef and #ifndef.
      (,(concat (c-lang-var c-cpp-prefix)
		"ifn?def\\s +\\(" (c-lang-var c-symbol-key) "\\)")
       1 font-lock-variable-name-face)

      ;; Fontify the directive names.
      (eval . (list
	       ,(concat (c-lang-var c-cpp-prefix) "[a-z]+")
	       0 c-preprocessor-face))
      )))

(defun c-font-lock-labels (limit)
  ;; Fontify all the declarations from the point to LIMIT.  Assumes
  ;; that strings and comments have been fontified already.  This
  ;; function does both the search and the fontification itself to
  ;; avoid differences between the (X)Emacs flavors in where they
  ;; continue the search after a match.

  (while (re-search-forward ":[^:]" limit t)

    (if (get-text-property (match-beginning 0) 'face)
	;; Ignore hits inside highlighted stuff.
	(goto-char (next-single-property-change (point) 'face nil limit))

      (save-excursion
	(let (id-start)
	  (goto-char (match-beginning 0))
	  (c-backward-syntactic-ws)
	  (and (setq id-start (c-on-identifier))

	       (progn
		 (goto-char id-start)
		 (c-backward-syntactic-ws)
		 (or
		  ;; Check for a char that precedes a statement.
		  (memq (char-before) '(?\} ?\{ ?\) ?\;))
		  ;; Check for a preceding label.  We exploit the font
		  ;; locking made earlier by this function.
		  (and (eq (char-before) ?:)
		       (progn
			 (backward-char)
			 (c-backward-syntactic-ws)
			 (not (bobp)))
		       (eq (get-text-property (1- (point)) 'face)
			   c-label-face))
		  ;; Check for a keyword that precedes a statement.
		  (c-after-conditional)))

	       (progn
		 ;; Got a label.
		 (goto-char id-start)
		 (looking-at c-symbol-key)
		 (put-text-property (match-beginning 1) (match-end 1)
				    'face c-label-face))))))))

;; The compiler might complain about this variable when the lambda
;; below is byte compiled.
(cc-bytecomp-defvar c-label-face)

(c-lang-defconst c-basic-matchers
  ;; Font lock matchers for basic keywords, labels, references and
  ;; various other easily recognizable things.

  all `(;; Fontify all keywords except the primitive types.
	(,(concat "\\<" (c-lang-var c-nontype-keywords-regexp))
	 1 font-lock-keyword-face)

	;; Fontify labels in languages that supports them.
	,@(when (c-lang-var c-label-key)
	    `(;; Fontify goto targets and case labels.
	      (eval . (list
		       ,(concat "\\<\\("
				(c-make-keywords-re nil
				  (c-lang-var c-before-label-kwds))
				"\\)\\>[^:\n\r]*")
		       (list
			;; Fontify all integers and symbols between
			;; (match-end 1) and (match-end 0) as labels.
			;; (This is possible to do with a normal
			;; anchored matcher in Emacs >= 20 and XEmacs
			;; >= 21 where the PRE-MATCH-FORM can override
			;; the limit.)
			,(byte-compile
			  `(lambda (limit)
			     (setq limit (match-end 0))
			     (goto-char (match-end 1))
			     (while (re-search-forward
				     ,(concat "\\(-?[0-9]+\\)\\|\\("
					      (c-lang-var c-symbol-key)
					      "\\)")
				     limit t)
			       (if (match-beginning 1)
				   (put-text-property (match-beginning 1)
						      (match-end 1)
						      'face c-label-face)
				 (put-text-property (match-beginning 3)
						    (match-end 3)
						    'face c-label-face))))))))

	      ;; Fontify normal labels.
	      c-font-lock-labels))

	;; Fontify leading identifiers in fully qualified names like
	;; "foo::bar" in languages that supports such things.
	,@(when (c-lang-var c-identifier-concat-key)
	    `((,(byte-compile
		 ;; Must use a function here since we match longer
		 ;; than we want to move before doing a new search.
		 ;; This is not necessary for XEmacs >= 20 since it
		 ;; restarts the search from the end of the first
		 ;; highlighted submatch (something that causes
		 ;; problems in other places).
		 `(lambda (limit)
		    (when (re-search-forward
			   ,(concat "\\(\\<"
				    (c-lang-var c-symbol-key)
				    "[ \t\n\r]*"
				    (c-lang-var c-identifier-concat-key)
				    "\\)"
				    "[ \t\n\r]*"
				    (c-lang-var c-symbol-start))
			  limit t)
		      (goto-char (match-end 1)))))
	       2 font-lock-reference-face)))
	))

(defun c-font-lock-declarators (limit list)
  ;; Assuming the point is at the first declarator in a declaration,
  ;; fontify it.  If LIST is non-nil, fontify also all following
  ;; declarators in a comma separated list (e.g.  "foo" and "bar" in
  ;; "int foo = 17, bar;").  Stop at LIMIT.

  ;;(message "c-font-lock-declarators from %s to %s" (point) limit)
  (let ((id-start (point)) id-end id-face got-init)
    (while (and
	    id-start
	    (< (point) limit)
	    (if (eq (char-after) ?\()
		;; Allow a parenthesized declaration expression in the
		;; first sexp of the declarator, to cope with
		;; e.g. "char (*foo)[]" (below we treat "(" as the
		;; start of the initializer/function prototype).
		;; Don't search past the end of the line to avoid
		;; running away very far in incorrect code.
		(= (car (parse-partial-sexp (point) (c-point 'eol) 0)) 0)
	      t)
	    ;; Search syntactically to the end of the identifier
	    ;; declarator (";", ",", a close paren, eob etc) or to the
	    ;; beginning of an initializer or function prototype ("="
	    ;; or "\\s\(").
	    (c-syntactic-re-search-forward
	     "\\(\\)\\([;,\{\[]\\|\\s\)\\|\\'\\|\\(=\\|\\(\\s\(\\)\\)\\)"
	     limit t 1 t))

      (setq id-end (match-beginning 0)
	    id-face (if (match-beginning 4)
			'font-lock-function-name-face
		      'font-lock-variable-name-face)
	    got-init (match-beginning 3))

      ;; Fontify the individual identifiers within the identifier
      ;; expression (we could have several, e.g. things like
      ;; "foo<bar>::gnu" in C++).
      (goto-char id-start)
      (while (c-syntactic-re-search-forward c-symbol-key id-end 'move)
	(unless (text-property-not-all (match-beginning 1) (match-end 1)
				       'face nil)
	  (put-text-property (match-beginning 1) (match-end 1)
			     'face id-face)))

      (cond ((eq id-face 'font-lock-function-name-face)
	     ;; Skip a parenthesized initializer (C++) or a function
	     ;; prototype.
	     (if (c-safe (c-forward-sexp 1) t)
		 (c-forward-syntactic-ws limit)
	       (goto-char limit)))

	    (got-init
	     ;; Skip an initializer expression.
	     (if (c-syntactic-re-search-forward "[;,]" limit 'move 1 t)
		 (backward-char)))

	    (t (c-forward-syntactic-ws limit)))

      ;; Check for a comma if LIST is set.  If one is found we set
      ;; id-start to the next declarator and iterate.
      (if (and list (< (point) limit) (looking-at ","))
	  (progn
	    (forward-char)
	    (c-forward-syntactic-ws limit)
	    (setq id-start (point)))
	(setq id-start nil)))))

(defun c-font-lock-type (start end)
  ;; Fontify the individual identifiers within the given region with
  ;; the type face.  They're taken as all symbols that aren't
  ;; fontified as keywords or references.  Note that we want to
  ;; override function or variable name faces here, since those might
  ;; be put on by an earlier `c-font-lock-declarators' call which got
  ;; confused by a comma.  Point is left at the end of the region on
  ;; return.

  ;;(message "c-font-lock-type from %s to %s" start end)
  (goto-char start)
  (while (c-syntactic-re-search-forward c-symbol-key end 'move)
    (unless (get-text-property (match-beginning 1) 'face)
      (put-text-property (match-beginning 1) (match-end 1)
			 'face 'font-lock-type-face))))

(defun c-font-lock-declarations (limit)
  ;; Fontify all the declarations from the point to LIMIT.  Assumes
  ;; that strings, comments and nontype keywords have been fontified
  ;; already.  This function does both the search and the
  ;; fontification itself, since it's cumbersome to generate the
  ;; proper sequence of match limits that font-lock normally acts on.

  ;;(message "c-font-lock-declarations search from %s to %s" (point) limit)

  ;; Clear the list of found types if we start from the start of the
  ;; buffer, to make it easier to get rid of misspelled types and
  ;; variables that has gotten recognized as types in malformed code.
  (when (= (point) (point-min))
    (c-clear-found-types))

  (save-restriction
    ;; Narrow to the limit to deliberately fail to fontify
    ;; declarations that crosses it.  E.g. the following is a common
    ;; situation while the first line is being written:
    ;;
    ;;     my_variable
    ;;     some_other_variable = 0;
    ;;
    ;; font-lock will put the limit at the end of the first line here,
    ;; and we use that to avoid recognizing my_variable as a type in a
    ;; declaration that spans to the next line.  This way the
    ;; statement isn't annoyingly flashed as a type while it's being
    ;; entered.
    (narrow-to-region (point-min) limit)

    ;; Must back up a bit since we look for the end of the previous
    ;; statement or declaration, which is earlier than the first
    ;; returned match.
    (when (memq (get-text-property (point) 'face)
		'(font-lock-comment-face font-lock-string-face))
      ;; But first we need to move to a syntactically relevant
      ;; position.
      (goto-char (previous-single-property-change (min (1+ (point)) limit)
						  'face nil (point-min))))
    (c-backward-syntactic-ws)
    (c-safe (backward-char))

    (let ((match (or (bobp)
		     ;; We always consider bob as a match to get the
		     ;; first declaration in the file, but we have to
		     ;; step past initial comments to avoid that the
		     ;; match-inside-comment-or-string-literal code
		     ;; kicks in.
		     (progn (c-forward-comments)
			    t)
		   (re-search-forward c-decl-prefix-re limit 'move)))
	  continue-pos
	  ;; Nonzero if `c-decl-prefix-re' matches inside a function
	  ;; arglist, i.e. if it matches '(' or ','.
	  arglist-match
	  ;; Set to the result of `c-forward-type'.
	  at-type
	  ;; These record the start and end of the type or possible
	  ;; type found by `c-forward-type'.
	  type-start type-end
	  ;; These store `at-type', `type-start' and `type-end' of the
	  ;; identifier before the one in those variables.  The
	  ;; previous identifier might turn out to be the real type in
	  ;; a declaration if the last one has to be the declarator in
	  ;; it.  If `prev-at-type' is nil then the other variables
	  ;; have undefined values.
	  prev-at-type prev-type-start prev-type-end
	  ;; Whether we've found a declaration.  We might know this
	  ;; before we've found the type in it.
	  at-decl)

      (while (progn
	       (while (and
		       match

		       (or
			;; Continue if the match is within a comment or a
			;; string literal.
			(when (memq (get-text-property (point) 'face)
				    '(font-lock-comment-face
				      font-lock-string-face))
			  ;; Go to the end of the highlighted region
			  ;; before continuing the search above.
			  (goto-char (next-single-property-change
				      (point) 'face nil limit))
			  t)

			;; Continue if the following token isn't some
			;; kind of symbol or keyword that can't start
			;; a declaration, or if it's fontified as
			;; something else besides a type already.
			(let (prop)
			  (setq arglist-match (memq (char-before) '(?\( ?,)))
			  (c-forward-comments)
			  (setq continue-pos (point))
			  (c-forward-syntactic-ws)
			  (when (or
				 (= (point) limit)
				 (not (looking-at c-symbol-start))
				 (unless (memq (setq prop (get-text-property
							   (point) 'face))
					       '(nil font-lock-type-face))
				   (and
				    (eq prop 'font-lock-keyword-face)
				    (looking-at c-not-decl-init-keywords))))
			    (goto-char continue-pos)
			    t))
			))

		 ;; We only want to match at bob the first time, to
		 ;; avoid looping forever there.  That's not a problem
		 ;; anywhere else since `re-search-forward' will move
		 ;; forward at least one char (except at the limit).
		 (setq match (re-search-forward c-decl-prefix-re limit 'move)))

	       (< (point) limit))

	(catch 'continue
	  (setq at-type nil
		at-decl nil)

	  ;; Check for a type, but be prepared to skip over leading
	  ;; specifiers like "static".  We treat any symbols as
	  ;; specifiers here, to cope with macros like __INLINE__ or
	  ;; anything else that might be in front of declarations.
	  (while (let ((start (point))
		       (res (c-forward-type t)))
		   (cond ((eq res t)
			  ;; Found a positive type, so stop searching.
			  (setq at-type t
				type-start start
				type-end (point))
			  nil)
			 (res
			  ;; Found a possible type.  Record this, but
			  ;; continue so that we'll jump over any
			  ;; leading unknown specifiers.
			  (when at-type
			    ;; Got two identifiers with nothing but
			    ;; whitespace between them.  That can only
			    ;; happen in declarations.
			    (setq at-decl t))
			  (setq prev-at-type at-type
				prev-type-start type-start
				prev-type-end type-end
				at-type res
				type-start start
				type-end (point)))
			 ((looking-at c-specifier-key)
			  ;; Found a known specifier keyword.
			  (setq at-decl t)
			  (goto-char (match-end 1)))))
	    (c-forward-syntactic-ws))
	  (unless at-type
	    (throw 'continue t))

	  ;; Check for and step over a type decl expression after the
	  ;; thing that is or might be a type.  We can skip this if we
	  ;; know we're in a declaration and don't have several
	  ;; possible positions for the type.
	  (unless (and at-decl (or (eq at-type t) (not prev-at-type)))
	    (if (catch 'at-decl
		  (goto-char type-end)
		  (c-forward-syntactic-ws)
		  (let ((start (point)) (paren-depth 0)
			got-prefix no-identifier got-suffix)

		    ;; Skip over type decl prefix operators.
		    (while (looking-at c-type-decl-prefix-key)
		      (if (eq (char-after) ?\()
			  (progn
			    (unless got-prefix (setq got-prefix ?\())
			    (setq paren-depth (1+ paren-depth))
			    (forward-char))
			(setq got-prefix t)
			(goto-char (match-end 1)))
		      (c-forward-syntactic-ws))

		    ;; Skip over an identifier.
		    (if (looking-at c-symbol-key)
			(progn
			  (goto-char (match-end 1))
			  (c-forward-syntactic-ws)
			  (when (= paren-depth 0)
			    ;; We're not inside parentheses so it can't be a
			    ;; function call.
			    (throw 'at-decl t)))
		      ;; The identifier seems to be missing.
		      (setq no-identifier t))

		    ;; Skip over type decl suffix operators.
		    (while (and (looking-at c-type-decl-suffix-key)
				(if (eq (char-after) ?\))
				    (when (> paren-depth 0)
				      (setq paren-depth (1- paren-depth))
				      (forward-char)
				      t)
				  (setq got-suffix t)
				  (if (save-match-data (looking-at "\\s\("))
				      (unless (c-safe (c-forward-sexp 1) t)
					(throw 'at-decl nil))
				    (goto-char (match-end 1)))
				  t))
		      (c-forward-syntactic-ws))

		    (when (or (= (point) start) (> paren-depth 0))
		      ;; We haven't found anything or we're at a
		      ;; token that isn't valid in a type decl
		      ;; inside a parenthesis.
		      (throw 'at-decl nil))

		    (when no-identifier
		      (when (or (eq got-prefix t)
				(eq at-type t)
				(and (eq got-prefix ?\() got-suffix))
			;; Found no identifier.  If we got a prefix or if
			;; the type is positive, we know that the type
			;; really wasn't the identifier.  It's only in
			;; declarations in e.g. function prototypes that
			;; the identifier may be left out.
			(throw 'at-decl t))

		      (when (and got-suffix
				 (not got-prefix)
				 (not (eq at-type t))
				 prev-at-type)
			;; Got only a suffix and there are two
			;; identifiers before.  The second one is not
			;; the type afterall, so return nil to let the
			;; conditional below shift to the type in
			;; `prev-*'.
			(throw 'at-decl nil)))

		    (when (looking-at "[=\(]")
		      ;; There's an initializer after the type decl
		      ;; expression so we know it's a declaration.
		      ;; (Checking for `(' here normally has no effect
		      ;; since it's probably matched as a suffix.
		      ;; That's often not a problem, however.)
		      (throw 'at-decl t))

		    (unless (looking-at (if arglist-match "[,\)]" "[,;]"))
		      ;; If this is a declaration it should end here,
		      ;; so check for allowed separation tokens.
		      (throw 'at-decl nil))

		    ;; If we get here we can't tell if this is a type
		    ;; decl or a normal expression by looking at it
		    ;; alone.  (That's under the assumption that
		    ;; normal expressions always can look like type
		    ;; decl expressions, which isn't really true but
		    ;; the cases where it doesn't hold are so uncommon
		    ;; (e.g. some placements of "const" in C++) it's
		    ;; not worth the effort to look for them.)

		    ;; It's a type decl expression if we know we're in
		    ;; a declaration, or if the preceding identifier
		    ;; is a positive type.
		    (when (or at-decl (memq at-type '(t found)))
		      (throw 'at-decl t))

		    ;; If we had a complete symbol table here (which
		    ;; rules out `c-found-types') we should return t
		    ;; due to the disambiguation rule (in at least
		    ;; C++) that anything that can be parsed as a
		    ;; declaration is a declaration.  Now we're being
		    ;; more defensive and prefer to not highlight
		    ;; things like "foo (bar);" as a declaration.
		    nil))
		(setq at-decl t)

	      (when prev-at-type
		;; Didn't find a type decl expression, but if we've
		;; passed two consecutive identifiers it's still a
		;; declaration - we only went a bit too far.
		(goto-char type-end)
		(c-forward-syntactic-ws)
		(setq at-type prev-at-type
		      type-start prev-type-start
		      type-end prev-type-end))))

	  (if at-decl
	      ;; We're at a declaration.  Highlight the type and the
	      ;; following declarators.
	      (progn
		(c-add-complex-type type-start type-end)
		(c-font-lock-type type-start type-end)
		(c-forward-syntactic-ws)
		(c-font-lock-declarators limit (not arglist-match)))

	    ;; Not at a declaration, but if we know that we passed a
	    ;; type we should still highlight it.
	    (when (eq at-type t)
	      (c-add-complex-type type-start type-end)
	      (c-font-lock-type type-start type-end))))

	(goto-char continue-pos)

	;; We search here to allow the first match at bob.  See note above.
	(setq match (re-search-forward c-decl-prefix-re limit 'move))))
    nil))

;; Need to declare these during compilation since they're referenced
;; from the lambda generated by
;; `c-make-simple-font-lock-decl-function' which is defined during
;; compilation below.  They don't need to have the proper definitions,
;; though, since the generated lambdas aren't called during
;; compilation.
(cc-bytecomp-defun c-font-lock-type)
(cc-bytecomp-defun c-font-lock-declarators)

(eval-and-compile
  ;; We need the following function during compilation since it's
  ;; called when the `c-lang-defconst' initializers are evaluated.

  (defun c-make-simple-font-lock-decl-function (regexp type-submatch pre post)
    ;; This function makes a byte compiled function that searches for
    ;; REGEXP and fontifies each match as a declaration.  It works much
    ;; like an anchored font-lock matcher but cuts out a little bit of
    ;; the overhead.  However, the main reason is to pass the real
    ;; search limit to `c-font-lock-declarators' since most (if not all)
    ;; font-lock implementations arbitrarily limits anchored matchers to
    ;; the same line.
    ;;
    ;; TYPE-SUBMATCH is the submatch in REGEXP that surrounds the type.
    ;; PRE is run before calling `c-font-lock-declarators' and POST is
    ;; run afterwards.

    ;; Note: Replace `byte-compile' with `eval' to debug the generated
    ;; lambda easier.
    (byte-compile
     `(lambda (limit)
	(while (re-search-forward ,regexp limit t)
	  ,(when type-submatch
	     `(save-match-data
		(c-font-lock-type (match-beginning ,type-submatch)
				  (match-end ,type-submatch))))
	  ,pre
	  (save-match-data
	    (c-font-lock-declarators limit t))
	  ,post)
	nil))))

(c-lang-defconst c-simple-decl-matchers
  ;; Simple font lock matchers for types and declarations.  These are
  ;; used on level 2 only and so aren't combined with
  ;; `c-complex-decl-matchers'.

  all `(;; Fontify all type names and the identifiers in the
	;; declarations they might start.
	(eval . (list (c-make-simple-font-lock-decl-function
		       c-known-type-key
		       1
		       '(progn (goto-char (match-end 1))
			       (c-forward-syntactic-ws))
		       '(goto-char (match-end 1)))))

	;; Fontify types preceded by `c-type-prefix-kwds' and the
	;; identifiers in the declarations they might start.
	,@(when (c-lang-var c-type-prefix-kwds)
	    (let ((prefix-re (c-make-keywords-re nil
			       (c-lang-var c-type-prefix-kwds))))
	      `((,(c-make-simple-font-lock-decl-function
		   (concat "\\<\\(" prefix-re "\\)"
			   "[ \t\n\r]+"
			   (c-lang-var c-symbol-key))
		   (+ (c-regexp-opt-depth prefix-re) 2)
		   '(progn (goto-char (match-end 2))
			   (c-forward-syntactic-ws))
		   '(goto-char (match-end 2)))))))
	))

(c-lang-defconst c-complex-decl-matchers
  ;; Complex font lock matchers for types and declarations.

  all `(;; The first two rules here mostly find occurences that
	;; `c-font-lock-declarations' will find later anyway, but not
	;; declarations containing blocks in the type (see note
	;; below).  It's also useful to fontify these everywhere to
	;; show e.g. when a type keyword is accidentally used as an
	;; identifier.

	;; Fontify basic types.
	(,(concat "\\<\\("
		  (c-make-keywords-re nil (c-lang-var c-type-kwds))
		  "\\)\\>")
	 1 'font-lock-type-face)

	;; Fontify types preceded by `c-type-prefix-kwds'.
	,@(when (c-lang-var c-type-prefix-kwds)
	    (let ((prefix-re (c-make-keywords-re nil
			       (c-lang-var c-type-prefix-kwds))))
	      `((,(concat "\\<\\(" prefix-re "\\)"
			  "[ \t\n\r]+"
			  (c-lang-var c-symbol-key))
		 ,(+ (c-regexp-opt-depth prefix-re) 2) 'font-lock-type-face))))

	;; Fontify symbols after closing braces as declaration
	;; identifiers under the assumption that they are part of
	;; declarations like "class Foo { ... } foo;".  It's too
	;; expensive to check this accurately by skipping past the
	;; brace block, so we use the heuristic that it's such a
	;; declaration if the first identifier is on the same line as
	;; the closing brace.  `c-font-lock-declarations' will later
	;; override it if it turns out to be an new declaration, but
	;; it will be wrong if it's an expression (see the test
	;; decls-8.cc).
	,@(when (c-lang-var c-opt-block-decls-with-vars-key)
	    `((,(c-make-simple-font-lock-decl-function
		 (concat "}"
			 (c-lang-var c-single-line-syntactic-ws)
			 "\\("
			 (c-lang-var c-type-decl-prefix-key)
			 "\\|"
			 (c-lang-var c-symbol-key)
			 "\\)")
		 nil
		 `(goto-char
		   (match-beginning
		    ,(1+ (c-lang-var c-single-line-syntactic-ws-depth))))
		 nil))))

	;; Fontify all declarations.
	c-font-lock-declarations

	;; FIXME: Fontify types in casts.
	))

(c-lang-defconst c-matchers-1
  all (c-lang-var c-cpp-matchers))

(c-lang-defconst c-matchers-2
  all (append (c-lang-var c-matchers-1)
	      (c-lang-var c-basic-matchers)
	      (c-lang-var c-simple-decl-matchers)))

(c-lang-defconst c-matchers-3
  all (append (c-lang-var c-matchers-1)
	      (c-lang-var c-basic-matchers)
	      (c-lang-var c-complex-decl-matchers)))

(c-lang-defconst c-matchers-4
  all (c-lang-var c-matchers-3))


;;; C.

(defconst c-font-lock-keywords-1 (c-lang-var c-matchers-1 c)
  "Subdued level highlighting for C mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c-font-lock-keywords-2 (c-lang-var c-matchers-2 c)
  "Medium level highlighting for C mode.
In addition to `c-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c-font-lock-extra-types'.")

(defconst c-font-lock-keywords-3 (c-lang-var c-matchers-3 c)
  "Gaudy level highlighting for C mode.
Like `c-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `c-font-lock-extra-types'.")

(defvar c-font-lock-keywords c-font-lock-keywords-3
  "Default expressions to highlight in C mode.")


;;; C++.

(defconst c++-font-lock-keywords-1 (c-lang-var c-matchers-1 c++)
  "Subdued level highlighting for C++ mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c++-font-lock-keywords-2 (c-lang-var c-matchers-2 c++)
  "Medium level highlighting for C++ mode.
In addition to `c++-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c++-font-lock-extra-types'.")

(defconst c++-font-lock-keywords-3 (c-lang-var c-matchers-3 c++)
  "Gaudy level highlighting for C++ mode.
Like `c++-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `c++-font-lock-extra-types'.")

(defvar c++-font-lock-keywords c++-font-lock-keywords-3
  "Default expressions to highlight in C++ mode.")


;;; Objective-C.

(defconst objc-font-lock-keywords-1 (c-lang-var c-matchers-1 objc)
  "Subdued level highlighting for ObjC mode.
Fontifies only compiler directives (in addition to the syntactic
fontification of strings and comments).")

(defconst objc-font-lock-keywords-2 (c-lang-var c-matchers-2 objc)
  "Medium level highlighting for ObjC mode.
In addition to `objc-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `objc-font-lock-extra-types'.")

(defconst objc-font-lock-keywords-3 (c-lang-var c-matchers-3 objc)
  "Gaudy level highlighting for ObjC mode.
Like `objc-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `objc-font-lock-extra-types'.")

(defvar objc-font-lock-keywords objc-font-lock-keywords-3
  "Default expressions to highlight in ObjC mode.")


;;; Java.

(defconst java-font-lock-keywords-1 (c-lang-var c-matchers-1 java)
  "Subdued level highlighting for Java mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst java-font-lock-keywords-2 (c-lang-var c-matchers-2 java)
  "Medium level highlighting for Java mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `java-font-lock-extra-types'.")

(defconst java-font-lock-keywords-3 (c-lang-var c-matchers-3 java)
  "Gaudy level highlighting for Java mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar java-font-lock-keywords java-font-lock-keywords-3
  "Default expressions to highlight in Java mode.")


;;; IDL.

(defconst idl-font-lock-keywords-1 (c-lang-var c-matchers-1 idl)
  "Subdued level highlighting for IDL mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst idl-font-lock-keywords-2 (c-lang-var c-matchers-2 idl)
  "Medium level highlighting for IDL mode.
In addition to `idl-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `idl-font-lock-extra-types'.")

(defconst idl-font-lock-keywords-3 (c-lang-var c-matchers-3 idl)
  "Gaudy level highlighting for IDL mode.
Like `idl-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `idl-font-lock-extra-types'.")

(defvar idl-font-lock-keywords idl-font-lock-keywords-3
  "Default expressions to highlight in IDL mode.")


;;; Pike.

(defconst pike-font-lock-keywords-1 (c-lang-var c-matchers-1 pike)
  "Subdued level highlighting for Pike mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst pike-font-lock-keywords-2 (c-lang-var c-matchers-2 pike)
  "Medium level highlighting for Pike mode.
In addition to `pike-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `pike-font-lock-extra-types'.")

(defconst pike-font-lock-keywords-3 (c-lang-var c-matchers-3 pike)
  "Gaudy level highlighting for Pike mode.
Like `pike-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `pike-font-lock-extra-types'.")

(defconst pike-font-lock-keywords-4 (c-lang-var c-matchers-4 pike)
  "Extra gaudy level highlighting for Pike mode.
In addition to `pike-font-lock-keywords-3', this adds fontification of
refdoc comments and the markup inside them.")

(defvar pike-font-lock-keywords pike-font-lock-keywords-4
  "Default expressions to highlight in Pike mode.")


(cc-provide 'cc-fonts)

;;; cc-fonts.el ends here
