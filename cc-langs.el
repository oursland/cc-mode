;;; cc-langs.el --- language specific settings for CC Mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)

(require 'cl)


;; Helpers for building of constants that are parameterized on a
;; per-language basis.

(eval-and-compile
  (defvar c-macroexpand-mode nil
    ;; Dynamically set to the mode symbol during `c-lang-defconst' so
    ;; that `c-lang-var' can do the right expansion.
    )

  (defmacro c-lang-defconst (var &rest args)
    ;; Sets the mode specific values of the constant VAR.  The rest of
    ;; the arguments are one or more repetitions of MODE VAL.  MODE is
    ;; the mode name without the "-mode" suffix, or a list of such
    ;; mode names, or `all' as a shortcut for a list of all modes.
    ;; VAL is evaluated (during compilation) for each mode with
    ;; `c-macroexpand-mode' temporarily bound, so `c-lang-var' without
    ;; an explicit mode may be used within it.  The assignments in
    ;; ARGS are processed in sequence, similar to `setq'.
    (let* ((res (list 'progn))
	   (res-tail res))
      (while args
	(let ((mode (car args)))
	  (cond ((eq mode 'all)
		 (setq mode '(c c++ objc java idl pike)))
		((symbolp mode)
		 (setq mode (list mode))))
	  (while mode
	    (let* ((c-macroexpand-mode
		    (intern (concat (symbol-name (car mode)) "-mode")))
		   ;; Set `c-buffer-is-cc-mode' so that
		   ;; `c-major-mode-is' works in the macro expansion.
		   (c-buffer-is-cc-mode c-macroexpand-mode)
		   (val (eval (car (cdr args)))))
	      ;; Need to install the value also during compilation,
	      ;; since val might refer to earlier mode specific
	      ;; values.
	      (put var c-macroexpand-mode val)
	      (setcdr res-tail (list `(put ',var ',c-macroexpand-mode ',val)))
	      (setq res-tail (cdr res-tail)))
	    (setq mode (cdr mode))))
	(setq args (cdr (cdr args))))
      res))
  (put 'c-lang-defconst 'lisp-indent-function 1)

  (defmacro c-lang-var (var &optional mode)
    ;; Get the mode specific value of the variable VAR in mode MODE.
    ;; MODE is the mode name without the "-mode" suffix.  It may also
    ;; be nil to use the current value of `c-macroexpand-mode' (which
    ;; is useful inside `c-lang-defconst') or `c-buffer-is-cc-mode'
    ;; (which is useful inside `c-lang-defvar').
    `(get ',var ,(if (eq mode 'nil)
		     (if c-macroexpand-mode
			 ;; In the macro expansion of `c-lang-defconst'.
			 `(quote ,c-macroexpand-mode)
		       `c-buffer-is-cc-mode)
		   `(quote ,(intern (concat (symbol-name mode) "-mode"))))))

  ;; These are used to collect the init forms from the subsequent
  ;; `c-lang-defvar'.  They become a big setq in the
  ;; `c-init-lang-defvars' lambda below.
  (defconst c-lang-defvar-init-form (list 'setq))
  (defconst c-lang-defvar-init-form-tail c-lang-defvar-init-form)

  (defmacro c-lang-defvar (var val)
    ;; Declares the buffer local variable VAR to get the value VAL at
    ;; mode initialization, at which point VAL is evaluated.
    ;; `c-lang-var' is typically used in VAL to get the right value
    ;; according to `c-buffer-is-cc-mode'.
    (setcdr c-lang-defvar-init-form-tail (list var val))
    (setq c-lang-defvar-init-form-tail
	  (cdr (cdr c-lang-defvar-init-form-tail)))
    `(progn
       (defvar ,var nil)
       (make-variable-buffer-local ',var)))
  (put 'c-lang-defvar 'lisp-indent-function 1)
  )


;; Some support functions that are used when the language specific
;; constants are built.  Since the constants are built during compile
;; time, these need to be defined then too.

(eval-and-compile
  ;; `require' in XEmacs doesn't have the third NOERROR argument.
  (condition-case nil (require 'regexp-opt) (file-error nil))

  (if (fboundp 'regexp-opt)
      (fset 'c-regexp-opt (symbol-function 'regexp-opt))
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

  (if (fboundp 'regexp-opt-depth)
      (fset 'c-regexp-opt-depth (symbol-function 'regexp-opt-depth))
    ;; Emacs 19.34 doesn't have the regexp-opt package.
    (defun c-regexp-opt-depth (regexp)
      ;; This is the definition of `regexp-opt-depth' in Emacs 20.
      (save-match-data
	;; Hack to signal an error if REGEXP does not have balanced
	;; parentheses.
	(string-match regexp "")
	;; Count the number of open parentheses in REGEXP.
	(let ((count 0) start)
	  (while (string-match "\\\\(" regexp start)
	    (setq count (1+ count) start (match-end 0)))
	  count))))

  (defun c-make-keywords-re (adorn &rest lists)
    "Make a regexp that matches all the strings in all the lists.
Duplicates in the lists are removed.  The regexp may contain zero or
more submatch expressions.  If ADORN is non-nil there will be at least
one submatch which matches the whole keyword, and the regexp will also
not match a prefix of any identifier.  Adorned regexps cannot be
appended."
    (setq lists (delete-duplicates (apply 'append (nconc lists '(nil)))
				   :test 'string-equal))
    (if lists
	(let ((re (c-regexp-opt lists)))
	  ;; Add our own grouping parenthesis around re instead of
	  ;; passing adorn to regexp-opt, since it in XEmacs makes the
	  ;; top level grouping "shy".
	  (if adorn
	      (concat "\\(" re "\\)"
		      "\\(" (c-lang-var c-nonsymbol-key) "\\|$\\)")
	    re))
      "\\<\\>"				; Matches nothing.
      ))
  (put 'c-make-keywords-re 'lisp-indent-function 1)
  )


;; Constants and variables that are language dependent.

;; Regexp that matches any character that can't be part of a symbol.
;; It's usually appended to other regexps to avoid matching a prefix.
;; It's assumed to not contain any submatchers.  The same thing
;; regarding Unicode identifiers applies here as to `c-symbol-key'
;; below.
(c-lang-defconst c-nonsymbol-key
  all "[^_a-zA-Z0-9$]")

;; Regexp that matches the start of a symbol.
;;
;; This definition isn't correct for the first character in the
;; languages that accept the full range of Unicode word constituents
;; in identifiers (e.g. Java and Pike).  For that we'd need to make a
;; regexp that matches all characters in the word constituent class
;; except 0-9, and the regexp engine currently can't do that.
(c-lang-defconst c-symbol-start
   (c c++ objc java idl) "[_a-zA-Z]"
   pike "[_a-zA-Z`]")
(c-lang-defvar c-symbol-start (c-lang-var c-symbol-start))

(eval-when-compile
  ;; The operator identifiers in Pike.
  (defconst c-pike-operator-symbols
    '("`+" "`-" "`&" "`|" "`^" "`<<" "`>>" "`*" "`/" "`%" "`~"
      "`==" "`<" "`>" "`!" "`[]" "`[]=" "`->" "`->=" "`()" "``+"
      "``-" "``&" "``|" "``^" "``<<" "``>>" "``*" "``/" "``%"
      "`+=")))

;; Regexp matching an identifier, not excluding keywords.
;;
;; We cannot use just `word' syntax class since `_' cannot be in word
;; class.  Putting underscore in word class breaks forward word
;; movement behavior that users are familiar with.  Besides, it runs
;; counter to Emacs convention.
(c-lang-defconst c-symbol-key
  all "[_a-zA-Z]\\(\\w\\|\\s_\\)*"
  pike (concat (c-lang-var c-symbol-key) "\\|"
	       (c-make-keywords-re nil c-pike-operator-symbols)))
(c-lang-defvar c-symbol-key (c-lang-var c-symbol-key))

;; Number of regexp grouping parens in `c-symbol-key'.
(c-lang-defconst c-symbol-key-depth
  all (c-regexp-opt-depth (c-lang-var c-symbol-key)))

;; Regexp matching the operators that join symbols to fully qualified
;; identifiers, or nil in languages that doesn't have such things.
;; Does not contain a \| operator at the top level.
(c-lang-defconst c-identifier-concat-key
  c++  "::"
  java "\\."
  pike "\\(::\\|\\.\\)")

;; Regexp matching a fully qualified identifier, like "A::B::c" in
;; C++.  (We cheat a bit here and don't recognize the full range of
;; syntactic whitespace between the tokens.)
(c-lang-defconst c-qualified-identifier-key
  all (c-lang-var c-symbol-key)		; Default to `c-symbol-key'.
  ;; These languages allow a leading qualifier operator.
  (c++ pike) (concat
	      "\\(" (c-lang-var c-identifier-concat-key) "[ \t\n\r]*\\)?"
	      ;; The submatch below is depth of c-identifier-concat-key + 2.
	      "\\(" (c-lang-var c-symbol-key) "\\)"
	      (concat "\\("
		      "[ \t\n\r]*"
		      (c-lang-var c-identifier-concat-key)
		      "[ \t\n\r]*"
		      ;; The submatch below is: c-symbol-key-depth +
		      ;; 2 * depth of c-identifier-concat-key + 4.
		      "\\(" (c-lang-var c-symbol-key) "\\)"
		      "\\)*"))
  ;; Java does not allow a leading qualifier operator.
  java (concat "\\(" (c-lang-var c-symbol-key) "\\)" ; 1
	       (concat "\\("
		       "[ \t\n\r]*"
		       (c-lang-var c-identifier-concat-key)
		       "[ \t\n\r]*"
		       ;; The submatch below is c-symbol-key-depth +
		       ;; depth of c-identifier-concat-key + 3.
		       "\\(" (c-lang-var c-symbol-key) "\\)"
		       "\\)*")))
(c-lang-defvar c-qualified-identifier-key
  (c-lang-var c-qualified-identifier-key))

;; Used to identify the submatch in `c-qualified-identifier-key' that
;; surrounds the last symbol in the qualified identifier.  It's a list
;; of submatch numbers, of which the first that have a match is taken.
;; It's assumed that at least one have a match when the regexp have
;; matched.
(c-lang-defconst c-qualified-identifier-last-sym-match
  all '(0)
  (c++ pike) (list (+ (c-lang-var c-symbol-key-depth)
		      (* 2 (c-regexp-opt-depth
			    (c-lang-var c-identifier-concat-key)))
		      4)
		   (+ (c-regexp-opt-depth
		       (c-lang-var c-identifier-concat-key))
		      2))
  java (list (+ (c-lang-var c-symbol-key-depth)
		(c-regexp-opt-depth
		 (c-lang-var c-identifier-concat-key))
		3)
	     1))

;; Syntax table built on the mode syntax table but additionally
;; classifies '_' and '$' as word constituents, so that all
;; identifiers are recognized as words.
(c-lang-defvar c-identifier-syntax-table
  (let ((table (make-syntax-table (c-mode-var "mode-syntax-table"))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    table))

(defvar c-stmt-delim-chars "^;{}?:")
;; The characters that should be considered to bound statements.  To
;; optimize `c-crosses-statement-barrier-p' somewhat, it's assumed to
;; begin with "^" to negate the set.  If ? : operators should be
;; detected then the string must end with "?:".

(defvar c-stmt-delim-chars-with-comma "^;,{}?:")
;; Variant of `c-stmt-delim-chars' that additionally contains ','.

;; List of all tokens that are longer than one character and that are
;; made up of characters in the punctuation or parenthesis syntax
;; classes.
(c-lang-defconst c-multichar-tokens
  all '("++" "--" "<<" ">>" "<=" ">=" "==" "!=" "&&" "||"
	"*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|=")
  (c c++ pike) (append '("##"	; Used by cpp.
			 "->" "::" "...")
		       (c-lang-var c-multichar-tokens))
  (c c++) (append '("<:" ":>" "<%" "%>" "%:" "%:%:")
		  (c-lang-var c-multichar-tokens))
  c++  (append '(".*" "->*")
	       (c-lang-var c-multichar-tokens))
  java (append '(">>>" ">>>=")
	       (c-lang-var c-multichar-tokens))
  pike (append (c-lang-var c-multichar-tokens)
	       '("..")
	       c-pike-operator-symbols))

;; Regexp matching all operator tokens and also any sequence of two
;; characters inside a symbol.
(c-lang-defconst c-multichar-op-sym-token-regexp
  all (concat "\\(\\w\\|\\s_\\)\\(\\w\\|\\s_\\)\\|"
	      (c-make-keywords-re nil (c-lang-var c-multichar-tokens))))
(c-lang-defvar c-multichar-op-sym-token-regexp
  (c-lang-var c-multichar-op-sym-token-regexp))

;; HELPME: Many of the following keyword lists are more or less bogus
;; for some languages (notably ObjC and IDL).  The effects of the
;; erroneous values in the language handling are mostly negligible
;; since the constants that actually matter in the syntax detection
;; code are mostly correct in the situations they are used, but I'd
;; still appreciate help to get them correct for other uses.

;; Primitive type keywords, excluding those on `c-complex-type-kwds'.
(c-lang-defconst c-primitive-type-kwds
  (c c++ idl) '("char" "double" "float" "int" "long" "short" "signed"
		"unsigned" "void")
  c (append '("complex" "imaginary")	; Conditionally defined in C99.
	    (c-lang-var c-primitive-type-kwds))
  c++ (append '("bool" "wchar_t")
	      (c-lang-var c-primitive-type-kwds))
  objc '("char" "double" "float" "id" "int" "long" "short" "signed" "unsigned"
	 "void")
  java '("boolean" "byte" "char" "double" "float" "int" "long" "short" "void")
  pike '(;; this_program isn't really a keyword, but it's practically
	 ;; used as a builtin type.
	 "float" "mixed" "string" "this_program" "void"))

;; An adorned regexp that matches `c-primitive-type-kwds'.
(c-lang-defconst c-primitive-type-key
  all (c-make-keywords-re t (c-lang-var c-primitive-type-kwds)))
(c-lang-defvar c-primitive-type-key (c-lang-var c-primitive-type-key))

;; Keywords that might act as prefixes for primitive types.  Note that
;; this is assumed to be a subset of `c-primitive-type-kwds'.
(c-lang-defconst c-primitive-type-prefix-kwds
  (c c++) '("long" "short" "signed" "unsigned"))

;; An adorned regexp that matches `c-primitive-type-prefix-kwds', or
;; nil in languages without such things.
(c-lang-defconst c-primitive-type-prefix-key
  all (and (c-lang-var c-primitive-type-prefix-kwds)
	   (c-make-keywords-re t (c-lang-var c-primitive-type-prefix-kwds))))
(c-lang-defvar c-primitive-type-prefix-key
  (c-lang-var c-primitive-type-prefix-key))

;; Keywords that can precede a parenthesis that contains a complex
;; type, e.g. "mapping(int:string)" in Pike.
(c-lang-defconst c-complex-type-kwds
  pike '("array" "function" "int" "mapping" "multiset" "object" "program"))

;; An adorned regexp that matches `c-complex-type-kwds', or nil in
;; languages without such things.
(c-lang-defconst c-complex-type-key
  all (and (c-lang-var c-complex-type-kwds)
	   (c-make-keywords-re t (c-lang-var c-complex-type-kwds))))
(c-lang-defvar c-complex-type-key (c-lang-var c-complex-type-key))

;; All keywords that starts a type, i.e. the union of
;; `c-primitive-type-kwds' and `c-complex-type-kwds'.
(c-lang-defconst c-type-kwds
  all (if (c-lang-var c-complex-type-kwds)
	  ;; Don't need `delete-duplicates' since these two are
	  ;; defined to be exclusive.
	  (append (c-lang-var c-primitive-type-kwds)
		  (c-lang-var c-complex-type-kwds))
	(c-lang-var c-primitive-type-kwds)))

;; Declaration specifier keywords.  These are keywords that may
;; precede declarations but that aren't part of a type, e.g. "struct"
;; in C isn't a specifier since the whole "struct foo" is a type, but
;; "typedef" is since it precedes the declaration that defines the
;; type.
(c-lang-defconst c-specifier-kwds
  (c c++) '("auto" "const" "extern" "inline" "register" "typedef"
	    "static" "volatile")
  c   (append '("restrict")
	      (c-lang-var c-specifier-kwds))
  c++ (append '("explicit" "friend" "mutable" "virtual")
	      (c-lang-var c-specifier-kwds))
  ;; I have no idea about these languages, so just use the specifiers in C.
  (objc idl) (c-lang-var c-specifier-kwds c)
  ;; Note: "const" is not used in Java, but it's still a reserved keyword.
  java '("abstract" "const" "final" "native" "private" "protected"
	 "public" "static" "strictfp" "synchronized" "transient" "volatile")
  pike '("constant" "final" "inline" "local" "nomask" "optional"
	 "private" "protected" "public" "static" "typedef" "variant"))

;; Declaration specifier keywords.
(c-lang-defconst c-specifier-key
  all (c-make-keywords-re t (c-lang-var c-specifier-kwds)))
(c-lang-defvar c-specifier-key (c-lang-var c-specifier-key))

;; Class/struct declaration keywords.
(c-lang-defconst c-class-kwds
  c '("struct" "union")
  c++ '("class" "struct" "union")
  objc '("interface" "implementation")
  java '("class" "interface")
  idl '("class" "interface" "struct" "union" "valuetype")
  pike '("class"))

;; Regexp matching the start of a class.
(c-lang-defconst c-class-key
  all (c-make-keywords-re t (c-lang-var c-class-kwds)))
(c-lang-defconst c-class-key		; ObjC needs some tuning of the regexp.
   objc (concat "@" (c-lang-var c-class-key)))
(c-lang-defvar c-class-key (c-lang-var c-class-key))

;; Keywords introducing blocks besides classes that contain another
;; declaration level.
(c-lang-defconst c-other-decl-block-kwds
  c '("extern")
  c++ '("namespace" "extern")
  idl '("module"))

;; Regexp matching the start of blocks besides classes that contain
;; another declaration level.
(c-lang-defconst c-other-decl-block-key
  all (c-make-keywords-re t (c-lang-var c-other-decl-block-kwds)))
(c-lang-defvar c-other-decl-block-key (c-lang-var c-other-decl-block-key))

;; Keywords introducing declarations that can contain a block which
;; might be followed by variable declarations, e.g. like "foo" in
;; "class Foo { ... } foo;".  So if there is a block in a declaration
;; like that, it ends with the following ';' and not right away.
(c-lang-defconst c-block-decls-with-vars
  c '("struct" "union" "enum" "typedef")
  c++ '("class" "struct" "union" "enum" "typedef"))

;; Regexp matching the `c-block-decls-with-vars' keywords, or nil in
;; languages without such constructs.
(c-lang-defconst c-opt-block-decls-with-vars-key
  all (and (c-lang-var c-block-decls-with-vars)
	   (c-make-keywords-re t (c-lang-var c-block-decls-with-vars))))
(c-lang-defvar c-opt-block-decls-with-vars-key
  (c-lang-var c-opt-block-decls-with-vars-key))

;; Keywords where the following symbol - if any - is a type name.
(c-lang-defconst c-type-prefix-kwds
  c '("struct" "union" "enum")
  c++ '("class" "struct" "typename" "union" "enum")
  java '("class")
  pike '("class" "enum"))

;; Adorned regexp matching `c-type-prefix-kwds'.
(c-lang-defconst c-type-prefix-key
  all (c-make-keywords-re t (c-lang-var c-type-prefix-kwds)))
(c-lang-defvar c-type-prefix-key (c-lang-var c-type-prefix-key))

;; Keywords introducing declarations that has not been accounted for
;; by any of the above.
(c-lang-defconst c-other-decl-kwds
  ;; FIXME: Shouldn't "template" be moved to `c-specifier-kwds' for C++?
  c++ '("template" "using")
  java '("import" "package")
  pike '("import" "inherit"))

;; Keywords introducing extra declaration specifiers in the region
;; between the header and the body (i.e. the "K&R-region") in
;; declarations.
(c-lang-defconst c-decl-spec-kwds java '("extends" "implements" "throws"))

;; Protection label keywords in classes.
(c-lang-defconst c-protection-kwds
  (c++ objc) '("private" "protected" "public"))

;; Statement keywords followed directly by a substatement.
(c-lang-defconst c-block-stmt-1-kwds
  (c pike) '("do" "else")
  (c++ objc) '("do" "else" "asm" "try")
  java '("do" "else" "finally" "try"))

;; Regexp matching the start of any statement followed directly by a
;; substatement (doesn't match a bare block, however).
(c-lang-defconst c-block-stmt-1-key
  all (c-make-keywords-re t (c-lang-var c-block-stmt-1-kwds)))
(c-lang-defvar c-block-stmt-1-key (c-lang-var c-block-stmt-1-key))

;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  c '("for" "if" "switch" "while")
  (c++ objc) '("for" "if" "switch" "while" "catch")
  java '("for" "if" "switch" "while" "catch" "synchronized")
  pike '("for" "if" "switch" "while" "foreach"))

;; Regexp matching the start of any statement followed by a paren sexp
;; and then by a substatement.
(c-lang-defconst c-block-stmt-2-key
  all (c-make-keywords-re t (c-lang-var c-block-stmt-2-kwds)))
(c-lang-defvar c-block-stmt-2-key (c-lang-var c-block-stmt-2-key))

;; Regexp matching the start of any statement that has a substatement
;; (except a bare block).  Nil in languages that doesn't have such
;; constructs.
(c-lang-defconst c-opt-block-stmt-key
  all (if (or (c-lang-var c-block-stmt-1-kwds)
	      (c-lang-var c-block-stmt-2-kwds))
	  (c-make-keywords-re t
	    (c-lang-var c-block-stmt-1-kwds)
	    (c-lang-var c-block-stmt-2-kwds))))
(c-lang-defvar c-opt-block-stmt-key (c-lang-var c-opt-block-stmt-key))

;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  (c c++ objc) '("break" "continue" "goto" "return")
  ;; Note: `goto' is not valid in Java, but the keyword is still reserved.
  java '("break" "continue" "goto" "return" "throw")
  pike '("break" "continue" "return"))

;; Statement keywords followed by an assembler expression.
(c-lang-defconst c-asm-stmt-kwds
  (c c++) '("asm" "__asm__"))

;; Regexp matching the start of an assembler statement.  Nil in
;; languages that doesn't support that.
(c-lang-defconst c-opt-asm-stmt-key
  all (if (c-lang-var c-asm-stmt-kwds)
	  (c-make-keywords-re t (c-lang-var c-asm-stmt-kwds))))
(c-lang-defvar c-opt-asm-stmt-key (c-lang-var c-opt-asm-stmt-key))

;; Keywords introducing labels in blocks.
(c-lang-defconst c-label-kwds (c c++ java pike) '("case" "default"))

;; Keywords followed by a label or a label reference.
(c-lang-defconst c-before-label-kwds (c c++ java pike) '("case" "goto"))

;; Regexp matching any keyword that introduces a label.
(c-lang-defconst c-label-kwds-regexp
  all (c-make-keywords-re t (c-lang-var c-label-kwds)))
(c-lang-defvar c-label-kwds-regexp (c-lang-var c-label-kwds-regexp))

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  (c c++) '("NULL") ;; Not a keyword, but practically works as one.
  c++ (append '("false" "true")
	      (c-lang-var c-constant-kwds)))

;; Keywords that can occur anywhere in expressions.
(c-lang-defconst c-expr-kwds
  (c objc) '("sizeof")
  c++ '("delete" "new" "operator" "sizeof" "this" "throw"
	"const_cast" "dynamic_cast" "reinterpret_cast" "static_cast" "typeid"
	"and" "and_eq" "bitand" "bitor" "compl" "not"
	"not_eq" "or" "or_eq" "xor" "xor_eq")
  java '("instanceof" "new" "super" "this")
  pike '(;; "this" isn't really a keyword, but it works as one in practice.
	 "catch" "class" "gauge" "global" "lambda" "predef" "this" "throw"))

;; Keywords that start lambda constructs, i.e. function definitions in
;; expressions.
(c-lang-defconst c-lambda-kwds pike '("lambda"))

;; Regexp matching the start of lambda constructs, or nil in languages
;; that doesn't have such things.
(c-lang-defconst c-opt-lambda-key
  pike (c-make-keywords-re t (c-lang-var c-lambda-kwds)))
(c-lang-defvar c-opt-lambda-key (c-lang-var c-opt-lambda-key))

;; Keywords that start constructs followed by statement blocks which
;; can be used in expressions (the gcc extension for this in C and C++
;; is handled separately).
(c-lang-defconst c-inexpr-block-kwds pike '("catch" "gauge"))

;; Regexp matching the start of in-expression statements, or nil in
;; languages that doesn't have such things.
(c-lang-defconst c-opt-inexpr-block-key
  pike (c-make-keywords-re t (c-lang-var c-inexpr-block-kwds)))
(c-lang-defvar c-opt-inexpr-block-key (c-lang-var c-opt-inexpr-block-key))

;; Keywords that start classes in expressions.
(c-lang-defconst c-inexpr-class-kwds
  java '("new")
  pike '("class"))

;; Regexp matching the start of a class in an expression, or nil in
;; languages that doesn't have such things.
(c-lang-defconst c-opt-inexpr-class-key
  (java pike) (c-make-keywords-re t (c-lang-var c-inexpr-class-kwds)))
(c-lang-defvar c-opt-inexpr-class-key (c-lang-var c-opt-inexpr-class-key))

;; Regexp matching the start of any class, both at top level and in
;; expressions.
(c-lang-defconst c-any-class-key
  all (c-make-keywords-re t
	(c-lang-var c-class-kwds)
	(c-lang-var c-inexpr-class-kwds)))
(c-lang-defconst c-any-class-key	; ObjC needs some tuning of the regexp.
  objc (concat "@" (c-lang-var c-any-class-key)))
(c-lang-defvar c-any-class-key (c-lang-var c-any-class-key))

;; Regexp matching the start of any declaration-level block that
;; contain another declaration level, i.e. that isn't a function
;; block.
(c-lang-defconst c-decl-block-key
  all (c-make-keywords-re t
	(c-lang-var c-class-kwds)
	(c-lang-var c-other-decl-block-kwds)
	(c-lang-var c-inexpr-class-kwds))
  ;; ObjC needs some tuning of the regexp.
  objc (concat "@" (c-lang-var c-decl-block-key)))
(c-lang-defvar c-decl-block-key (c-lang-var c-decl-block-key))

;; Keywords that can introduce bitfields.
(c-lang-defconst c-bitfield-kwds
  (c c++) '("char" "int" "long" "signed" "unsigned"))

;; Regexp matching the start of a bitfield (not uniquely), or nil in
;; languages without bitfield support.
(c-lang-defconst c-opt-bitfield-key
  (c c++) (c-make-keywords-re t (c-lang-var c-bitfield-kwds)))
(c-lang-defvar c-opt-bitfield-key (c-lang-var c-opt-bitfield-key))

;; All keywords as a list.
(c-lang-defconst c-keywords
  all (delete-duplicates (append (c-lang-var c-type-kwds)
				 (c-lang-var c-specifier-kwds)
				 (c-lang-var c-class-kwds)
				 (c-lang-var c-other-decl-block-kwds)
				 (c-lang-var c-block-decls-with-vars)
				 (c-lang-var c-type-prefix-kwds)
				 (c-lang-var c-other-decl-kwds)
				 (c-lang-var c-decl-spec-kwds)
				 (c-lang-var c-protection-kwds)
				 (c-lang-var c-block-stmt-1-kwds)
				 (c-lang-var c-block-stmt-2-kwds)
				 (c-lang-var c-simple-stmt-kwds)
				 (c-lang-var c-asm-stmt-kwds)
				 (c-lang-var c-label-kwds)
				 (c-lang-var c-constant-kwds)
				 (c-lang-var c-expr-kwds)
				 (c-lang-var c-lambda-kwds)
				 (c-lang-var c-inexpr-block-kwds)
				 (c-lang-var c-inexpr-class-kwds)
				 (c-lang-var c-bitfield-kwds)
				 nil)
			 :test 'string-equal))
(c-lang-defvar c-keywords (c-lang-var c-keywords))

;; All keywords as an adorned regexp.
(c-lang-defconst c-keywords-regexp
  all (c-make-keywords-re t (c-lang-var c-keywords)))
(c-lang-defvar c-keywords-regexp (c-lang-var c-keywords-regexp))

;; All keywords that aren't types as an adorned regexp.
(c-lang-defconst c-nontype-keywords-regexp
  all (c-make-keywords-re t
	(set-difference (c-lang-var c-keywords)
			(c-lang-var c-type-kwds)
			:test 'string-equal)))
(c-lang-defvar c-nontype-keywords-regexp
  (c-lang-var c-nontype-keywords-regexp))

;; Adorned regexp matching all keywords that can't appear at the start
;; of a declaration.
(c-lang-defconst c-not-decl-init-keywords
  all (c-make-keywords-re t
	(set-difference (c-lang-var c-keywords)
			(append (c-lang-var c-type-kwds)
				(c-lang-var c-specifier-kwds)
				(c-lang-var c-class-kwds)
				(c-lang-var c-other-decl-block-kwds)
				(c-lang-var c-block-decls-with-vars)
				(c-lang-var c-type-prefix-kwds))
			:test 'string-equal)))
(c-lang-defvar c-not-decl-init-keywords (c-lang-var c-not-decl-init-keywords))

;; Regexp matching an access protection label in a class, or nil in
;; languages that doesn't have such things.
(c-lang-defconst c-opt-access-key
  c++ (concat "\\("
	      (c-make-keywords-re nil (c-lang-var c-protection-kwds))
	      "\\)[ \t\n\r]*:"))
(c-lang-defconst c-opt-access-key
  objc (concat "@" (c-make-keywords-re t (c-lang-var c-protection-kwds))))
(c-lang-defvar c-opt-access-key (c-lang-var c-opt-access-key))

;; Regexp matching a normal label, i.e. not a label that's recognized
;; with a keyword, like switch labels.  It's only used at the
;; beginning of a statement.
(c-lang-defconst c-label-key
  all "\\<\\>"
  (c c++ java pike) (concat "\\(" (c-lang-var c-symbol-key) "\\)"
			    "[ \t\n\r]*:\\([^:]\\|$\\)"))
(c-lang-defvar c-label-key (c-lang-var c-label-key))

;; Regexp matching the beginning of a declaration specifier in the
;; region between the header and the body of a declaration.
;;
;; FIXME: This is currently not used uniformly; c++-mode and java-mode
;; each have their own ways of using it.
(c-lang-defconst c-opt-decl-spec-key
  c++ (concat ":?[ \t\n\r]*\\(virtual[ \t\n\r]+\\)?\\("
	      (c-make-keywords-re nil (c-lang-var c-protection-kwds))
	      "\\)[ \t\n\r]+"
	      "\\(" (c-lang-var c-symbol-key) "\\)")
  java (c-make-keywords-re t (c-lang-var c-decl-spec-kwds)))
(c-lang-defvar c-opt-decl-spec-key (c-lang-var c-opt-decl-spec-key))

;; Regexp describing friend declarations classes, or nil in languages
;; that doesn't have such things.
(c-lang-defconst c-opt-friend-key
  ;; FIXME: Ought to use `c-specifier-kwds' or similar, and the
  ;; template skipping isn't done properly.
  c++ "friend[ \t]+\\|template[ \t]*<.+>[ \t]*friend[ \t]+")
(c-lang-defvar c-opt-friend-key (c-lang-var c-opt-friend-key))

;; Special regexp to match the start of methods.
(c-lang-defconst c-opt-method-key
  objc (concat
	"^\\s *[+-]\\s *"
	"\\(([^)]*)\\)?"		; return type
	;; \\s- in objc syntax table does not include \n
	;; since it is considered the end of //-comments.
	"[ \t\n]*\\(" (c-lang-var c-symbol-key) "\\)"))
(c-lang-defvar c-opt-method-key (c-lang-var c-opt-method-key))

;; Regexp matching something that might precede a declaration or a
;; cast, e.g. the last token of a preceding statement or declaration.
;; It should not match bob, though.  We match a sequence of characters
;; to skip over things like "};" more quickly.
(c-lang-defconst c-decl-prefix-re
  (java idl objc) "[\{\}\(;,]+"
  ;; We additionally match ")" in C for K&R region declarations, and
  ;; in C and C++ for when a cpp macro definition begins with a
  ;; declaration.
  (c c++) "[\{\}\(\);,]+"
  ;; Also match "[" for multiple value assignments and type casts in Pike.
  pike "[\{\}\(\)\[;,]+")
(c-lang-defvar c-decl-prefix-re (c-lang-var c-decl-prefix-re))

;; Regexp matching the close paren(s) of a cast, or nil in languages
;; without casts.  Note that the corresponding open paren(s) should be
;; matched by `c-decl-prefix-re'.
(c-lang-defconst c-cast-close-paren-key
  (c c++ java) ")"
  pike "[\]\)]")
(c-lang-defvar c-cast-close-paren-key (c-lang-var c-cast-close-paren-key))

;; Regexp matching the operators that might precede the identifier in
;; a declaration, e.g. the "*" in "char *argv".  This regexp should
;; match "(" if parentheses are valid in type declarations.  The end
;; of the first submatch is taken as the end of the operator.
(c-lang-defconst c-type-decl-prefix-key
  all  "\\<\\>" ;; Default to a regexp that never matches.
  c    "\\([*\(]\\)\\($\\|[^=]\\)"
  c++  (concat "\\("
	       "[*\(&~]"
	       "\\|\\(\\(" (c-lang-var c-symbol-key) "\\)[ \t\n\r]*\\)?::"
	       "\\|const\\>\\|volatile\\>"
	       "\\)"
	       "\\($\\|[^=]\\)")
  pike "\\([*\(!~]\\)\\($\\|[^=]\\)")
(c-lang-defvar c-type-decl-prefix-key (c-lang-var c-type-decl-prefix-key))

;; Regexp matching the operators that might follow after the
;; identifier in a declaration, e.g. the "[" in "char argv[]".  This
;; regexp should match ")" if parentheses are valid in type
;; declarations.  If it matches an open paren of some kind, the type
;; declaration check continues at the corresponding close paren,
;; otherwise the end of the first submatch is taken as the end of the
;; operator.
(c-lang-defconst c-type-decl-suffix-key
  ;; Default to a regexp that matches only a function argument list
  ;; parenthesis.
  all  "\\(\(\\)"
  c    "\\([\)\[\(]\\)"
  c++  (concat "\\("
	       "[\)\[\(]"
	       "\\|const\\>\\|volatile\\>"
	       "\\)")
  java "\\([\[\(]\\)")
(c-lang-defvar c-type-decl-suffix-key (c-lang-var c-type-decl-suffix-key))

;; Regexp matching operators that concatenate types, e.g. the "|" in
;; "int|string" in Pike.  The end of the first submatch is taken as
;; the end of the operator.  nil in languages without such operators.
(c-lang-defconst c-type-concat-key
  pike "\\([|.&]\\)\\($\\|[^|.&]\\)")
(c-lang-defvar c-type-concat-key (c-lang-var c-type-concat-key))

;; Regexp matching operators that might follow after a type, or nil in
;; languages that doesn't have such operators.  The end of the first
;; submatch is taken as the end of the operator.
(c-lang-defconst c-type-suffix-key
  (c c++ pike) "\\(\\.\\.\\.\\)"
  java "\\(\\[[ \t\n\r]*\\]\\)")
(c-lang-defvar c-type-suffix-key (c-lang-var c-type-suffix-key))

;; Regexp matching the known type identifiers.  This is initialized
;; from the type keywords and `*-font-lock-extra-types'.  The first
;; submatch is the one that matches the type.  Note that this regexp
;; assumes that '_' and '$' have word syntax.
(c-lang-defvar c-known-type-key
  (let ((extra-types (c-mode-var "font-lock-extra-types")))
    (concat "\\<\\("
	    (c-make-keywords-re nil (c-lang-var c-type-kwds))
	    (if (consp extra-types)
		(concat "\\|" (mapconcat 'identity extra-types "\\|"))
	      "")
	    "\\)\\>")))

;; Regexp matching the prefix of a cpp directive in the languages that
;; normally uses that macro preprocessor.
(c-lang-defconst c-cpp-prefix
  (c c++) "^\\s *#\\s *"
  ;; The preprocessor in Pike recognizes cpp directives anywhere,
  ;; not just at boi.
  pike "#\\s *")

;; Name of functions in cpp expressions that take an identifier as the
;; argument.
(c-lang-defconst c-cpp-defined-fns
  (c c++) '("defined")
  pike '("defined" "efun" "constant"))

;; List of open- and close-chars that makes up a pike-style brace
;; list, i.e. for a `([ ])' list there should be a cons (?\[ . ?\]) in
;; this list.
(c-lang-defconst c-special-brace-lists pike '((?{ . ?})
					      (?\[ . ?\])
					      (?< . ?>)))
(c-lang-defvar c-special-brace-lists (c-lang-var c-special-brace-lists))

;; Non-nil means K&R style argument declarations are valid.
(c-lang-defconst c-recognize-knr-p c t)
(c-lang-defvar c-recognize-knr-p (c-lang-var c-recognize-knr-p))

;; Regexp to match the start of any type of comment.
;;
;; FIXME: Ought to use `c-comment-prefix-regexp' with some
;; modifications instead of this.
(c-lang-defconst c-comment-start-regexp
  ;; Might seem like overkill to make this a language dependent
  ;; constant, but awk-mode is on its way..
  all "/[/*]")
(c-lang-defvar c-comment-start-regexp (c-lang-var c-comment-start-regexp))

;; Regexp to match the start of documentation comments.
(c-lang-defconst c-doc-comment-start-regexp
  all "\\<\\>"
  ;; From font-lock.el: `doxygen' uses /*! while others use /**.
  (c c++ objc) "/\\*[*!]"
  java "/\\*\\*"
  pike "/[/*]!")
(c-lang-defvar c-doc-comment-start-regexp
  (c-lang-var c-doc-comment-start-regexp))

;; Strings that starts and ends comments inserted with M-; etc.
;; comment-start and comment-end are initialized from these.
(c-lang-defconst comment-start
  c "/* "
  (c++ objc java idl pike) "// ")
(c-lang-defvar comment-start (c-lang-var comment-start))
(c-lang-defconst comment-end
  c "*/"
  (c++ objc java idl pike) "")
(c-lang-defvar comment-end (c-lang-var comment-end))

;; Regexp matching a piece of syntactic whitespace that isn't a
;; sequence of simple whitespace characters.  As opposed to
;; `c-(forward|backward)-syntactic-ws', this doesn't regard cpp
;; directives as syntactic whitespace.
(c-lang-defconst c-nonwhite-syntactic-ws
  all (concat "/" (concat
		   "\\("
		   "/[^\n\r]*[\n\r]"	; Line comment.
		   "\\|"
		   ;; Block comment. We intentionally don't allow line
		   ;; breaks in them to avoid going very far and risk
		   ;; running out of regexp stack; this regexp is
		   ;; intended to handle only short comments that
		   ;; might be put in the middle of limited constructs
		   ;; like declarations.
		   "\\*\\([^*\n\r]\\|\\*[^/\n\r]\\)*\\*/"
		   "\\)")
	      "\\|"
	      "\\\\[\n\r]"))		; Line continuations.

;; Regexp matching syntactic whitespace, including possibly the empty
;; string.  As opposed to `c-(forward|backward)-syntactic-ws', this
;; doesn't regard cpp directives as syntactic whitespace.  Does not
;; contain a \| operator at the top level.
(c-lang-defconst c-syntactic-ws
  all (concat "[ \t\n\r]*\\("
	      "\\(" (c-lang-var c-nonwhite-syntactic-ws) "\\)"
	      "[ \t\n\r]*\\)*"))

;; Number of regexp grouping parens in `c-syntactic-ws'.
(c-lang-defconst c-syntactic-ws-depth
  all (c-regexp-opt-depth (c-lang-var c-syntactic-ws)))

;; Regexp matching syntactic whitespace, which is at least one
;; character long.  As opposed to `c-(forward|backward)-syntactic-ws',
;; this doesn't regard cpp directives as syntactic whitespace.  Does
;; not contain a \| operator at the top level.
(c-lang-defconst c-nonempty-syntactic-ws
  all (concat "\\([ \t\n\r]\\|"
	      (c-lang-var c-nonwhite-syntactic-ws)
	      "\\)+"))

;; Number of regexp grouping parens in `c-nonempty-syntactic-ws'.
(c-lang-defconst c-nonempty-syntactic-ws-depth
  all (c-regexp-opt-depth (c-lang-var c-nonempty-syntactic-ws)))

;; Regexp matching syntactic whitespace without any line breaks.  As
;; opposed to `c-(forward|backward)-syntactic-ws', this doesn't regard
;; cpp directives as syntactic whitespace.  Does not contain a \|
;; operator at the top level.
(c-lang-defconst c-single-line-syntactic-ws
  all (concat "[ \t]*\\("
	      "/\\*\\([^*\n\r]\\|\\*[^/\n\r]\\)*\\*/" ; Block comment
	      "[ \t]*\\)*"))

;; Number of regexp grouping parens in `c-single-line-syntactic-ws'.
(c-lang-defconst c-single-line-syntactic-ws-depth
  all (c-regexp-opt-depth (c-lang-var c-single-line-syntactic-ws)))

;; Regexp that matches when there is no syntactically significant text
;; before eol.  Macros are regarded as syntactically significant text
;; here.
(c-lang-defvar c-syntactic-eol
  (concat (concat
	   ;; Match horizontal whitespace and block comments that
	   ;; doesn't contain newlines.
	   "\\(\\s \\|"
	   (concat "/\\*"
		   "\\([^*\n\r]\\|\\*[^/\n\r]\\)*"
		   "\\*/")
	   "\\)*")
	  (concat
	   ;; Match eol (possibly inside a block comment), or the
	   ;; beginning of a line comment.  Note: This has to be
	   ;; modified for awk where line comments start with '#'.
	   "\\("
	   (concat "\\("
		   "/\\*\\([^*\n\r]\\|\\*[^/\n\r]\\)*"
		   "\\)?"
		   "$")
	   "\\|//\\)")))

;; Regexp to append to paragraph-start.
(c-lang-defconst paragraph-start
  (c c++ objc idl) "$"
  java "\\(@[a-zA-Z]+\\>\\|$\\)"	; For Javadoc.
  pike "\\(@[a-zA-Z]+\\>\\([^{]\\|$\\)\\|$\\)") ; For Pike refdoc.

;; Regexp to append to paragraph-separate.
(c-lang-defconst paragraph-separate
  (c c++ objc java idl) "$"
  pike (c-lang-var paragraph-start))

;; Prefix added to `c-current-comment-prefix' to set
;; `c-opt-in-comment-lc', or nil if it should be nil.
(c-lang-defconst c-in-comment-lc-prefix pike "@[\n\r]\\s *")

;; Regexp to match in-comment line continuations, or nil in languages
;; where that isn't applicable.  It's assumed that it only might match
;; from and including the last character on a line.  Built from
;; `*-in-comment-lc-prefix' and the current value of
;; `c-current-comment-prefix'.
(c-lang-defvar c-opt-in-comment-lc
  (if (c-lang-var c-in-comment-lc-prefix)
      (concat (c-lang-var c-in-comment-lc-prefix)
	      c-current-comment-prefix)))

(defconst c-init-lang-defvars
  ;; Make a lambda of the collected `c-lang-defvar' initializations.
  ;; We need to do it this way to make sure that the lambda is
  ;; compiled during compilation when `c-lang-defvar-init-form' has a
  ;; value.
  (cc-eval-when-compile
    (if (cc-bytecomp-is-compiling)
	;; `byte-compile-lambda' is an undocumented function in
	;; bytecomp.el.  We use it instead of `byte-compile' since
	;; that one resets the environment so we get warnings about
	;; all the variables in `c-lang-defvar-init-form'.
	(byte-compile-lambda `(lambda () ,c-lang-defvar-init-form))
      `(lambda () ,c-lang-defvar-init-form))))

(defun c-init-language-vars ()
  ;; Initialize all `c-lang-defvar' variables according to
  ;; `c-buffer-is-cc-mode'.
  (if (not (memq c-buffer-is-cc-mode
		 '(c-mode c++-mode objc-mode java-mode idl-mode pike-mode)))
      (error "Cannot initialize language variables for unknown mode %s"
	     c-buffer-is-cc-mode))
  (funcall c-init-lang-defvars))

;; Regexp trying to describe the beginning of a Java top-level
;; definition.  This is not used by CC Mode, nor is it maintained
;; since it's practically impossible to write a regexp that reliably
;; matches such a construct.  Other tools are necessary.
(defconst c-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*")


;; Syntax tables.

(defun c-populate-syntax-table (table)
  ;; Populate the syntax TABLE
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
  (modify-syntax-entry ?\' "\""    table)
  ;; Set up block and line oriented comments.  The new C standard
  ;; mandates both comment styles even in C, so since all languages
  ;; now require dual comments, we make this the default.
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; Emacs 19 & 20
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs"))
   )
  (modify-syntax-entry ?\n "> b"  table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" table))

;;;###autoload
(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table))

;;;###autoload
(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table))

(defvar c++-template-syntax-table nil
  "A variant of `c++-mode-syntax-table' that defines `<' and `>' as
parenthesis characters.  Used temporarily when template argument lists
are parsed.")
(if c++-template-syntax-table
    ()
  (setq c++-template-syntax-table
	(copy-syntax-table c++-mode-syntax-table))
  (modify-syntax-entry ?< "(>" c++-template-syntax-table)
  (modify-syntax-entry ?> ")<" c++-template-syntax-table))

;;;###autoload
(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra Objective-C only syntax
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table))

;;;###autoload
(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table))

;;;###autoload
(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(if idl-mode-syntax-table
    nil
  (setq idl-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table idl-mode-syntax-table))

;;;###autoload
(defvar pike-mode-syntax-table nil
  "Syntax table used in pike-mode buffers.")
(if pike-mode-syntax-table
    ()
  (setq pike-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table pike-mode-syntax-table)
  (modify-syntax-entry ?@ "." pike-mode-syntax-table))


;; internal state variables

;; Internal state of hungry delete key feature
(defvar c-hungry-delete-key nil)
(make-variable-buffer-local 'c-hungry-delete-key)

;; Internal state of auto newline feature.
(defvar c-auto-newline nil)
(make-variable-buffer-local 'c-auto-newline)

;; Internal auto-newline/hungry-delete designation string for mode line.
(defvar c-auto-hungry-string nil)
(make-variable-buffer-local 'c-auto-hungry-string)


(cc-provide 'cc-langs)

;;; cc-langs.el ends here
