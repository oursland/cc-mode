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
;; along with this program; see the file COPYING.  If not, write to
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
(require 'regexp-opt nil t)


;; Regular expressions and other values which must be parameterized on
;; a per-language basis.

(defun c-modeval (suffix)
  (let ((var (c-modevar suffix)))
    (and (boundp var) (symbol-value var))))

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
      (let ((re (if (fboundp 'regexp-opt)
		    ;; Doesn't exist in Emacs 19.34.
		    (regexp-opt lists adorn)
		  (concat (if adorn "\\(" "")
			  (mapconcat 'regexp-quote lists "\\|")
			  (if adorn "\\)" "")))))
	(if adorn
	    (concat re "\\>\\([^_]\\|$\\)")
	  re))
    "\\<\\>"))				; Matches nothing.
(put 'c-make-keywords-re 'lisp-indent-function 1)

;; Regexp describing a `symbol' in all languages, not excluding
;; keywords.  We cannot use just `word' syntax class since `_' cannot
;; be in word class.  Putting underscore in word class breaks forward
;; word movement behavior that users are familiar with.  Besides, this
;; runs counter to Emacs convention.
;;
;; This definition isn't correct for the first character in the
;; languages that accept the full range of word constituents in
;; identifiers (e.g. Java and Pike).  For that we'd need to make a
;; regexp that matches all characters in the word constituent class
;; except 0-9, and the regexp engine currently can't do that.
(defconst c-C-symbol-key "[_a-zA-Z]\\(\\w\\|\\s_\\)*")
(defconst c-C++-symbol-key c-C-symbol-key)
(defconst c-ObjC-symbol-key c-C-symbol-key)
(defconst c-Java-symbol-key c-C-symbol-key)
(defconst c-IDL-symbol-key c-C-symbol-key)
(defconst c-Pike-symbol-key
  (concat "\\(" c-C-symbol-key "\\|"
	  (c-make-keywords-re nil
	    '("`+" "`-" "`&" "`|" "`^" "`<<" "`>>" "`*" "`/" "`%" "`~" "`=="
	      "`<" "`>" "`!" "`[]" "`[]=" "`->" "`->=" "`()" "``+" "``-" "``&"
	      "``|" "``^" "``<<" "``>>" "``*" "``/" "``%" "`+="))
	  "\\)"))
(defvar c-symbol-key nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-symbol-key)

(defvar c-stmt-delim-chars "^;{}?:")
;; The characters that should be considered to bound statements.  To
;; optimize c-crosses-statement-barrier-p somewhat, it's assumed to
;; begin with "^" to negate the set.  If ? : operators should be
;; detected then the string must end with "?:".

(defvar c-stmt-delim-chars-with-comma "^;,{}?:")
;; Variant of c-stmt-delim-chars that additionally contains ','.

;; HELPME: Many of the following keyword lists are more or less bogus
;; for some languages (notably ObjC and IDL).  The effects of the
;; erroneous values in the language handling are mostly negligible
;; since the constants that actually matter in the syntax detection
;; code are mostly correct in the situations they are used, but I'd
;; still appreciate help to get them correct for other uses.

;; Primitive type keywords.
(defconst c-C-primitive-type-kwds
  '("char" "double" "float" "int" "long" "short" "signed" "unsigned" "void"))
(defconst c-C++-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-ObjC-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-Java-primitive-type-kwds
  '("boolean" "byte" "char" "double" "float" "int" "long" "short" "void"))
(defconst c-IDL-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-Pike-primitive-type-kwds
  '("constant" "float" "int" "mapping" "multiset" "object" "program"
    "string" "void"))

;; Declaration specifier keywords.
(defconst c-C-specifier-kwds
  '("auto" "const" "extern" "register" "static" "volatile"))
(defconst c-C++-specifier-kwds
  (nconc '("friend" "inline" "virtual") c-C-specifier-kwds))
(defconst c-ObjC-specifier-kwds c-C++-specifier-kwds)
(defconst c-Java-specifier-kwds
  ;; Note: `const' is not used, but it's still a reserved keyword.
  '("abstract" "const" "final" "native" "private" "protected"
    "public" "static" "synchronized" "transient" "volatile"))
(defconst c-IDL-specifier-kwds c-C++-specifier-kwds)
(defconst c-Pike-specifier-kwds
  '("final" "inline" "local" "nomask" "optional" "private"
    "protected" "static" "variant"))

;; Class/struct declaration keywords.
(defconst c-C-class-kwds '("struct" "union"))
(defconst c-C++-class-kwds (nconc '("class") c-C-class-kwds))
(defconst c-ObjC-class-kwds '("interface" "implementation"))
(defconst c-Java-class-kwds '("class" "interface"))
(defconst c-IDL-class-kwds (nconc '("interface" "valuetype") c-C++-class-kwds))
(defconst c-Pike-class-kwds '("class"))

;; Regexp matching the start of a class.  Built from *-class-kwds.
(defvar c-class-key nil)
(make-variable-buffer-local 'c-class-key)

;; Keywords introducing blocks besides classes that contain another
;; declaration level.
(defconst c-C-other-decl-block-kwds '("extern"))
(defconst c-C++-other-decl-block-kwds
  (nconc '("namespace") c-C-other-decl-block-kwds))
;(defconst c-ObjC-other-decl-block-kwds nil)
;(defconst c-Java-other-decl-block-kwds nil)
(defconst c-IDL-other-decl-block-kwds '("module"))
;(defconst c-Pike-other-decl-block-kwds nil)

;; Regexp matching the start of blocks besides classes that contain
;; another declaration level.  Built from *-other-decl-block-kwds.
(defvar c-other-decl-block-key nil)
(make-variable-buffer-local 'c-other-decl-block-key)

;; Regexp matching the start of any declaration-level block that
;; contain another declaration level, i.e. that isn't a function
;; block.  Built from the union of *-class-kwds,
;; *-other-decl-block-kwds, and *-inexpr-class-kwds.
(defvar c-decl-block-key nil)
(make-variable-buffer-local 'c-decl-block-key)

;; Keywords introducing other declarations.
(defconst c-C-other-decl-kwds '("enum" "typedef"))
;; FIXME: Shouldn't "template" be moved to c-C++-specifier-kwds?
(defconst c-C++-other-decl-kwds (nconc '("template") c-C-other-decl-kwds))
;(defconst c-ObjC-other-decl-kwds nil)
(defconst c-Java-other-decl-kwds '("import" "package"))
;(defconst c-IDL-other-decl-kwds nil)
(defconst c-Pike-other-decl-kwds '("import" "inherit"))

;; Keywords introducing extra declaration specifiers in the region
;; between the header and the body (i.e. the "K&R-region") in
;; declarations.
;(defconst c-C-decl-spec-kwds nil)
;(defconst c-C++-decl-spec-kwds nil)
;(defconst c-ObjC-decl-spec-kwds nil)
(defconst c-Java-decl-spec-kwds '("extends" "implements" "throws"))
;(defconst c-IDL-decl-spec-kwds nil)
;(defconst c-Pike-decl-spec-kwds nil)

;; Protection label keywords in classes.
;(defconst c-C-protection-kwds nil)
(defconst c-C++-protection-kwds '("private" "protected" "public"))
(defconst c-ObjC-protection-kwds c-C++-protection-kwds)
;(defconst c-Java-protection-kwds nil)
;(defconst c-IDL-protection-kwds nil)
;(defconst c-Pike-protection-kwds nil)

;; Statement keywords followed directly by a substatement.
(defconst c-C-block-stmt-1-kwds '("do" "else"))
(defconst c-C++-block-stmt-1-kwds (nconc '("asm" "try") c-C-block-stmt-1-kwds))
(defconst c-ObjC-block-stmt-1-kwds c-C++-block-stmt-1-kwds)
(defconst c-Java-block-stmt-1-kwds
  (nconc '("finally" "try") c-C-block-stmt-1-kwds))
;(defconst c-IDL-block-stmt-1-kwds nil)
(defconst c-Pike-block-stmt-1-kwds c-C-block-stmt-1-kwds)

;; Regexp matching the start of any statement followed directly by a
;; substatement (doesn't match a bare block, however).  Built from
;; *-block-stmt-1-kwds.
(defvar c-block-stmt-1-key nil)
(make-variable-buffer-local 'c-block-stmt-1-key)

;; Statement keywords followed by a paren sexp and then by a substatement.
(defconst c-C-block-stmt-2-kwds '("for" "if" "switch" "while"))
(defconst c-C++-block-stmt-2-kwds (nconc '("catch") c-C-block-stmt-2-kwds))
(defconst c-ObjC-block-stmt-2-kwds c-C++-block-stmt-2-kwds)
(defconst c-Java-block-stmt-2-kwds
  (nconc '("synchronized") c-C++-block-stmt-2-kwds))
;(defconst c-IDL-block-stmt-2-kwds nil)
(defconst c-Pike-block-stmt-2-kwds (nconc '("foreach") c-C-block-stmt-2-kwds))

;; Regexp matching the start of any statement followed by a paren sexp
;; and then by a substatement.  Built from *-block-stmt-2-kwds.
(defvar c-block-stmt-2-key nil)
(make-variable-buffer-local 'c-block-stmt-2-key)

;; Regexp matching the start of any statement that has a substatement
;; (except a bare block), i.e. built from the union of
;; *-block-stmt-1-kwds and *-block-stmt-2-kwds.  Nil in languages that
;; doesn't have such constructs.
(defvar c-opt-block-stmt-key nil)
(make-variable-buffer-local 'c-opt-block-stmt-key)

;; Statement keywords followed by an expression or nothing.
(defconst c-C-simple-stmt-kwds '("break" "continue" "goto" "return"))
(defconst c-C++-simple-stmt-kwds c-C-simple-stmt-kwds)
(defconst c-ObjC-simple-stmt-kwds c-C-simple-stmt-kwds)
(defconst c-Java-simple-stmt-kwds
  ;; Note: `goto' is not a valid statement, but the keyword is still reserved.
  (nconc '("throw") c-C-simple-stmt-kwds))
;(defconst c-IDL-simple-stmt-kwds nil)
(defconst c-Pike-simple-stmt-kwds '("break" "continue" "return"))

;; Keywords introducing labels in blocks.
(defconst c-C-label-kwds '("case" "default"))
(defconst c-C++-label-kwds c-C-label-kwds)
(defconst c-ObjC-label-kwds c-C-label-kwds)
(defconst c-Java-label-kwds c-C-label-kwds)
;(defconst c-IDL-label-kwds nil)
(defconst c-Pike-label-kwds c-C-label-kwds)

;; Regexp matching any keyword that introduces a label.  Built from
;; *-label-kwds.
(defvar c-label-kwds-regexp nil)
(make-variable-buffer-local 'c-label-kwds-regexp)

;; Keywords that can occur anywhere in expressions.
(defconst c-C-expr-kwds '("sizeof"))
(defconst c-C++-expr-kwds
  (nconc '("delete" "new" "operator" "this" "throw") c-C-expr-kwds))
(defconst c-ObjC-expr-kwds c-C-expr-kwds)
(defconst c-Java-expr-kwds '("instanceof" "new" "super" "this"))
;(defconst c-IDL-expr-kwds nil)
(defconst c-Pike-expr-kwds
  (nconc '("catch" "class" "gauge" "lambda" "predef") c-C-expr-kwds))

;; Keywords that start lambda constructs, i.e. function definitions in
;; expressions.
;(defconst c-C-lambda-kwds nil)
;(defconst c-C++-lambda-kwds nil)
;(defconst c-ObjC-lambda-kwds nil)
;(defconst c-Java-lambda-kwds nil)
;(defconst c-IDL-lambda-kwds nil)
(defconst c-Pike-lambda-kwds '("lambda"))

;; Regexp matching the start of lambda constructs, or nil in languages
;; that doesn't have such things.  Built from *-lambda-kwds.
(defvar c-opt-lambda-key nil)
(make-variable-buffer-local 'c-opt-lambda-key)

;; Keywords that start constructs followed by statement blocks which
;; can be used in expressions (the gcc extension for this in C and C++
;; is handled separately, though).
;(defconst c-C-inexpr-block-kwds nil)
;(defconst c-C++-inexpr-block-kwds nil)
;(defconst c-ObjC-inexpr-block-kwds nil)
;(defconst c-Java-inexpr-block-kwds nil)
;(defconst c-IDL-inexpr-block-kwds nil)
(defconst c-Pike-inexpr-block-kwds '("catch" "gauge"))

;; Regexp matching the start of in-expression statements, or nil in
;; languages that doesn't have such things.  Built from
;; *-inexpr-block-kwds.
(defvar c-opt-inexpr-block-key nil)
(make-variable-buffer-local 'c-opt-inexpr-block-key)

;; Keywords that start classes in expressions.
;(defconst c-C-inexpr-class-kwds nil)
;(defconst c-C++-inexpr-class-kwds nil)
;(defconst c-ObjC-inexpr-class-kwds nil)
(defconst c-Java-inexpr-class-kwds '("new"))
;(defconst c-IDL-inexpr-class-kwds nil)
(defconst c-Pike-inexpr-class-kwds '("class"))

;; Regexp matching the start of a class in an expression, or nil in
;; languages that doesn't have such things.  Built from
;; *-inexpr-class-kwds.
(defvar c-opt-inexpr-class-key nil)
(make-variable-buffer-local 'c-opt-inexpr-class-key)

;; Regexp matching the start of any class, both at top level and in
;; expressions.  Built from *-class-kwds and *-inexpr-class-kwds.
(defvar c-any-class-key nil)
(make-variable-buffer-local 'c-any-class-key)

;; Keywords that can introduce bitfields.
(defconst c-C-bitfield-kwds '("char" "int" "long" "signed" "unsigned"))
(defconst c-C++-bitfield-kwds c-C-bitfield-kwds)
;(defconst c-ObjC-bitfield-kwds nil)
;(defconst c-Java-bitfield-kwds nil)
;(defconst c-IDL-bitfield-kwds nil)
;(defconst c-Pike-bitfield-kwds nil)

;; Regexp matching the start of a bitfield (not uniquely), or nil in
;; languages without bitfield support.  Built from *-bitfield-kwds.
(defvar c-opt-bitfield-key nil)
(make-variable-buffer-local 'c-opt-bitfield-key)

;; All keywords.  Built from *-kwds above.
(defvar c-keywords nil)			; As a list.
(defvar c-keywords-regexp nil)		; As an adorned regexp.
(make-variable-buffer-local 'c-keywords)
(make-variable-buffer-local 'c-keywords-regexp)

;; Regexp matching an access protection label in a class, or nil in
;; languages that doesn't have such things.
;(defconst c-C-access-key nil)
(defconst c-C++-access-key
  (concat "\\("
	  (c-make-keywords-re nil c-C++-protection-kwds)
	  "\\)[ \t\n\r]*:"))
(defconst c-ObjC-access-key
  (concat "@" (c-make-keywords-re t c-ObjC-protection-kwds)))
;(defconst c-Java-access-key nil)
;(defconst c-IDL-access-key nil)
;(defconst c-Pike-access-key nil)
(defvar c-opt-access-key nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-opt-access-key)

;; Regexp matching a normal label, i.e. not a label that's recognized
;; with a keyword, like switch labels.  It's only used at the
;; beginning of a statement.
(defvar c-label-key nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-label-key)

;; Regexp matching the beginning of a declaration specifier in the
;; region between region and a body of a declaration.
;;
;; FIXME: This is currently not used in a uniformly; c++-mode and
;; java-mode each have their own ways of using it.
;(defconst c-C-decl-spec-key nil)
(defconst c-C++-decl-spec-key
  (concat ":?[ \t\n\r]*\\(virtual[ \t\n\r]+\\)?"
	  (c-make-keywords-re nil c-C++-protection-kwds)
	  "[ \t\n\r]+"
	  c-C++-symbol-key))
;(defconst c-ObjC-decl-spec-key nil)
(defconst c-Java-decl-spec-key (c-make-keywords-re t c-Java-decl-spec-kwds))
;(defconst c-IDL-decl-spec-key nil)
;(defconst c-Pike-decl-spec-key nil)
(defvar c-opt-decl-spec-key nil)	; Set by c-init-language-vars.
(make-variable-buffer-local 'c-opt-decl-spec-key)

;; Regexp describing friend declarations classes, or nil in languages
;; that doesn't have such things.
;(defconst c-C-friend-key nil)
;; FIXME: Ought to use *-specifier-kwds or similar, and the template
;; skipping isn't done properly.
(defconst c-C++-friend-key
  "friend[ \t]+\\|template[ \t]*<.+>[ \t]*friend[ \t]+")
;(defconst c-ObjC-friend-key nil)
;(defconst c-Java-friend-key nil)
;(defconst c-IDL-friend-key nil)
;(defconst c-Pike-friend-key nil)
(defvar c-opt-friend-key nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-opt-friend-key)

;; Special regexp to match the start of methods.
;(defconst c-C-method-key nil)
;(defconst c-C++-method-key nil)
(defconst c-ObjC-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in objc syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-ObjC-symbol-key))
;(defconst c-Java-method-key nil)
;(defconst c-IDL-method-key nil)
;(defconst c-Pike-method-key nil)
(defvar c-opt-method-key nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-opt-method-key)

;; List of open- and close-chars that makes up a pike-style brace
;; list, i.e. for a `([ ])' list there should be a cons (?\[ . ?\]) in
;; this list.
;(defconst c-C-special-brace-lists nil)
;(defconst c-C++-special-brace-lists nil)
;(defconst c-ObjC-special-brace-lists nil)
;(defconst c-Java-special-brace-lists nil)
;(defconst c-IDL-special-brace-lists nil)
(defconst c-Pike-special-brace-lists '((?{ . ?})
				       (?\[ . ?\])
				       (?< . ?>)))
(defvar c-special-brace-lists nil)	; Set by c-init-language-vars.
(make-variable-buffer-local 'c-special-brace-lists)

;; Non-nil means K&R style argument declarations are valid.
(defconst c-C-recognize-knr-p t)
;(defconst c-C++-recognize-knr-p nil)
;(defconst c-ObjC-recognize-knr-p nil)
;(defconst c-Java-recognize-knr-p nil)
;(defconst c-IDL-recognize-knr-p nil)
;(defconst c-Pike-recognize-knr-p nil)
(defvar c-recognize-knr-p nil)		; Set by c-init-language-vars.
(make-variable-buffer-local 'c-recognize-knr-p)

;; Regexp to match the start of any type of comment.
;;
;; FIXME: Ought to use c-comment-prefix-regexp with some modifications
;; instead of this.
(defconst c-C-comment-start-regexp "/[/*]")
(defconst c-C++-comment-start-regexp c-C-comment-start-regexp)
(defconst c-ObjC-comment-start-regexp c-C-comment-start-regexp)
;; We need to match all 3 Java style comments
;; 1) Traditional C block; 2) javadoc /** ...; 3) C++ style
(defconst c-Java-comment-start-regexp "/\\(/\\|[*][*]?\\)")
(defconst c-IDL-comment-start-regexp c-C-comment-start-regexp)
(defconst c-Pike-comment-start-regexp c-C-comment-start-regexp)
(defvar c-comment-start-regexp nil)	; Set by c-init-language-vars.
(make-variable-buffer-local 'c-comment-start-regexp)

;; Strings that starts and ends comments inserted with M-; etc.
;; comment-start and comment-end are initialized from these.
(defconst c-C-comment-start "/* ")
(defconst c-C-comment-end "*/")
(defconst c-C++-comment-start "// ")
(defconst c-C++-comment-end "")
(defconst c-ObjC-comment-start "// ")
(defconst c-ObjC-comment-end "")
(defconst c-Java-comment-start "// ")
(defconst c-Java-comment-end "")
(defconst c-IDL-comment-start "// ")
(defconst c-IDL-comment-end "")
(defconst c-Pike-comment-start "// ")
(defconst c-Pike-comment-end "")

;; Regexp to append to paragraph-start.
(defconst c-C-paragraph-start "$")
(defconst c-C++-paragraph-start "$")
(defconst c-ObjC-paragraph-start "$")
(defconst c-Java-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")		; For Javadoc.
(defconst c-IDL-paragraph-start "$")
(defconst c-Pike-paragraph-start
  "\\(@[a-zA-Z]+\\>\\([^{]\\|$\\)\\|$\\)") ; For Pike refdoc.

;; Regexp to append to paragraph-separate.
(defconst c-C-paragraph-separate "$")
(defconst c-C++-paragraph-separate "$")
(defconst c-ObjC-paragraph-separate "$")
(defconst c-Java-paragraph-separate "$")
(defconst c-IDL-paragraph-separate "$")
(defconst c-Pike-paragraph-separate c-Pike-paragraph-start)

;; Prefix added to `c-current-comment-prefix' to set
;; `c-opt-in-comment-lc', or nil if it should be nil.
;(defconst c-C-in-comment-lc-prefix nil)
;(defconst c-C++-in-comment-lc-prefix nil)
;(defconst c-ObjC-in-comment-lc-prefix nil)
;(defconst c-Java-in-comment-lc-prefix nil)
;(defconst c-IDL-in-comment-lc-prefix nil)
(defconst c-Pike-in-comment-lc-prefix "@[\n\r]\\s *")

;; Regexp to match in-comment line continuations, or nil in languages
;; where that isn't applicable.  It's assumed that it only might match
;; from and including the last character on a line.  Built from
;; *-in-comment-lc-prefix.
(defvar c-opt-in-comment-lc nil)
(make-variable-buffer-local 'c-opt-in-comment-lc)

(defun c-init-language-vars ()
  (if (not (memq c-buffer-is-cc-mode '(c-mode c++-mode objc-mode java-mode
				       idl-mode pike-mode)))
      (error "Cannot initialize language variables for unknown mode %s"
	     c-buffer-is-cc-mode))

  (setq c-symbol-key (c-modeval "symbol-key"))

  (setq c-other-decl-block-key (c-make-keywords-re t
				 (c-modeval "other-decl-block-kwds"))
	c-decl-block-key (c-make-keywords-re t
			   (c-modeval "class-kwds")
			   (c-modeval "other-decl-block-kwds")
			   (c-modeval "inexpr-class-kwds"))
	c-class-key (c-make-keywords-re t
		      (c-modeval "class-kwds"))
	c-any-class-key (c-make-keywords-re t
			  (c-modeval "class-kwds")
			  (c-modeval "inexpr-class-kwds")))
  (if (c-major-mode-is 'objc-mode)
      (setq c-class-key (concat "@" c-class-key)
	    c-decl-block-key (concat "@" c-decl-block-key)
	    c-any-class-key (concat "@" c-any-class-key)))

  (setq c-block-stmt-1-key (c-make-keywords-re t
			     (c-modeval "block-stmt-1-kwds"))
	c-block-stmt-2-key (c-make-keywords-re t
			     (c-modeval "block-stmt-2-kwds"))
	c-opt-block-stmt-key (and (or (c-modeval "block-stmt-1-kwds")
				      (c-modeval "block-stmt-2-kwds"))
				  (c-make-keywords-re t
				    (c-modeval "block-stmt-1-kwds")
				    (c-modeval "block-stmt-2-kwds")))
	c-label-kwds-regexp (c-make-keywords-re t
			      (c-modeval "label-kwds"))
	c-opt-lambda-key (and (c-modeval "lambda-kwds")
			      (c-make-keywords-re t
				(c-modeval "lambda-kwds")))
	c-opt-inexpr-block-key (and (c-modeval "inexpr-block-kwds")
				    (c-make-keywords-re t
				      (c-modeval "inexpr-block-kwds")))
	c-opt-inexpr-class-key (and (c-modeval "inexpr-class-kwds")
				    (c-make-keywords-re t
				      (c-modeval "inexpr-class-kwds")))
	c-opt-bitfield-key (and (c-modeval "bitfield-kwds")
				(c-make-keywords-re t
				  (c-modeval "bitfield-kwds"))))

  (setq c-keywords (delete-duplicates
		    (append (c-modeval "primitive-type-kwds")
			    (c-modeval "specifier-kwds")
			    (c-modeval "class-kwds")
			    (c-modeval "other-decl-block-kwds")
			    (c-modeval "other-decl-kwds")
			    (c-modeval "decl-spec-kwds")
			    (c-modeval "protection-kwds")
			    (c-modeval "block-stmt-1-kwds")
			    (c-modeval "block-stmt-2-kwds")
			    (c-modeval "simple-stmt-kwds")
			    (c-modeval "label-kwds")
			    (c-modeval "expr-kwds")
			    (c-modeval "lambda-kwds")
			    (c-modeval "inexpr-block-kwds")
			    (c-modeval "inexpr-class-kwds")
			    (c-modeval "bitfield-kwds")
			    nil)
		    :test 'string-equal)
	c-keywords-regexp (c-make-keywords-re t c-keywords))

  (setq c-opt-access-key (c-modeval "access-key")
	c-label-key (concat c-symbol-key "[ \t\n\r]*:\\([^:]\\|$\\)")
	c-opt-decl-spec-key (c-modeval "decl-spec-key")
	c-opt-friend-key (c-modeval "friend-key")
	c-opt-method-key (c-modeval "method-key")
	c-special-brace-lists (c-modeval "special-brace-lists")
	c-recognize-knr-p (c-modeval "recognize-knr-p"))

  (setq c-comment-start-regexp (c-modeval "comment-start-regexp")
	comment-start (c-modeval "comment-start")
	comment-end (c-modeval "comment-end")
	c-opt-in-comment-lc (and (c-modeval "in-comment-lc-prefix")
				 (concat (c-modeval "in-comment-lc-prefix")
					 c-current-comment-prefix))))

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
