;;; cc-fonts.el --- font lock support for CC Mode

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
;; font-lock-function-name-face	Function names in declarations and definitions,
;;				and classes in those contexts.  Also used for
;;				preprocessor defines with arguments.
;; font-lock-variable-name-face	Variables in declarations and definitions, and
;;				other identifiers in such variable contexts.
;;				Also used for preprocessor defines without
;;				arguments.
;; font-lock-constant-face	Builtin constants.  As opposed to the preceding
;;				two faces, this one is used on the names in
;;				expressions, and it's not used in
;;				declarations, even if there happen to be a
;;				"const" in them somewhere.
;; font-lock-type-face		Types (both pre- and user defined) and classes
;;				in type contexts.
;; font-lock-reference-face	Name qualifiers.
;; font-lock-builtin-face	Not used directly.
;;
;; Face aliases, mapped to different faces depending on (X)Emacs flavor:
;;
;; c-doc-face			Documentation comments (like Javadoc).
;; c-label-face			Label identifiers.
;; c-preprocessor-face		Preprocessor directives.
;; c-invalid-face		Invalid syntax.  Note that CC Mode normally
;;				doesn't try to fontify syntax errors.  Instead
;;				it's as picky as possible about only
;;				fontifying syntactically correct structures so
;;				that incorrect ones simply aren't fontified.
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
;;    `font-lock-comment-face' in older Emacs (that since source
;;    documentation are actually comments in these languages, as
;;    opposed to elisp).  FIXME: Doc comment fontification currently
;;    only works with font-lock packages that have
;;    `font-lock-syntactic-face-function', i.e Emacs 21 and later.
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
(cc-require-when-compile 'cc-langs)
(cc-require 'cc-vars)
(cc-require 'cc-engine)

;; Avoid repeated loading through the eval-after-load directive in
;; cc-mode.el.
(provide 'cc-fonts)

(require 'font-lock)

;; Need to declare these local symbols during compilation since
;; they're referenced from lambdas in `byte-compile' calls that are
;; executed at compile time.  They don't need to have the proper
;; definitions, though, since the generated functions aren't called
;; during compilation.
(cc-bytecomp-defvar c-preprocessor-face)
(cc-bytecomp-defun c-fontify-recorded-types-and-refs)
(cc-bytecomp-defun c-font-lock-identifier-list)
(cc-bytecomp-defun c-font-lock-declarators)


;; Note that font-lock in XEmacs doesn't expand face names as
;; variables, so we have to use the (eval . FORM) in the font lock
;; matchers wherever we use these alias variables.

(defvar c-doc-face
  (cond ((c-face-name-p 'font-lock-doc-string-face)
	 ;; XEmacs.
	 'font-lock-doc-string-face)
	((c-face-name-p 'font-lock-doc-face)
	 ;; Emacs 21 and later.
	 'font-lock-doc-face)
	(t
	 'font-lock-comment-face))
  "Face name used for source documentation that's extracted by special
tools (e.g. Javadoc).")

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
  (cond ((c-face-name-p 'font-lock-label-face)
	 ;; If it happen to occur in the future.  (Well, the more
	 ;; pragmatic reason is to get unique faces for the test
	 ;; suite.)
	 'font-lock-label-face)
	((and (c-face-name-p 'font-lock-constant-face)
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
      (t (:inverse-video t)))
    "Face used to highlight invalid syntax."
    :group 'c-fonts))

;; To make hard spaces visible an inverted version of `c-invalid-face'
;; is used.  Since font-lock in Emacs expands all face names in
;; `font-lock-keywords' as variables we need to have a variable for it
;; that resolves to its own name.
(defconst c-nonbreakable-space-face 'c-nonbreakable-space-face)

(cc-bytecomp-defun face-inverse-video-p) ; Only in Emacs.
(cc-bytecomp-defun face-property-instance) ; Only in XEmacs.

(defun c-make-inverse-face (oldface newface)
  ;; Emacs and XEmacs have completely different face manipulation
  ;; routines. :P
  ;;
  ;; This function does not do any hidden buffer changes
  (copy-face oldface newface)
  (cond ((fboundp 'face-inverse-video-p)
	 ;; Emacs 20 and later.  This only looks at the inverse flag
	 ;; in the current frame.  Other display configurations might
	 ;; be different, but it can only show if the same Emacs has
	 ;; frames on e.g. a color and a monochrome display
	 ;; simultaneously.
	 (unless (face-inverse-video-p oldface)
	   (invert-face newface)))
	((fboundp 'face-property-instance)
	 ;; XEmacs.  Same pitfall here.
	 (unless (face-property-instance oldface 'reverse)
	   (invert-face newface)))
	(t
	 ;; Emacs 19 has no inverse flag at all.  Just inverse the
	 ;; face and hope it wasn't inversed already.
	 (invert-face newface))))

(eval-and-compile
  ;; We need the following functions during compilation since they're
  ;; called when the `c-lang-defconst' initializers are evaluated.
  ;; Define them at runtime too for the sake of derived modes.

  (defmacro c-put-font-lock-face (from to face)
    ;; Put a face on a region (overriding any existing face) in the way
    ;; font-lock would do it.  In XEmacs that means putting an
    ;; additional font-lock property, or else the font-lock package
    ;; won't recognize it as fontified and might override it
    ;; incorrectly.
    (if (fboundp 'font-lock-set-face)
	;; Note: This function has no docstring in XEmacs so it might be
	;; considered internal.
	`(font-lock-set-face ,from ,to ,face)
      `(put-text-property ,from ,to 'face ,face)))

  (defmacro c-fontify-types-and-refs (varlist &rest body)
    ;; Like `let', but additionally activates `c-record-type-identifiers'
    ;; and `c-record-ref-identifiers', and fontifies the recorded ranges
    ;; accordingly on exit.
    `(let ((c-record-type-identifiers t)
	   c-record-ref-identifiers
	   ,@varlist)
       (prog1 (progn ,@body)
	 (c-fontify-recorded-types-and-refs))))
  (put 'c-fontify-types-and-refs 'lisp-indent-function 1)
  (eval-after-load "edebug" '(def-edebug-spec c-fontify-types-and-refs let*))

  (defsubst c-skip-comments-and-strings (limit)
    ;; If the point is within a region fontified as a comment or
    ;; string literal skip to the end of it or to LIMIT, whichever
    ;; comes first, and return t.  Otherwise return nil.  The match
    ;; data is not clobbered.
    (when (memq (get-text-property (point) 'face)
		'(font-lock-comment-face font-lock-string-face))
      (goto-char (next-single-property-change (point) 'face nil limit))
      t))

  (defun c-make-font-lock-search-function (regexp &rest highlights)
    ;; This function makes a byte compiled function that works much like
    ;; a matcher element in `font-lock-keywords'.  It cuts out a little
    ;; bit of the overhead compared to a real matcher.  The main reason
    ;; is however to pass the real search limit to the anchored
    ;; matcher(s), since most (if not all) font-lock implementations
    ;; arbitrarily limits anchored matchers to the same line, and also
    ;; to insulate against various other irritating differences between
    ;; the different (X)Emacs font-lock packages.
    ;;
    ;; REGEXP is the matcher, which must be a regexp.  Only matches
    ;; where the beginning is outside any comment or string literal are
    ;; significant.
    ;;
    ;; HIGHLIGHTS is a list of highlight specs, just like in
    ;; `font-lock-keywords', with these limitations: The face is always
    ;; overridden (no big disadvantage, since hits in comments etc are
    ;; filtered anyway), there is no "laxmatch", and an anchored matcher
    ;; is always a form which must do all the fontification directly.
    ;; `limit' is a variable bound to the real limit in the context of
    ;; the anchored matcher forms.
    ;;
    ;; This function does not do any hidden buffer changes, but the
    ;; generated functions will.  They are however used in places
    ;; covered by the font-lock context.

    ;; Note: Replace `byte-compile' with `eval' to debug the generated
    ;; lambda easier.
    (byte-compile
     `(lambda (limit)
	(let (-match-end-pos-)
	  (while (re-search-forward ,regexp limit t)
	    (setq -match-end-pos- (point))
	    (unless (progn
		      (goto-char (match-beginning 0))
		      (c-skip-comments-and-strings limit))
	      (goto-char -match-end-pos-)
	      ,@(mapcar
		 (lambda (highlight)
		   (if (integerp (car highlight))
		       (progn
			 (unless (nth 2 highlight)
			   (error
			    "The override flag must currently be set in %s"
			    highlight))
			 (when (nth 3 highlight)
			   (error
			    "The laxmatch flag may currently not be set in %s"
			    highlight))
			 `(save-match-data
			    (c-put-font-lock-face
			     (match-beginning ,(car highlight))
			     (match-end ,(car highlight))
			     ,(elt highlight 1))))
		     (when (nth 3 highlight)
		       (error "Match highlights currently not supported in %s"
			      highlight))
		     `(progn
			,(nth 1 highlight)
			(save-match-data ,(car highlight))
			,(nth 2 highlight))))
		 highlights))))
	nil))))

(defun c-fontify-recorded-types-and-refs ()
  ;; Converts the ranges recorded on `c-record-type-identifiers' and
  ;; `c-record-ref-identifiers' to fontification.
  (let (elem)
    (while (consp c-record-type-identifiers)
      (setq elem (car c-record-type-identifiers)
	    c-record-type-identifiers (cdr c-record-type-identifiers))
      (c-put-font-lock-face (car elem) (cdr elem)
			    'font-lock-type-face))
    (while c-record-ref-identifiers
      (setq elem (car c-record-ref-identifiers)
	    c-record-ref-identifiers (cdr c-record-ref-identifiers))
      ;; Note that the reference face is a variable that is
      ;; dereferenced, since it's an alias in Emacs.
      (c-put-font-lock-face (car elem) (cdr elem)
			    font-lock-reference-face))))

(defun c-font-lock-syntactic-face-function (state)
  ;; This function can make hidden buffer changes, but the font-lock
  ;; context covers that.
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
  "Font lock matchers for preprocessor directives and purely lexical
stuff.  Used on level 1 and higher."

  ;; Note: In C++, `c-font-lock-declarations' assumes that no matcher
  ;; here sets `font-lock-type-face'.

  t `(,@(when (c-lang-const c-opt-cpp-prefix)
	  (let* ((noncontinued-line-end "\\(\\=\\|\\(\\=\\|[^\\]\\)[\n\r]\\)")
		 (ncle-depth (c-regexp-opt-depth noncontinued-line-end)))
	    `(;; The stuff after #error and #warning is a message, so
	      ;; fontify it as a string.
	      (,(concat noncontinued-line-end
			(c-lang-const c-opt-cpp-prefix)
			"\\(error\\|warning\\)\\>\\s *\\(.*\\)$")
	       ,(+ ncle-depth 2) font-lock-string-face)

	      ;; Fontify filenames in #include <...> as strings.
	      (,(concat noncontinued-line-end
			(c-lang-const c-opt-cpp-prefix)
			"\\(import\\|include\\)\\>\\s *\\(<[^>\n\r]*>?\\)")
	       ,(+ ncle-depth 2) font-lock-string-face)

	      ;; #define.
	      (,(concat
		 noncontinued-line-end
		 (c-lang-const c-opt-cpp-prefix)
		 "define\\s +"
		 (concat "\\("		; 1
			 ;; Macro with arguments - a "function".
			 "\\(" (c-lang-const c-symbol-key) "\\)\(" ; 2
			 "\\|"
			 ;; Macro without arguments - a "variable".
			 "\\(" (c-lang-const c-symbol-key) ; 3 + c-sym-key-dep
			 "\\)\\(\\s \\|$\\)"
			 "\\)"))
	       (,(+ 2 ncle-depth)
		font-lock-function-name-face nil t)
	       (,(+ 3 ncle-depth (c-lang-const c-symbol-key-depth))
		font-lock-variable-name-face nil t))

	      ;; Fontify symbol names in #elif or #if ... defined preprocessor
	      ;; directives.
	      (eval . (list
		       ,(concat noncontinued-line-end
				(c-lang-const c-opt-cpp-prefix)
				"\\(if\\|elif\\)\\>")
		       (list
			,(concat "\\<\\(" ; 1
				 ;; Don't use regexp-opt here to avoid the
				 ;; depth hazzle.  As it happens, it currently
				 ;; wouldn't have any effect anyway.
				 (mapconcat 'regexp-quote
					    (c-lang-const c-cpp-defined-fns)
					    "\\|")
				 "\\)\\>"
				 "\\s *\(?"
				 "\\(" (c-lang-const c-symbol-key) "\\)?") ; 2
			nil nil
			(list 1 c-preprocessor-face)
			'(,(+ 2 ncle-depth)
			  font-lock-variable-name-face nil t))))

	      ;; Fontify symbols after #ifdef and #ifndef.
	      (,(concat noncontinued-line-end
			(c-lang-const c-opt-cpp-prefix)
			"ifn?def\\s +\\(" (c-lang-const c-symbol-key) "\\)")
	       ,(1+ ncle-depth) font-lock-variable-name-face)

	      ;; Fontify the directive names.
	      (,(c-make-font-lock-search-function
		 (concat noncontinued-line-end
			 "\\("
			 (c-lang-const c-opt-cpp-prefix)
			 "[a-z]+"
			 "\\)")
		 `(,(1+ ncle-depth) c-preprocessor-face t)))
	      )))

      ,@(when (c-major-mode-is 'pike-mode)
	  `((eval . (list "\\`#![^\n\r]*"
			  0 c-preprocessor-face))))

      ;; Make hard spaces visible through an inverted `c-invalid-face'.
      (eval . (list
	       "\240"
	       0 (progn
		   (unless (c-face-name-p 'c-nonbreakable-space-face)
		     (c-make-inverse-face c-invalid-face
					  'c-nonbreakable-space-face))
		   'c-nonbreakable-space-face)))
      ))

(defun c-font-lock-labels (limit)
  ;; Fontify all the declarations from the point to LIMIT.  Assumes
  ;; that strings and comments have been fontified already.  This
  ;; function does both the search and the fontification itself to
  ;; avoid differences between the (X)Emacs flavors in where they
  ;; continue the search after a match.
  ;;
  ;; This function can make hidden buffer changes, but the font-lock
  ;; context covers that.

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
		 (c-put-font-lock-face (match-beginning 0) (match-end 0)
				       c-label-face))))))))

(defun c-font-lock-identifier-list (limit face)
  ;; Fontify a comma separated list of identifiers with FACE.  Any
  ;; highlighted keywords in front of each identifier are skipped over.
  ;; The list is taken to end when an identifier isn't followed by a
  ;; comma.

  (save-restriction
    (narrow-to-region (point-min) limit)

    (c-fontify-types-and-refs
	(id-end
	 ;; If FACE is `font-lock-type-face' then all things we encounter
	 ;; in the list are types.
	 (c-promote-possible-types t)
	 ;; The font-lock package in Emacs is known to clobber this.
	 (parse-sexp-lookup-properties t))

      (while (and
	      (progn
		(c-forward-syntactic-ws)
		(not (eobp)))

	      ;; Try to move forward over a type or name and fontify it.
	      (if (save-restriction
		    ;; Widen temporarily so that we don't trip up
		    ;; on a limit set in an odd place.
		    (widen)
		    (if (eq face 'font-lock-type-face)
			(c-forward-type)
		      (when (c-forward-name)
			(setq id-end (point))
			(when (c-simple-skip-symbol-backward)
			  (c-put-font-lock-face (point) id-end face))
			(goto-char id-end)
			t)))
		  ;; If it worked, check for and skip over a following comma.
		  (progn
		    (c-forward-syntactic-ws)
		    (when (eq (char-after) ?,)
		      (forward-char)
		      t))

		;; If it failed, check if we're on a keyword and skip
		;; over it in that case.  It's this way to handle that
		;; a name might start with a keyword.
		(when (eq (get-text-property (point) 'face)
			  'font-lock-keyword-face)
		  (goto-char (next-single-property-change
			      (point) 'face nil limit))
		  t)))))))

(c-lang-defconst c-basic-matchers-before
  "Font lock matchers for basic keywords, labels, references and various
other easily recognizable things that should be fontified before generic
casts and declarations are fontified.  Used on level 2 and higher."

  ;; Note: In C++, `c-font-lock-declarations' assumes that no matcher
  ;; here sets `font-lock-type-face'.

  t `(;; Fontify keyword constants.
      ,@(when (c-lang-const c-constant-kwds)
	  (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
	    (if (c-major-mode-is 'pike-mode)
		;; No symbol is a keyword after "->" in Pike.
		`((,(concat "\\(\\=\\|\\(\\=\\|[^-]\\)[^>]\\)"
			    "\\<\\(" re "\\)\\>")
		   3 font-lock-constant-face))
	      `((,(concat "\\<\\(" re "\\)\\>")
		 1 font-lock-constant-face)))))

      ;; Fontify all keywords except the primitive types.
      ,(if (c-major-mode-is 'pike-mode)
	   ;; No symbol is a keyword after "->" in Pike.
	   `(,(concat "\\(\\=\\|\\(\\=\\|[^-]\\)[^>]\\)"
		      "\\<" (c-lang-const c-regular-keywords-regexp))
	     3 font-lock-keyword-face)
	 `(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
	   1 font-lock-keyword-face))

      ;; Fontify leading identifiers in fully qualified names like
      ;; "foo::bar" in languages that supports such things.
      ,@(when (c-lang-const c-opt-identifier-concat-key)
	  `((,(byte-compile
	       ;; Must use a function here since we match longer
	       ;; than we want to move before doing a new search.
	       ;; This is not necessary for XEmacs >= 20 since it
	       ;; restarts the search from the end of the first
	       ;; highlighted submatch (something that causes
	       ;; problems in other places).
	       `(lambda (limit)
		  (while (re-search-forward
			  ,(concat "\\(\\<" ; 1
				   "\\(" (c-lang-const c-symbol-key) "\\)" ; 2
				   "[ \t\n\r\f\v]*"
				   (c-lang-const c-opt-identifier-concat-key)
				   "[ \t\n\r\f\v]*"
				   "\\)"
				   (if (c-major-mode-is 'c++-mode)
				       ;; Handle destructors and member
				       ;; pointers.
				       "\\([~*][ \t\n\r\f\v]*\\)?"
				     "")
				   (c-lang-const c-symbol-start))
			  limit t)
		    (unless (progn
			      (goto-char (match-beginning 0))
			      (c-skip-comments-and-strings limit))
		      (or (get-text-property (match-beginning 2) 'face)
			  (c-put-font-lock-face (match-beginning 2)
						(match-end 2)
						font-lock-reference-face))
		      (goto-char (match-end 1)))))))))

      ;; Fontify labels in languages that supports them.
      ,@(when (c-lang-const c-label-key)

	  `(;; Fontify goto targets and case labels.  This
	    ;; deliberately fontifies only a single identifier or a
	    ;; signed integer as a label; all other forms are
	    ;; considered to be expressions and thus fontified as
	    ;; such (i.e. not at all).

	    ;; (Got three different interpretation levels here,
	    ;; which makes it a bit complicated: 1) The backquote
	    ;; stuff is expanded when compiled or loaded, 2) the
	    ;; eval form is evaluated at font-lock setup (to
	    ;; substitute c-label-face correctly), and 3) the
	    ;; resulting structure is interpreted during
	    ;; fontification.)
	    (eval
	     . ,(let* ((c-before-label-re
			(c-make-keywords-re nil
			  (c-lang-const c-before-label-kwds)))
		       (identifier-offset
			(+ (c-regexp-opt-depth c-before-label-re)
			   3))
		       (integer-offset
			(+ identifier-offset
			   (c-regexp-opt-depth (c-lang-const c-identifier-key))
			   1)))

		  `(list
		    ,(concat
		      "\\<\\("
		      c-before-label-re
		      "\\)\\>"
		      "\\s *"
		      "\\("
		      ;; Match a (simple) qualified identifier,
		      ;; i.e. we don't bother with `c-forward-name'.
		      ;; We highlight the last symbol in it as a
		      ;; label.
		      "\\(" (c-lang-const ; identifier-offset
			     c-identifier-key) "\\)"
		      "\\|"
		      ;; Match an integer.
		      "\\(-?[0-9]+\\)"	; integer-offset
		      (if (c-major-mode-is 'pike-mode)
			  ;; Pike allows integer ranges.
			  (concat
			   "\\s *\\(\\.\\.\\s *\\(-?[0-9]+\\)?\\)?"
			   "\\|\\.\\.\\s *\\(-?[0-9]+\\)")
			"")
		      "\\)"
		      "\\s *[:;]")

		    ,@(mapcar
		       (lambda (submatch)
			 `(list ,(+ identifier-offset submatch)
				c-label-face nil t))
		       (c-lang-const c-identifier-last-sym-match))

		    (list ,integer-offset c-label-face nil t)

		    ,@(when (c-major-mode-is 'pike-mode)
			`((list ,(+ integer-offset 2) c-label-face nil t)
			  (list ,(+ integer-offset 3) c-label-face nil t)))
		    )))

	    ;; Fontify normal labels.
	    c-font-lock-labels))

      ,@(when (c-major-mode-is 'pike-mode)
	  `(;; Constant declarations in Pike lacks a type, so special
	    ;; treatment is necessary before the normal declaration
	    ;; fontification is done.
	    (,(concat "\\<constant\\>"
		      (c-lang-const c-syntactic-ws)
		      "\\(" (c-lang-const c-symbol-key) "\\)"
		      (c-lang-const c-syntactic-ws)
		      "=")
	     ,(+ (c-lang-const c-syntactic-ws-depth)
		 1)
	     font-lock-variable-name-face)

	    ;; Handle the identifier after "inherit" as a type.
	    ,(let ((identifier-offset
		    (1+ (c-lang-const c-syntactic-ws-depth))))
	       `(,(byte-compile
		   `(lambda (limit)
		      (when (re-search-forward
			     ,(concat "\\<inherit\\>"
				      (c-lang-const c-syntactic-ws)
				      "\\(" ; identifier-offset
				      (c-lang-const c-identifier-key)
				      "\\)")
			     limit t)
			;; Register the type.
			(save-match-data
			  (c-add-type (match-beginning ,identifier-offset)
				      (match-end ,identifier-offset)))
			t)))
		 ,@(mapcar
		    (lambda (submatch)
		      `(,(+ identifier-offset submatch)
			font-lock-type-face nil t))
		    (c-lang-const c-identifier-last-sym-match))))
	    ))
      ))

(defun c-font-lock-c++-templates (limit)
  ;; Fontify types and references in names containing template
  ;; arglists in C++.  This will also fontify cases like normal
  ;; function calls on the form "foo (a < b, c > d)", but
  ;; `c-font-lock-declarations' will undo that later.

  (c-fontify-types-and-refs (id-start id-end pos)
    (while (and (< (point) limit)
		(re-search-forward (cc-eval-when-compile
				     (concat "\\("
					     (c-lang-const c-symbol-key c++)
					     "\\)"
					     (c-lang-const c-syntactic-ws c++)
					     "<"))
				   limit t))
      (setq id-start (match-beginning 1)
	    id-end (match-end 1)
	    pos (point))

      (goto-char (match-beginning 0))
      (unless (c-skip-comments-and-strings limit)

	(goto-char (1- pos))
	(when (c-forward-c++-template-arglist)
	  (unless (get-text-property id-start 'face)
	    (c-forward-syntactic-ws)
	    (if (looking-at "::")
		(c-put-font-lock-face id-start id-end
				      font-lock-reference-face)
	      (c-put-font-lock-face id-start id-end
				    'font-lock-type-face))))))))

(defun c-font-lock-declarators (limit list types)
  ;; Assuming the point is in the syntactic whitespace before the
  ;; first declarator in a declaration, fontify it.  If LIST is
  ;; non-nil, fontify also all following declarators in a comma
  ;; separated list (e.g.  "foo" and "bar" in "int foo = 17, bar;").
  ;; Stop at LIMIT.  If TYPES is non-nil, fontify all identifiers as
  ;; types.

  ;;(message "c-font-lock-declarators from %s to %s" (point) limit)
  (c-forward-syntactic-ws limit)
  (c-fontify-types-and-refs
      ((pos (point)) next-pos id-start id-end
       paren-depth
       id-face got-init
       ;; The font-lock package in Emacs is known to clobber this.
       (parse-sexp-lookup-properties t))

    (while (and
	    pos
	    (< (point) limit)

	    (let (got-identifier)
	      (setq paren-depth 0)
	      ;; Skip over type decl prefix operators.  (Note similar
	      ;; code in `c-font-lock-declarations'.)
	      (while (and (looking-at c-type-decl-prefix-key)
			  (if (and (c-major-mode-is 'c++-mode)
				   (match-beginning 2))
			      ;; If the second submatch matches in C++ then
			      ;; we're looking at an identifier that's a
			      ;; prefix only if it specifies a member pointer.
			      (progn
				(setq id-start (point))
				(c-forward-name)
				(if (looking-at "\\(::\\)")
				    ;; We only check for a trailing "::" and
				    ;; let the "*" that should follow be
				    ;; matched in the next round.
				    t
				  ;; It turned out to be the real identifier,
				  ;; so flag that and stop.
				  (setq got-identifier t)
				  nil))
			    t))
		(if (eq (char-after) ?\()
		    (progn
		      (setq paren-depth (1+ paren-depth))
		      (forward-char))
		  (goto-char (match-end 1)))
		(c-forward-syntactic-ws))

	      ;; If we didn't pass the identifier above already, do it now.
	      (unless got-identifier
		(setq id-start (point))
		(c-forward-name))
	      (setq id-end (point))

	      (/= id-end pos))

	    ;; Skip out of the parens surrounding the identifier.
	    (or (= paren-depth 0)
		(c-safe (goto-char (scan-lists (point) 1 paren-depth))))

	    ;; Search syntactically to the end of the declarator
	    ;; (";", ",", ")", eob etc) or to the beginning of an
	    ;; initializer or function prototype ("=" or "\\s\(").
	    (c-syntactic-re-search-forward
	     "[;,\{\[\)]\\|\\'\\|\\(=\\|\\(\\s\(\\)\\)" limit t))

      (setq next-pos (match-beginning 0)
	    id-face (if (match-beginning 2)
			'font-lock-function-name-face
		      'font-lock-variable-name-face)
	    got-init (match-beginning 1))

      (if types
	  ;; Register and fontify the identifer as a type.
	  (let ((c-promote-possible-types t))
	    (goto-char id-start)
	    (c-forward-type))
	;; Fontify the last symbol in the identifier.
	(goto-char id-end)
	(when (and (c-simple-skip-symbol-backward)
		   (not (get-text-property (point) 'face)))
	  (c-put-font-lock-face (point) id-end id-face)))

      (goto-char next-pos)
      (setq pos nil)
      (when list
	;; Jump past any initializer or function prototype to see if
	;; there's a ',' to continue at.

	(cond ((eq id-face 'font-lock-function-name-face)
	       ;; Skip a parenthesized initializer (C++) or a function
	       ;; prototype.
	       (if (c-safe (c-forward-sexp 1) t)
		   (c-forward-syntactic-ws limit)
		 (goto-char limit)))

	      (got-init
	       ;; Skip an initializer expression.
	       (if (c-syntactic-re-search-forward "[;,]" limit 'move t)
		   (backward-char)))

	      (t (c-forward-syntactic-ws limit)))

	;; If a ',' is found we set pos to the next declarator and iterate.
	(when (and (< (point) limit) (looking-at ","))
	  (forward-char)
	  (c-forward-syntactic-ws limit)
	  (setq pos (point)))))))

(defconst c-font-lock-maybe-type-faces
  ;; List of faces that might be put at the start of a type when
  ;; `c-font-lock-declarations' runs.  This needs to be evaluated to
  ;; ensure that face name aliases in Emacs are resolved
  ;; (`font-lock-reference-face' might be an alias for
  ;; `font-lock-constant-face').
  (list nil font-lock-type-face font-lock-reference-face))

(defvar c-fl-decl-syntactic-pos nil)
(make-variable-buffer-local 'c-fl-decl-syntactic-pos)
(defvar c-fl-decl-match-pos nil)
(make-variable-buffer-local 'c-fl-decl-match-pos)
;; Used in `c-font-lock-declarations' to cache the search done for the
;; first declaration in the last call.  When that function starts, it
;; needs to back up over syntactic whitespace to find the last place
;; `c-decl-prefix-re' matches before the region being fontified.  That
;; can sometimes cause a search back and forth over a quite large
;; region of comments and macros, which would be repeated for each
;; changed character since font-lock refontifies the current line for
;; each change.  Thus it's worthwhile to cache the first such search.
;;
;; Note that we exploit the fact that the start position to
;; `c-font-lock-declarations' will always be less or equal to the
;; lowest position where the buffer has changed, so we know any cached
;; positions less than that are still valid.
;;
;; `c-fl-decl-syntactic-pos' is a syntactically relevant position in
;; the syntactic whitespace less or equal to some start position.
;;
;; `c-fl-decl-match-pos' is the match position if `c-decl-prefix-re'
;; matched before the syntactic whitespace at
;; `c-fl-decl-syntactic-pos', or nil if there's no such match.

(eval-when-compile
  ;; Macros used inside `c-font-lock-declarations'.  These ought to be
  ;; defsubsts or perhaps even defuns, but they contain lots of free
  ;; variables that refer to things inside `c-font-lock-declarations'.

  (defmacro c-fl-decl-prefix-search ()
    '(while (and (setq match (re-search-forward c-decl-prefix-re nil 'move))
		 (if (memq (get-text-property (setq match-pos
						    (1- (match-end 1)))
					      'face)
			   '(font-lock-comment-face font-lock-string-face))
		     t
		   ;; Skip forward past comments only, to set the position to
		   ;; continue at, so we don't skip macros.
		   (goto-char (match-end 1))
		   (c-forward-comments)
		   (setq continue-pos (point))
		   nil))
       ;; Search again if the match is within a comment or a string
       ;; literal.
       (goto-char (next-single-property-change
		   match-pos 'face nil (point-max)))))

  (defmacro c-fl-shift-type-backward ()
    ;; `c-font-lock-declarations' can consume an arbitrary length list of
    ;; types when parsing a declaration, which means that it sometimes
    ;; consumes the identifier in the declaration as a type.  This is used to
    ;; "backtrack" and make the last type be treated as an identifier instead.
    '(setq identifier-type at-type
	   identifier-start type-start
	   identifier-end type-end
	   at-type (if (eq prev-at-type 'prefix)
		       t
		     prev-at-type)
	   type-start (if prev-at-type
			  prev-type-start
			start-pos)
	   type-end (if prev-at-type
			prev-type-end
		      start-pos)
	   prev-at-type nil
	   got-parens nil
	   got-identifier t
	   got-suffix t
	   paren-depth 0)))

(defun c-font-lock-declarations (limit)
  ;; Fontify all the declarations and casts from the point to LIMIT.
  ;; Assumes that strings and comments have been fontified already.
  ;;
  ;; This function can make hidden buffer changes, but the font-lock
  ;; context covers that.

  ;;(message "c-font-lock-declarations search from %s to %s" (point) limit)

  ;; Clear the list of found types if we start from the start of the
  ;; buffer, to make it easier to get rid of misspelled types and
  ;; variables that has gotten recognized as types in malformed code.
  (when (bobp)
    (c-clear-found-types))

  (save-restriction
    (let ((start-pos (point))
	  c-disallow-comma-in-template-arglists
	  ;; The result of the last search for `c-decl-prefix-re'.
	  match
	  ;; The position of the last token matched by the last
	  ;; `c-decl-prefix-re' match.  0 for the implicit match at bob.
	  match-pos
	  ;; The position to continue searching at.
	  continue-pos
	  ;; The position of the last "real" token we've stopped at.  This can
	  ;; be greater than `continue-pos' when we get hits inside macros.
	  (token-pos 0)
	  ;; Nonzero if the `c-decl-prefix-re' match is in an arglist context,
	  ;; as opposed to a statement-level context.  The major difference is
	  ;; that "," works as declaration delimiter in an arglist context,
	  ;; whereas it only separates declarators in the same declaration in
	  ;; a statement context.  If it's nonzero then the value is the
	  ;; matched char, e.g. ?\( or ?,.
	  arglist-match
	  ;; Set to the result of `c-forward-type'.
	  at-type
	  ;; These record the start and end of the type or possible type found
	  ;; by `c-forward-type'.
	  type-start type-end
	  ;; These store `at-type', `type-start' and `type-end' of the
	  ;; identifier before the one in those variables.  The previous
	  ;; identifier might turn out to be the real type in a declaration if
	  ;; the last one has to be the declarator in it.  If `prev-at-type'
	  ;; is nil then the other variables have undefined values.
	  prev-at-type prev-type-start prev-type-end
	  ;; Whether we've found a declaration or a cast.  We might know this
	  ;; before we've found the type in it.
	  at-decl-or-cast
	  ;; Set if we've found a "typedef" specifier.  The identifiers in the
	  ;; declaration are then fontified as types.
	  at-typedef
	  ;; The position of the next token after the closing paren of the
	  ;; last fontified cast.
	  last-cast-end
	  ;; The same for the currently investigated cast.
	  cast-end
	  ;; The maximum of the end positions of all the checked type decl
	  ;; expressions in the successfully identified declarations.  The
	  ;; position might be either before or after the syntactic whitespace
	  ;; following the last token in the type decl expression.
	  (max-type-decl-end 0)
	  ;; Same as `max-type-decl-end', but used when we're before
	  ;; `token-pos'.
	  (max-type-decl-end-before-token 0)
	  ;; The end position of the last entered macro.
	  (macro-end -1)
	  ;; Allow recording of identifier ranges in `c-forward-type' etc for
	  ;; later fontification.  Not using `c-fontify-types-and-refs' here
	  ;; since the ranges should be fontified selectively only when a
	  ;; declaration or cast has been successfully recognized.
	  c-record-type-identifiers
	  c-record-ref-identifiers
	  ;; The font-lock package in Emacs is known to clobber this.
	  (parse-sexp-lookup-properties t))

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
      (narrow-to-region
       (point-min)
       (save-excursion
	 ;; Narrow after any operator chars following the limit though, since
	 ;; those characters can be useful in recognizing a declaration (in
	 ;; particular the '{' that opens a function body after the header).
	 (goto-char limit)
	 (skip-chars-forward "^_a-zA-Z0-9$")
	 (point)))

      ;; Must back up a bit since we look for the end of the previous
      ;; statement or declaration, which is earlier than the first
      ;; returned match.
      (let ((prop (get-text-property (point) 'face)))
	(when (memq prop '(font-lock-comment-face font-lock-string-face))
	  ;; But first we need to move to a syntactically relevant
	  ;; position.  Can't use backward movement on the face property
	  ;; since the font-lock package might not have fontified the
	  ;; comment or string all the way to the start.
	  (goto-char (next-single-property-change (point) 'face nil limit))
	  (when (eq prop 'font-lock-comment-face)
	    ;; Back up over the comment to compare to
	    ;; `c-fl-decl-syntactic-pos'.  There's no use in doing
	    ;; something similar for string literals since even if
	    ;; `c-decl-prefix-re' matches directly before it, it
	    ;; couldn't be the start of a declaration.
	    (c-backward-single-comment))))

      ;; Must back out of any macro so that we don't miss any declaration that
      ;; could follow after it, unless the limit is inside the macro.  We only
      ;; check that for the current line to save some time; it's enough for
      ;; the by far most common case when font-lock refontifies the current
      ;; line only.
      (when (save-excursion
	      (and (= (forward-line 1) 0)
		   (or (< (point) limit)
		       (progn
			 (backward-char)
			 (not (eq (char-before) ?\\))))))
	(c-beginning-of-macro))

      ;; Clear the c-fl-decl- cache if it applied further down.
      (and c-fl-decl-syntactic-pos
	   (< start-pos c-fl-decl-syntactic-pos)
	   (setq c-fl-decl-syntactic-pos nil))

      (let ((syntactic-pos (point)))
	(c-backward-syntactic-ws c-fl-decl-syntactic-pos)

	;; If we hit `c-fl-decl-syntactic-pos' and `c-fl-decl-match-pos' is
	;; set then we install the cached values.  If we hit
	;; `c-fl-decl-syntactic-pos' and `c-fl-decl-match-pos' is nil then we
	;; know there's no decl prefix in the whitespace before
	;; `c-fl-decl-syntactic-pos' and so we can continue the search from
	;; this point. If we didn't hit `c-fl-decl-syntactic-pos' then we're
	;; now in the right spot to begin searching anyway.
	(if (and (eq (point) c-fl-decl-syntactic-pos)
		 c-fl-decl-match-pos)
	    (setq match t
		  match-pos c-fl-decl-match-pos
		  continue-pos syntactic-pos)
	  (setq c-fl-decl-syntactic-pos syntactic-pos)

	  (if (bobp)
	      ;; Always consider bob a match to get the first declaration in
	      ;; the file.  Do this separately instead of letting
	      ;; `c-decl-prefix-re' match bob, so that it always can consume
	      ;; at least one character to ensure that we won't get stuck in
	      ;; an infinite loop.
	      (progn (c-forward-comments)
		     (setq match t
			   match-pos 0
			   continue-pos (point)))
	    (backward-char)
	    (or (bobp) (backward-char))
	    (c-fl-decl-prefix-search))

	  ;; Advance `continue-pos' if we got a hit before the start
	  ;; position.  The earliest position that could affect after
	  ;; the start position is the char before the preceding
	  ;; comments.
	  (when (and continue-pos (< continue-pos start-pos))
	    (goto-char syntactic-pos)
	    (c-backward-comments)
	    (or (bobp) (backward-char))
	    (setq continue-pos (max continue-pos (point))))

	  ;; If we got a match it's always outside macros so advance
	  ;; to the next token and set `token-pos'.  The loop below
	  ;; will later go back using `continue-pos' to fix macros
	  ;; inside the syntactic ws.
	  (when (and match (< (point) syntactic-pos))
	    (goto-char syntactic-pos)
	    (c-forward-syntactic-ws)
	    (and continue-pos
		 (< continue-pos (point))
		 (setq token-pos (point))))

	  (setq c-fl-decl-match-pos (and match-pos
					 (< match-pos start-pos)
					 match-pos))))

      (while (progn
	       (while (and
		       match

		       (or
			;; Kludge to filter out matches on "[" in C++
			;; (see comment in `c-decl-prefix-re').
			(and (c-major-mode-is 'c++-mode)
			     (eq (char-after match-pos) ?\[))

			(progn
			  ;; Set `arglist-match'.  We look at whether the
			  ;; match token is a statement-level one since the
			  ;; tokens that can start arglists vary more between
			  ;; the languages.  Look for ":" for the sake of
			  ;; C++-style protection labels.
			  (setq arglist-match (char-after match-pos))
			  (when (memq arglist-match '(?{ ?} ?\; ?:))
			    (setq arglist-match nil))

			  ;; If `continue-pos' is less or equal to
			  ;; `token-pos', we've got a hit inside a macro
			  ;; that's in the syntactic whitespace before the
			  ;; last "real" declaration we've checked.  If
			  ;; they're equal we've arrived at the declaration a
			  ;; second time, so there's nothing to do.
			  (= continue-pos token-pos))

			(let (prop)
			  ;; If `continue-pos' is less than `token-pos' we're
			  ;; still searching macros in the syntactic
			  ;; whitespace, so we need only to skip comments and
			  ;; not macros, since they can't be nested.
			  (when (> continue-pos token-pos)
			    (c-forward-syntactic-ws)
			    (setq token-pos (point)))

			  ;; Continue if the following token isn't some kind
			  ;; of symbol or keyword that can start a
			  ;; declaration, or if it's fontified as something
			  ;; else besides a type or reference (which might
			  ;; lead a type) already.
			  (when (or
				 (= (point) limit)
				 (not (looking-at c-identifier-start))
				 (if (eq (setq prop (get-text-property
						     (point) 'face))
					 'font-lock-keyword-face)
				     (looking-at c-not-decl-init-keywords)
				   (not (memq prop
					      c-font-lock-maybe-type-faces))))
			    (goto-char continue-pos)
			    t))
			))

		 (c-fl-decl-prefix-search))
	       (< (point) limit))

	(catch 'false-alarm
	  ;; Narrow to the end of the macro if we got a hit inside one.
	  (when (/= match-pos 0)
	    (save-excursion
	      (goto-char match-pos)
	      (when (>= match-pos macro-end)
		(setq macro-end
		      (if (save-excursion (and (c-beginning-of-macro)
					       (< (point) match-pos)))
			  (progn (c-end-of-macro)
				 (point))
			-1)))))
	  (when (/= macro-end -1)
	    (when (<= macro-end (point))
	      (setq macro-end -1)
	      (throw 'false-alarm t))
	    (narrow-to-region (point-min) macro-end))

	  (setq at-type nil
		at-decl-or-cast nil
		at-typedef nil
		c-record-type-identifiers t
		c-record-ref-identifiers nil
		;; `start-pos' is used below to point to the start of the
		;; first type, i.e. after any leading specifiers.  It might
		;; also point at the beginning of the preceding syntactic
		;; whitespace.
		start-pos (point)
		;; If we're in an arglist context we normally don't want to
		;; recognize comma in nested template arglists since those
		;; commas could be part of our own arglist.  However, we allow
		;; it when our arglist is known to contain declarations.
		c-disallow-comma-in-template-arglists
		(and arglist-match
		     (>= (point) (if (< (point) token-pos)
				     max-type-decl-end-before-token
				   max-type-decl-end))))

	  ;; Check for a type, but be prepared to skip over leading specifiers
	  ;; like "static".  We treat any symbols as specifiers here, to cope
	  ;; with macros like __INLINE__ or anything else that might be in
	  ;; front of declarations.
	  (while (let ((start (point))
		       (res (c-forward-type)))

		   (cond (res
			  ;; Found a known or possible type or a prefix of a
			  ;; known type.

			  (when at-type
			    ;; Got two identifiers with nothing but whitespace
			    ;; between them.  That can only happen in
			    ;; declarations.
			    (setq at-decl-or-cast t)

			    (when (eq at-type 'found)
			      ;; If the previous identifier is a found type we
			      ;; record it as one; it might be some sort of
			      ;; alias for a prefix like "unsigned".
			      (save-excursion
				(goto-char type-start)
				(let ((c-promote-possible-types t))
				  (c-forward-type)))))

			  (setq prev-at-type at-type
				prev-type-start type-start
				prev-type-end type-end
				at-type res
				type-start start
				type-end (point))

			  ;; If the type isn't known we continue so that we'll
			  ;; jump over all specifiers and type identifiers.
			  ;; The reason to do this for a known type prefix is
			  ;; to make things like "unsigned INT16" work.
			  (not (eq res t)))

			 ((looking-at c-specifier-key)
			  ;; Found a known specifier keyword.  Can occur in
			  ;; both declarations and casts, but we don't set
			  ;; `at-decl-or-cast' here to avoid flashing types
			  ;; prematurely in declarations as they're being
			  ;; written.
			  (setq start (or (match-end 1) (match-end 0)))
			  (when (looking-at c-typedef-specifier-key)
			    (setq at-typedef t))

			  (if (and (c-major-mode-is 'c++-mode)
				   (looking-at "template\\>"))
			      ;; Special case for a C++ template prefix.
			      (progn
				(goto-char (match-end 0))
				(c-forward-syntactic-ws)
				(unless (and (eq (char-after) ?<)
					     (c-forward-c++-template-arglist))
				  (throw 'false-alarm t))
				(c-forward-syntactic-ws))
			    (goto-char start))
			  (setq start-pos (point)))))

	    (c-forward-syntactic-ws))

	  (cond ((eq at-type 'prefix)
		 ;; A prefix type is itself a known type when it's not
		 ;; followed by another type.
		 (setq at-type t))
		((not at-type)
		 ;; Got no type but set things up to continue anyway to handle
		 ;; the various cases when a declaration doesn't start with a
		 ;; type.
		 (setq type-end start-pos)))

	  ;; Check for and step over a type decl expression after the thing
	  ;; that is or might be a type.  We can skip this if we know we're in
	  ;; a declaration and don't have several possible positions for the
	  ;; type.  Normally a known type ensures that, but in a typedef the
	  ;; known type could be in the identifier position, e.g. if it's
	  ;; matched by `*-font-lock-extra-types'.
	  (if (and at-decl-or-cast (eq at-type t) (not at-typedef))
	      (progn
		;; Go to the end of the first type decl expression.  We know
		;; there's an identifier after the type.  If there's any
		;; parenthesis after the identifier we jump over that too
		;; under the assumption that it's a function decl arglist.
		(goto-char type-end)
		(when (c-safe (c-forward-sexp) t)
		  (c-forward-syntactic-ws)
		  (when (eq (char-after) ?\()
		    (c-safe (c-forward-sexp)))))

	    (if (catch 'at-decl-or-cast
		  (goto-char type-end)
		  (c-forward-syntactic-ws)
		  (let ((start (point)) (paren-depth 0) pos
			;; True if there's a non-open-paren match of
			;; `c-type-decl-prefix-key'.
			got-prefix
			;; True if the declarator is surrounded by a
			;; parenthesis pair.
			got-parens
			;; True if the first match of `c-type-decl-prefix-key'
			;; is before any open parenthesis that surrounds the
			;; declarator.
			got-prefix-before-parens
			;; True if there is an identifier in the declarator.
			got-identifier
			;; True if there's a non-close-paren match of
			;; `c-type-decl-suffix-key'.
			got-suffix
			;; The earlier values of `at-type', `type-start' and
			;; `type-end' if we've shifted the type backwards.
			identifier-type identifier-start identifier-end)

		    ;; Skip over type decl prefix operators.  (Note
		    ;; similar code in `c-font-lock-declarators'.)
		    (while (and (looking-at c-type-decl-prefix-key)
				(if (and (c-major-mode-is 'c++-mode)
					 (match-beginning 2))
				    ;; If the second submatch matches in C++
				    ;; then we're looking at an identifier
				    ;; that's a prefix only if it specifies a
				    ;; member pointer.
				    (when (setq got-identifier
						(c-forward-name))
				      (if (looking-at "\\(::\\)")
					  ;; We only check for a trailing "::"
					  ;; and let the "*" that should
					  ;; follow be matched in the next
					  ;; round.
					  (progn (setq got-identifier nil) t)
					;; It turned out to be the real
					;; identifier, so stop.
					nil))
				  t))
		      (if (eq (char-after) ?\()
			  (progn
			    (setq paren-depth (1+ paren-depth))
			    (forward-char))
			(unless got-prefix-before-parens
			  (setq got-prefix-before-parens (= paren-depth 0)))
			(setq got-prefix t)
			(goto-char (match-end 1)))
		      (c-forward-syntactic-ws))
		    (setq got-parens (> paren-depth 0))

		    ;; Skip over an identifier.
		    (when (or got-identifier
			      (and (looking-at c-identifier-start)
				   (setq got-identifier (c-forward-name))))
		      (c-forward-syntactic-ws))

		    ;; Skip over type decl suffix operators.
		    (while (if (looking-at c-type-decl-suffix-key)
			       (if (eq (char-after) ?\))
				   (when (> paren-depth 0)
				     (setq paren-depth (1- paren-depth))
				     (forward-char)
				     t)
				 (if (save-match-data (looking-at "\\s\("))
				     (and (c-safe (c-forward-sexp 1) t)
					  (setq got-suffix t))
				   (goto-char (match-end 1))
				   (setq got-suffix t)))
			     ;; No suffix matched.  We might have matched the
			     ;; identifier as a type and the open paren of a
			     ;; function arglist as a type decl prefix.  In
			     ;; that case we should "backtrack": Reinterpret
			     ;; the last type as the identifier, move out of
			     ;; the arglist and continue searching for suffix
			     ;; operators.
			     ;;
			     ;; Do this even if there's no preceding type, to
			     ;; cope with old style function declarations in
			     ;; K&R C and (con|de)structors in C++.  That
			     ;; isn't applicable in an arglist context, though.
			     (when (and (= paren-depth 1)
					(not got-prefix)
					(not (eq at-type t))
					(or prev-at-type
					    (not arglist-match))
					(setq pos (c-up-list-forward (point)))
					(eq (char-before pos) ?\)))
			       (c-fl-shift-type-backward)
			       (goto-char pos)
			       t))
		      (c-forward-syntactic-ws))

		    ;; Now we've collected info about various characteristics
		    ;; of the construct we're looking at.  Below follows a
		    ;; decision tree based on that.  It's ordered to check
		    ;; more certain signs before less certain ones.

		    (when (or (= (point) start) (> paren-depth 0))
		      ;; We haven't found anything.
		      (throw 'at-decl-or-cast nil))

		    (if got-identifier
			(progn
			  (when (and at-type (not (or got-prefix got-parens)))
			    ;; Got another identifier directly after the type,
			    ;; so it's a declaration.
			    (throw 'at-decl-or-cast t))

			  (when (looking-at "=[^=]\\|\(")
			    ;; There's an initializer after the type decl
			    ;; expression so we know it's a declaration.
			    ;; (Checking for "(" here normally has no effect
			    ;; since it's probably matched as a suffix.
			    ;; That's often not a problem, however.)
			    (throw 'at-decl-or-cast t)))

		      (when (or (and (eq at-type t) (not prev-at-type))
				(and got-prefix got-suffix)
				(and got-parens got-prefix)
				(and got-parens got-suffix))
			;; Found no identifier.  If the type is known we know
			;; that there can't be any identifier somewhere else,
			;; and it's only in declarations in e.g. function
			;; prototypes and in casts that the identifier may be
			;; left out.
			;;
			;; Otherwise we require at least two of `got-prefix',
			;; `got-parens', and `got-suffix': `got-parens' only
			;; is not enough since it's probably an empty function
			;; call.  `got-suffix' only is not enough since it can
			;; build an ordinary expression together with the
			;; preceding identifier which we've taken as a type.
			;;
			;; However, we could actually accept on `got-prefix'
			;; only, but that can easily occur temporarily while
			;; writing an expression so we avoid that case anyway.
			;; We could do a better job if we knew the point when
			;; the fontification was invoked.
			(throw 'at-decl-or-cast t))

		      (when (and got-suffix
				 (not got-prefix)
				 (not got-parens)
				 prev-at-type)
			;; Got only a suffix and there are two identifiers
			;; before.  The second one is not the type afterall,
			;; so return nil to let the conditional below shift to
			;; the type in `prev-*'.
			(throw 'at-decl-or-cast nil))

		      (when (and got-parens
				 (not got-prefix)
				 (not arglist-match)
				 (not (eq at-type t)))
			;; Got an empty paren pair and a preceding type that
			;; probably really is the identifier.  Shift the type
			;; backwards to make the last one the identifier.
			;; This is analogous to the "backtracking" done inside
			;; the `c-type-decl-suffix-key' loop above.
			(c-fl-shift-type-backward)))

		    (when (and got-identifier
			       (not arglist-match)
			       (looking-at c-after-suffixed-type-decl-key)
			       (if (and got-parens
					(not got-prefix)
					(not got-suffix)
					(not (eq at-type t)))
				   ;; Shift the type backward in the case that
				   ;; there's a single identifier inside
				   ;; parens.  That can only occur in K&R
				   ;; style function declarations so it's more
				   ;; likely that it really is a function
				   ;; call.  Therefore we only do this after
				   ;; `c-after-suffixed-type-decl-key' has
				   ;; matched.
				   (progn (c-fl-shift-type-backward) t)
				 got-suffix))
		      ;; A declaration according to
		      ;; `c-after-suffixed-type-decl-key'.
		      (throw 'at-decl-or-cast t))

		    (unless (looking-at (if arglist-match "[,\)]" "[,;]"))
		      ;; If this is a declaration it should end here, so check
		      ;; for allowed separation tokens.
		      (throw 'at-decl-or-cast nil))

		    ;; If we get here we can't tell if this is a type decl or
		    ;; a normal expression by looking at it alone.  (That's
		    ;; under the assumption that normal expressions always can
		    ;; look like type decl expressions, which isn't really
		    ;; true but the cases where it doesn't hold are so
		    ;; uncommon (e.g. some placements of "const" in C++) it's
		    ;; not worth the effort to look for them.)

		    ;; It's a type decl expression if we know we're in a
		    ;; declaration, or if the preceding identifier is a known
		    ;; type.
		    (when (or at-decl-or-cast (memq at-type '(t found)))
		      (throw 'at-decl-or-cast t))

		    (when (and got-prefix-before-parens
			       got-identifier
			       (not arglist-match)
			       (not got-suffix))
		      ;; Got something like "foo * bar".  If we're not inside
		      ;; an arglist then it would be a meaningless expression
		      ;; since the result isn't used.  We therefore choose to
		      ;; recognize it as a declaration.  Do not allow a suffix
		      ;; since it could then be a function call.
		      (throw 'at-decl-or-cast t))

		    (when (and (c-major-mode-is 'c++-mode)
			       ;; In C++ we check if the identifier is a known
			       ;; type, since (con|de)structors use the class
			       ;; name as identifier.  We've always shifted
			       ;; over the identifier as a type and then
			       ;; backed up in this case.
			       identifier-type
			       (or (eq identifier-type 'found)
				   (and (eq (char-after identifier-start) ?~)
					;; `at-type' probably won't be 'found
					;; for destructors since the "~" is
					;; then part of the type name being
					;; checked against the list of known
					;; types, so do a check without that
					;; operator.
					(c-check-type (1+ identifier-start)
						      identifier-end))))
		      (throw 'at-decl-or-cast t))

		    ;; If we had a complete symbol table here (which rules out
		    ;; `c-found-types') we should return t due to the
		    ;; disambiguation rule (in at least C++) that anything
		    ;; that can be parsed as a declaration is a declaration.
		    ;; Now we're being more defensive and prefer to highlight
		    ;; things like "foo (bar);" as a declaration only if we're
		    ;; inside the type decl expression of an earlier
		    ;; recognized declaration.
		    (< (point) (if (< (point) token-pos)
				   max-type-decl-end-before-token
				 max-type-decl-end))))
		(setq at-decl-or-cast t)

	      (when prev-at-type
		;; Didn't find a type decl expression, but if we've passed two
		;; consecutive identifiers it's still a declaration - we only
		;; went a bit too far.
		(goto-char type-end)
		(setq at-type (if (eq prev-at-type 'prefix) t prev-at-type)
		      type-start prev-type-start
		      type-end prev-type-end
		      prev-at-type nil)

		;; We don't analyze a type decl expression as thoroughly at
		;; this point as we do above, so just jump over any following
		;; parenthesis under the assumption that it's a function
		;; decl arglist.
		(c-forward-syntactic-ws)
		(when (eq (char-after) ?\()
		  (c-safe (c-forward-sexp))))))

	  ;; Point is now after the type decl expression.

	  (cond
	   ;; Check for a cast.
	   ((save-excursion
	      (and
	       c-opt-cast-close-paren-key

	       ;; Should be the first type/identifier in a paren.
	       (memq arglist-match '(?\( ?\[))

	       ;; The closing paren should match `c-opt-cast-close-paren-key'.
	       (progn
		 (c-forward-syntactic-ws)
		 (looking-at c-opt-cast-close-paren-key))

	       ;; There should be a symbol or an expression open paren after
	       ;; it.
	       (progn
		 (forward-char)
		 (c-forward-syntactic-ws)
		 (setq cast-end (point))
		 (or (and (looking-at c-identifier-start)
			  (not (looking-at c-keywords-regexp)))
		     (looking-at "[\(\[]")))

	       ;; There should either be a cast before it or something that
	       ;; isn't an identifier or close paren.
	       (/= match-pos 0)
	       (progn
		 (goto-char match-pos)
		 (or (eq (point) last-cast-end)
		     (progn
		       (c-backward-syntactic-ws)
		       (or (bobp)
			   (and
			    (progn
			      (backward-char)
			      ;; Check for a word or symbol char first since
			      ;; `c-on-identifier' returns nil on keywords and
			      ;; a paren after a keyword is not a cast.
			      (not (looking-at "\\sw\\|\\s_\\|[\]\)]")))
			    (progn
			      (forward-char)
			      (not (c-on-identifier))))))))))

	    (setq last-cast-end cast-end)
	    (when (and at-type (not (eq at-type t)))
	      (let ((c-promote-possible-types t))
		(goto-char type-start)
		(c-forward-type))))

	    (at-decl-or-cast
	     ;; We're at a declaration.  Highlight the type and the following
	     ;; declarators.

	     ;; Set `max-type-decl-end' or `max-type-decl-end-before-token'
	     ;; under the assumption that we're after the first type decl
	     ;; expression in the declaration now.  That's not really true; we
	     ;; could also be after a parenthesized initializer expression in
	     ;; C++, but this is only used as a last resort to slant ambiguous
	     ;; expression/declarations, and overall it's worth the risk to
	     ;; occasionally fontify an expression as a declaration in an
	     ;; initializer expression compared to getting ambiguous things in
	     ;; normal function prototypes fontified as expressions.
	     (if (< (point) token-pos)
		 (setq max-type-decl-end-before-token
		       (max max-type-decl-end-before-token (point)))
	       (setq max-type-decl-end
		     (max max-type-decl-end (point))))

	     (when (and at-type (not (eq at-type t)))
	       (let ((c-promote-possible-types t))
		 (goto-char type-start)
		 (c-forward-type)))

	     (goto-char type-end)
	     (c-font-lock-declarators
	      (point-max)
	      (if arglist-match
		  ;; Should normally not fontify a list of declarators inside
		  ;; an arglist, but the first argument in the ';' separated
		  ;; list of a "for" statement is an exception.
		  (when (and (eq arglist-match ?\() (/= match-pos 0))
		    (save-excursion
		      (goto-char match-pos)
		      (c-backward-syntactic-ws)
		      (and (c-simple-skip-symbol-backward)
			   (looking-at c-paren-stmt-key))))
		t)
	      at-typedef))

	    (t
	     ;; False alarm.  Skip the fontification done below.
	     (throw 'false-alarm t)))

	  ;; A cast or declaration has been successfully identified, so do all
	  ;; the fontification of types and refs that's been recorded by the
	  ;; calls to `c-forward-type' and `c-forward-name' above.
	  (c-fontify-recorded-types-and-refs)
	  nil)

	(when c-disallow-comma-in-template-arglists
	  ;; Remove any incorrect template arglists that's been recognized on
	  ;; the preceding opportunistic check in `c-complex-decl-matchers'.
	  ;; The currently visited range will at least contain the incorrect
	  ;; type name in that case.  No other preceding matchers should have
	  ;; set `font-lock-type-face'.
	  (let ((end (point)) id-start id-end tmpl-end)
	    (goto-char start-pos)
	    (while (and
		    (< (point) end)
		    (setq id-start (text-property-any
				    (point) end 'face 'font-lock-type-face)))
	      (goto-char (setq id-end (next-single-property-change
				       id-start 'face nil end)))
	      (c-forward-syntactic-ws)
	      (when (and (eq (char-after) ?<)
			 (looking-at "\\s\(")
			 (progn
			   (setq tmpl-end
				 (save-excursion
				   (c-safe (c-forward-sexp) (1- (point)))))
			   (not (c-forward-c++-template-arglist))))
		(c-clear-char-syntax (point))
		(if tmpl-end (c-clear-char-syntax tmpl-end))
		(c-put-font-lock-face id-start id-end nil)))
	    (goto-char end)))

	(when (/= macro-end -1)
	  ;; Restore limits if we did macro narrowment above.
	  (narrow-to-region (point-min) limit))
	(goto-char continue-pos)
	(c-fl-decl-prefix-search)))
    nil))

(defun c-font-lock-c++-new (limit)
  ;; Assuming point is after a "new" keyword, fontify the type in the
  ;; allocation expression.  As usual, C++ takes the prize in coming
  ;; up with a hard to parse syntax. :P

  (save-excursion
    (catch 'false-alarm
      ;; A "new" keyword is followed by one to three expressions, where
      ;; the type is the middle one, and the only required part.
      (let (expr1-pos expr2-pos
	    ;; Enable recording of identifier ranges in `c-forward-type'
	    ;; etc for later fontification.  Not using
	    ;; `c-fontify-types-and-refs' here since the ranges should
	    ;; be fontified selectively only when an allocation
	    ;; expression is successfully recognized.
	    (c-record-type-identifiers t)
	    c-record-ref-identifiers)
	(c-forward-syntactic-ws)

	;; The first placement arglist is always parenthesized, if it
	;; exists.
	(when (eq (char-after) ?\()
	  (setq expr1-pos (1+ (point)))
	  (condition-case nil
	      (c-forward-sexp)
	    (scan-error (throw 'false-alarm t)))
	  (c-forward-syntactic-ws))

	;; The second expression is either a type followed by some "*" or
	;; "[...]" or similar, or a parenthesized type followed by a full
	;; identifierless declarator.
	(setq expr2-pos (1+ (point)))
	(cond ((eq (char-after) ?\())
	      ((let ((c-promote-possible-types t))
		 (c-forward-type)))
	      (t (setq expr2-pos nil)))

	(when expr1-pos
	  (cond
	   ((not expr2-pos)
	    ;; No second expression, so the first has to be a
	    ;; parenthesized type.
	    (goto-char expr1-pos)
	    (let ((c-promote-possible-types t))
	      (c-forward-type)))

	   ((eq (char-before expr2-pos) ?\()
	    ;; Got two parenthesized expressions, so we have to look
	    ;; closer at them to decide which is the type.  No need to
	    ;; handle `c-record-ref-identifiers' since all references
	    ;; has already been handled by other fontification rules.
	    (let (expr1-res expr2-res expr1-rec expr2-rec)

	      (goto-char expr1-pos)
	      (setq expr1-res (c-forward-type)
		    expr1-rec c-record-type-identifiers
		    c-record-type-identifiers t)
	      (c-forward-syntactic-ws)
	      (unless (looking-at
		       (cc-eval-when-compile
			 (concat (c-lang-const c-symbol-start c++)
				 "\\|[*:\)\[]")))
		;; There's something after the would-be type that can't
		;; be there, so this is a placement arglist.
		(setq expr1-res nil))

	      (goto-char expr2-pos)
	      (setq expr2-res (c-forward-type)
		    expr2-rec c-record-type-identifiers)
	      (c-forward-syntactic-ws)
	      (unless (looking-at
		       (cc-eval-when-compile
			 (concat (c-lang-const c-symbol-start c++)
				 "\\|[*:\)\[]")))
		;; There's something after the would-be type that can't
		;; be there, so this is an initialization expression.
		(setq expr2-res nil))
	      (when (and (eq (car (parse-partial-sexp (point) (point-max) -1))
			     -1)
			 (progn (c-forward-syntactic-ws)
				(eq (char-after) ?\()))
		;; If there's a third paren expression then the second
		;; one is the type, so demote the first match.
		(setq expr1-res nil))

	      ;; We fontify the most likely type, with a preference for
	      ;; the first argument since a placement arglist is more
	      ;; unusual than an initializer.
	      (cond ((memq expr1-res '(t prefix))
		     (setq c-record-type-identifiers expr1-rec))
		    ((memq expr2-res '(t prefix))
		     (setq c-record-type-identifiers expr2-rec))
		    ((eq expr1-res 'found)
		     (let ((c-promote-possible-types t))
		       (goto-char expr1-pos)
		       (c-forward-type)))
		    ((eq expr2-res 'found)
		     (let ((c-promote-possible-types t))
		       (goto-char expr2-pos)
		       (c-forward-type)))
		    ((and (eq expr1-res 'maybe) (not expr2-res))
		     (let ((c-promote-possible-types t))
		       (goto-char expr1-pos)
		       (c-forward-type)))
		    ((and (not expr1-res) (eq expr2-res 'maybe))
		     (let ((c-promote-possible-types t))
		       (goto-char expr2-pos)
		       (c-forward-type)))
		    ;; If both types are 'maybe then we're too uncertain.
		    )))))

	;; Fontify the type that now is recorded in
	;; `c-record-type-identifiers', if any.
	(c-fontify-recorded-types-and-refs)))))

(c-lang-defconst c-simple-decl-matchers
  "Simple font lock matchers for types and declarations.  These are used
on level 2 only and so aren't combined with `c-complex-decl-matchers'."

  t `(;; Fontify all type names and the identifiers in the
      ;; declarations they might start.  Use eval here since
      ;; `c-known-type-key' gets its value from
      ;; `*-font-lock-extra-types' on mode init.
      (eval . (list ,(c-make-font-lock-search-function
		      'c-known-type-key
		      '(1 'font-lock-type-face t)
		      '((c-font-lock-declarators limit t nil)
			(save-match-data
			  (goto-char (match-end 1))
			  (c-forward-syntactic-ws))
			(goto-char (match-end 1))))))

      ;; Fontify types preceded by `c-type-prefix-kwds' and the
      ;; identifiers in the declarations they might start.
      ,@(when (c-lang-const c-type-prefix-kwds)
	  (let ((prefix-re (c-make-keywords-re nil
			     (c-lang-const c-type-prefix-kwds))))
	    `((,(c-make-font-lock-search-function
		 (concat "\\<\\(" prefix-re "\\)"
			 "[ \t\n\r\f\v]+"
			 "\\(" (c-lang-const c-symbol-key) "\\)")
		 `(,(+ (c-regexp-opt-depth prefix-re) 2)
		   'font-lock-type-face t)
		 '((c-font-lock-declarators limit t nil)
		   (save-match-data
		     (goto-char (match-end 2))
		     (c-forward-syntactic-ws))
		   (goto-char (match-end 2))))))))
      ))

(c-lang-defconst c-complex-decl-matchers
  "Complex font lock matchers for types and declarations.  Used on level
3 and higher."

  t `(;; Fontify templates in C++.
      ,@(when (c-major-mode-is 'c++-mode)
	  `(c-font-lock-c++-templates))

      ;; Fontify all declarations and casts.
      c-font-lock-declarations

      ;; The first two rules here mostly find occurences that
      ;; `c-font-lock-declarations' has found already, but not
      ;; declarations containing blocks in the type (see note below).
      ;; It's also useful to fontify these everywhere to show e.g. when
      ;; a type keyword is accidentally used as an identifier.

      ;; Fontify basic types.
      ,(let ((re (c-make-keywords-re nil (c-lang-const c-type-kwds))))
	 (if (c-major-mode-is 'pike-mode)
	     ;; No symbol is a keyword after "->" in Pike.
	     `(,(concat "\\(\\=\\|\\(\\=\\|[^-]\\)[^>]\\)"
			"\\<\\(" re "\\)\\>")
	       3 font-lock-type-face)
	   `(,(concat "\\<\\(" re "\\)\\>")
	     1 'font-lock-type-face)))

      ;; Fontify types preceded by `c-type-prefix-kwds'.
      ,@(when (c-lang-const c-type-prefix-kwds)
	  `((,(byte-compile
	       `(lambda (limit)
		  (c-fontify-types-and-refs ((c-promote-possible-types t))
		    (save-restriction
		      ;; Narrow to avoid going past the limit in
		      ;; `c-forward-type'.
		      (narrow-to-region (point) limit)
		      (while (re-search-forward
			      ,(concat "\\<\\("
				       (c-make-keywords-re nil
					 (c-lang-const c-type-prefix-kwds))
				       "\\)\\>")
			      limit t)
			(unless (c-skip-comments-and-strings limit)
			  (c-forward-syntactic-ws)
			  ,(if (c-major-mode-is 'c++-mode)
			       `(when (and (c-forward-type)
					   (progn (c-forward-syntactic-ws)
						  (eq (char-after) ?=)))
				  ;; In C++ we additionally check for a "class
				  ;; X = Y" construct which is used in
				  ;; templates, to fontify Y as a type.
				  (forward-char)
				  (c-forward-syntactic-ws)
				  (c-forward-type))
			     `(c-forward-type))
			  )))))))))

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
      ,@(when (c-lang-const c-opt-block-decls-with-vars-key)
	  `((,(c-make-font-lock-search-function
	       (concat "}"
		       (c-lang-const c-single-line-syntactic-ws)
		       "\\("		; 1 + c-single-line-syntactic-ws-depth
		       (c-lang-const c-type-decl-prefix-key)
		       "\\|"
		       (c-lang-const c-symbol-key)
		       "\\)")
	       `((c-font-lock-declarators limit t nil)
		 (goto-char
		  (match-beginning
		   ,(1+ (c-lang-const c-single-line-syntactic-ws-depth)))))))))

      ;; Fontify the type in C++ "new" expressions.
      ,@(when (c-major-mode-is 'c++-mode)
	  `(("\\<new\\>"
	     (c-font-lock-c++-new))))
      ))

(c-lang-defconst c-basic-matchers-after
  "Font lock matchers for various things that should be fontified after
generic casts and declarations are fontified.  Used on level 2 and
higher."

  t `(;; Fontify the type after "new" in Java.
      ,@(when (c-lang-const c-inexpr-class-kwds)
	  (let* ((re (c-make-keywords-re nil
		       (c-lang-const c-inexpr-class-kwds)))
		 (identifier-offset (+ (c-regexp-opt-depth re)
				       (c-lang-const c-syntactic-ws-depth)
				       2)))
	    `((,(concat
		 "\\<\\(" re "\\)\\>"
		 (c-lang-const c-syntactic-ws)
		 "\\("			; identifier-offset
		 (c-lang-const c-identifier-key)
		 "\\)")
	       ,@(mapcar
		  (lambda (submatch)
		    `(,(+ identifier-offset submatch)
		      font-lock-type-face nil t))
		  (c-lang-const c-identifier-last-sym-match))))))

      ;; Fontify the identifiers inside enum lists.  (The enum type
      ;; name is handled by `c-simple-decl-matchers' or
      ;; `c-complex-decl-matchers' below.
      ,@(when (c-lang-const c-brace-list-kwds)
	  `((,(c-make-font-lock-search-function
	       (concat
		"\\<\\("
		(c-make-keywords-re nil (c-lang-const c-brace-list-kwds))
		"\\)\\>"
		;; Disallow various common punctuation chars that can't come
		;; before the '{' of the enum list, to avoid searching too far.
		"[^\]\[{}();,/#=]*"
		"{")
	       '((c-font-lock-declarators limit t nil)
		 (goto-char (match-end 0))
		 (goto-char (match-end 0)))))))

      ;; Fontify the list of exceptions after Java style "throws" etc.
      ,@(when (c-lang-const c-decl-spec-kwds)
	  `((,(c-make-font-lock-search-function
	       (concat
		"\\<\\("
		(c-make-keywords-re nil (c-lang-const c-decl-spec-kwds))
		"\\)\\>")
	       '((c-font-lock-identifier-list limit 'font-lock-type-face))))))

      ,@(when (c-major-mode-is 'c++-mode)
	  `(;; Fontify class inherit lists in C++.
	    (,(c-make-font-lock-search-function
	       (concat
		"\\<\\("
		(c-make-keywords-re nil (c-lang-const c-class-kwds))
		"\\)\\>"
		;; Disallow various common punctuation chars that can't come
		;; before the ':' that starts the inherit list, to avoid
		;; searching too far.
		"[^\]\[{}();,/#=:]*"
		":")
	       '((c-font-lock-identifier-list limit 'font-lock-type-face))))

	    ;; Fontify throw specifications.
	    (,(c-make-font-lock-search-function
	       (concat "\\<throw\\>"
		       (c-lang-const c-syntactic-ws)
		       "(")
	       '((c-font-lock-identifier-list limit 'font-lock-type-face))))))
      ))

(c-lang-defconst c-matchers-1
  t (c-lang-const c-cpp-matchers))

(c-lang-defconst c-matchers-2
  t (append (c-lang-const c-matchers-1)
	    (c-lang-const c-basic-matchers-before)
	    (c-lang-const c-simple-decl-matchers)
	    (c-lang-const c-basic-matchers-after)))

(c-lang-defconst c-matchers-3
  t (append (c-lang-const c-matchers-1)
	    (c-lang-const c-basic-matchers-before)
	    (c-lang-const c-complex-decl-matchers)
	    (c-lang-const c-basic-matchers-after)))

(c-lang-defconst c-matchers-4
  t (c-lang-const c-matchers-3))


(defun c-override-default-keywords (def-var new-def)
  ;; This is used to override the value on a `*-font-lock-keywords'
  ;; variable only if it's nil or has the same value as one of the
  ;; `*-font-lock-keywords-*' variables.  Older font-lock packages
  ;; define a default value for `*-font-lock-keywords' which we want
  ;; to override, but we should otoh avoid clobbering a user setting.
  ;; This heuristic for that isn't perfect, but I can't think of any
  ;; better. /mast
  ;;
  ;; This function does not do any hidden buffer changes.
  (when (and (boundp def-var)
	     (memq (symbol-value def-var)
		   (cons nil
			 (mapcar
			  (lambda (suffix)
			    (let ((sym (intern (concat (symbol-name def-var)
						       suffix))))
			      (and (boundp sym) (symbol-value sym))))
			  '("-1" "-2" "-3" "-4")))))
    (set def-var new-def)))

;;; C.

(defconst c-font-lock-keywords-1 (c-lang-const c-matchers-1 c)
  "Minimal highlighting for C mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c-font-lock-keywords-2 (c-lang-const c-matchers-2 c)
  "Fast normal highlighting for C mode.
In addition to `c-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c-font-lock-extra-types'.")

(defconst c-font-lock-keywords-3 (c-lang-const c-matchers-3 c)
  "Accurate normal highlighting for C mode.
Like `c-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `c-font-lock-extra-types'.")

(c-override-default-keywords 'c-font-lock-keywords
			     c-font-lock-keywords-3)

(defvar c-font-lock-keywords c-font-lock-keywords-3
  "Default expressions to highlight in C mode.")

;;; C++.

(defconst c++-font-lock-keywords-1 (c-lang-const c-matchers-1 c++)
  "Minimal highlighting for C++ mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c++-font-lock-keywords-2 (c-lang-const c-matchers-2 c++)
  "Fast normal highlighting for C++ mode.
In addition to `c++-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `c++-font-lock-extra-types'.")

(defconst c++-font-lock-keywords-3 (c-lang-const c-matchers-3 c++)
  "Accurate normal highlighting for C++ mode.
Like `c++-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `c++-font-lock-extra-types'.")

(c-override-default-keywords 'c++-font-lock-keywords
			     c++-font-lock-keywords-3)

(defvar c++-font-lock-keywords c++-font-lock-keywords-3
  "Default expressions to highlight in C++ mode.")

;;; Objective-C.

(defconst objc-font-lock-keywords-1 (c-lang-const c-matchers-1 objc)
  "Minimal highlighting for ObjC mode.
Fontifies only compiler directives (in addition to the syntactic
fontification of strings and comments).")

(defconst objc-font-lock-keywords-2 (c-lang-const c-matchers-2 objc)
  "Fast normal highlighting for ObjC mode.
In addition to `objc-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `objc-font-lock-extra-types'.")

(defconst objc-font-lock-keywords-3 (c-lang-const c-matchers-3 objc)
  "Accurate normal highlighting for ObjC mode.
Like `objc-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `objc-font-lock-extra-types'.")

(c-override-default-keywords 'objc-font-lock-keywords
			     objc-font-lock-keywords-3)

(defvar objc-font-lock-keywords objc-font-lock-keywords-3
  "Default expressions to highlight in ObjC mode.")

;;; Java.

(defconst java-font-lock-keywords-1 (c-lang-const c-matchers-1 java)
  "Minimal highlighting for Java mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst java-font-lock-keywords-2 (c-lang-const c-matchers-2 java)
  "Fast normal highlighting for Java mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `java-font-lock-extra-types'.")

(defconst java-font-lock-keywords-3 (c-lang-const c-matchers-3 java)
  "Accurate normal highlighting for Java mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(c-override-default-keywords 'java-font-lock-keywords
			     java-font-lock-keywords-3)

(defvar java-font-lock-keywords java-font-lock-keywords-3
  "Default expressions to highlight in Java mode.")

;;; IDL.

(defconst idl-font-lock-keywords-1 (c-lang-const c-matchers-1 idl)
  "Minimal highlighting for IDL mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst idl-font-lock-keywords-2 (c-lang-const c-matchers-2 idl)
  "Fast normal highlighting for IDL mode.
In addition to `idl-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `idl-font-lock-extra-types'.")

(defconst idl-font-lock-keywords-3 (c-lang-const c-matchers-3 idl)
  "Accurate normal highlighting for IDL mode.
Like `idl-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `idl-font-lock-extra-types'.")

(c-override-default-keywords 'idl-font-lock-keywords
			     idl-font-lock-keywords-3)

(defvar idl-font-lock-keywords idl-font-lock-keywords-3
  "Default expressions to highlight in IDL mode.")

;;; Pike.

(defconst pike-font-lock-keywords-1 (c-lang-const c-matchers-1 pike)
  "Minimal highlighting for Pike mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst pike-font-lock-keywords-2 (c-lang-const c-matchers-2 pike)
  "Fast normal highlighting for Pike mode.
In addition to `pike-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, and
the user defined types on `pike-font-lock-extra-types'.")

(defconst pike-font-lock-keywords-3 (c-lang-const c-matchers-3 pike)
  "Accurate normal highlighting for Pike mode.
Like `pike-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `pike-font-lock-extra-types'.")

(defconst pike-font-lock-keywords-4 (c-lang-const c-matchers-4 pike)
  "Accurate extra highlighting for Pike mode.
In addition to `pike-font-lock-keywords-3', this adds fontification of
refdoc comments and the markup inside them.")

(c-override-default-keywords 'pike-font-lock-keywords
			     pike-font-lock-keywords-4)

(defvar pike-font-lock-keywords pike-font-lock-keywords-4
  "Default expressions to highlight in Pike mode.")


(cc-provide 'cc-fonts)

;;; cc-fonts.el ends here
