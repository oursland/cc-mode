;;; cc-defs.el --- compile time definitions for CC Mode

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

;; This file contains macros, defsubsts, and various other things that
;; must be loaded early both during compilation and at runtime.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

;; `require' in XEmacs doesn't have the third NOERROR argument.
(condition-case nil (require 'regexp-opt) (file-error nil))


(eval-and-compile
  ;; The following is used below during compilation.

  (defvar c-inside-eval-when-compile nil)

  (defmacro cc-eval-when-compile (&rest body)
    "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant.

This variant works around bugs in `eval-when-compile' in various
\(X)Emacs versions.  See cc-defs.el for details."

    (if c-inside-eval-when-compile
	;; XEmacs 21.4.6 has a bug in `eval-when-compile' in that it
	;; evaluates its body at macro expansion time if it's nested
	;; inside another `eval-when-compile'.  So we use a dynamically
	;; bound variable to avoid nesting them.
	`(progn ,@body)

      `(eval-when-compile
	 ;; In all (X)Emacsen so far, `eval-when-compile' byte compiles
	 ;; its contents before evaluating it.  That can cause forms to
	 ;; be compiled in situations they aren't intended to be
	 ;; compiled.
	 ;;
	 ;; Example: It's not possible to defsubst a primitive, e.g. the
	 ;; following will produce an error (in any emacs flavor), since
	 ;; `nthcdr' is a primitive function that's handled specially by
	 ;; the byte compiler and thus can't be redefined:
	 ;;
	 ;;     (defsubst nthcdr (val) val)
	 ;;
	 ;; `defsubst', like `defmacro', needs to be evaluated at
	 ;; compile time, so this will produce an error during byte
	 ;; compilation.
	 ;;
	 ;; CC Mode occasionally needs to do things like this for
	 ;; cross-emacs compatibility.  It therefore uses the following
	 ;; to conditionally do a `defsubst':
	 ;;
	 ;;     (eval-when-compile
	 ;;       (if (not (fboundp 'foo))
	 ;;           (defsubst foo ...)))
	 ;;
	 ;; But `eval-when-compile' byte compiles its contents and
	 ;; _then_ evaluates it (in all current emacs versions, up to
	 ;; and including Emacs 20.6 and XEmacs 21.1 as of this
	 ;; writing).  So this will still produce an error, since the
	 ;; byte compiler will get to the defsubst anyway.  That's
	 ;; arguably a bug because the point with `eval-when-compile' is
	 ;; that it should evaluate rather than compile its contents.
	 ;;
	 ;; We get around it by expanding the body to a quoted constant
	 ;; that we eval.
	 (eval '(let ((c-inside-eval-when-compile t)) ,@body)))))

  (put 'cc-eval-when-compile 'lisp-indent-hook 0))


;; cc-mode-19.el contains compatibility macros that should be used if
;; needed.
(eval-and-compile
  (if (or (not (fboundp 'functionp))
	  (not (condition-case nil
		   (progn (eval '(char-before)) t)
		 (error nil)))
	  (not (condition-case nil
		   (progn (eval '(char-after)) t)
		 (error nil)))
	  (not (fboundp 'when))
	  (not (fboundp 'unless))
	  (not (fboundp 'regexp-opt))
	  (not (fboundp 'regexp-opt-depth)))
      (cc-load "cc-mode-19")
    (defalias 'c-regexp-opt 'regexp-opt)
    (defalias 'c-regexp-opt-depth 'regexp-opt-depth)))

(require 'cl)

;; Silence the compiler.
(cc-bytecomp-defvar c-enable-xemacs-performance-kludge-p) ; In cc-vars.el
(cc-bytecomp-defvar c-buffer-is-cc-mode) ; In cc-vars.el
(cc-bytecomp-defun buffer-syntactic-context-depth) ; XEmacs
(cc-bytecomp-defun region-active-p)	; XEmacs
(cc-bytecomp-defvar zmacs-region-stays)	; XEmacs
(cc-bytecomp-defvar zmacs-regions)	; XEmacs
(cc-bytecomp-defvar mark-active)	; Emacs
(cc-bytecomp-defvar deactivate-mark)	; Emacs
(cc-bytecomp-defvar inhibit-point-motion-hooks) ; Emacs
(cc-bytecomp-defun scan-lists)		; 5 args in XEmacs, 3 in Emacs
(cc-bytecomp-defvar parse-sexp-lookup-properties) ; Emacs 20+
(cc-bytecomp-defvar text-property-default-nonsticky) ; Emacs 21
(cc-bytecomp-defvar lookup-syntax-properties) ; XEmacs 21


;;; Macros.

(defmacro c-point (position &optional point)
  "Return the value of certain commonly referenced POSITIONs relative to POINT.
The current point is used if POINT isn't specified.  POSITION can be
one of the following symbols:

`bol'  -- beginning of line
`eol'  -- end of line
`bod'  -- beginning of defun
`eod'  -- end of defun
`boi'  -- beginning of indentation
`ionl' -- indentation of next line
`iopl' -- indentation of previous line
`bonl' -- beginning of next line
`eonl' -- end of next line
`bopl' -- beginning of previous line
`eopl' -- end of previous line

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify point or mark.

This function does not do any hidden buffer changes."

  (if (eq (car-safe position) 'quote)
      (let ((position (eval position)))
	(cond

	 ((eq position 'bol)
	  (if (and (fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (point))))

	 ((eq position 'eol)
	  (if (and (fboundp 'line-end-position) (not point))
	      `(line-end-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (end-of-line)
	       (point))))

	 ((eq position 'boi)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (back-to-indentation)
	     (point)))

	 ((eq position 'bod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-beginning-of-defun-1)
	     (point)))

	 ((eq position 'eod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-end-of-defun-1)
	     (point)))

	 ((eq position 'bopl)
	  (if (and (fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line -1)
	       (point))))

	 ((eq position 'bonl)
	  (if (and (fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (point))))

	 ((eq position 'eopl)
	  (if (and (fboundp 'line-end-position) (not point))
	      `(line-end-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (or (bobp) (backward-char))
	       (point))))

	 ((eq position 'eonl)
	  (if (and (fboundp 'line-end-position) (not point))
	      `(line-end-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (end-of-line)
	       (point))))

	 ((eq position 'iopl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line -1)
	     (back-to-indentation)
	     (point)))

	 ((eq position 'ionl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line 1)
	     (back-to-indentation)
	     (point)))

	 (t (error "Unknown buffer position requested: %s" position))))

    ;;(message "c-point long expansion")
    `(save-excursion
       ,@(if point `((goto-char ,point)))
       (let ((position ,position))
	 (cond
	  ((eq position 'bol)  (beginning-of-line))
	  ((eq position 'eol)  (end-of-line))
	  ((eq position 'boi)  (back-to-indentation))
	  ((eq position 'bod)  (c-beginning-of-defun-1))
	  ((eq position 'eod)  (c-end-of-defun-1))
	  ((eq position 'bopl) (forward-line -1))
	  ((eq position 'bonl) (forward-line 1))
	  ((eq position 'eopl) (progn
				 (beginning-of-line)
				 (or (bobp) (backward-char))))
	  ((eq position 'eonl) (progn
				 (forward-line 1)
				 (end-of-line)))
	  ((eq position 'iopl) (progn
				 (forward-line -1)
				 (back-to-indentation)))
	  ((eq position 'ionl) (progn
				 (forward-line 1)
				 (back-to-indentation)))
	  (t (error "Unknown buffer position requested: %s" position))))
       (point))))

(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  ;;
  ;; This function does not do any hidden buffer changes.
  `(condition-case nil
       (progn ,@body)
     (error nil)))
(put 'c-safe 'lisp-indent-function 0)

;; The following is essentially `save-buffer-state' from lazy-lock.el.
;; It ought to be a standard macro.
(defmacro c-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST (in `let*' style) and eval BODY,
then restore the buffer state under the assumption that no significant
modification has been made.  A change is considered significant if it
affected the buffer text in any way that wasn't completely restored
again.  Changes in text properties like `face' or `syntax-table' are
considered insignificant.

The return value is the value of the last form in BODY."
  `(let* (,@(append '((modified (buffer-modified-p)) (buffer-undo-list t)
		      (inhibit-read-only t) (inhibit-point-motion-hooks t)
		      before-change-functions after-change-functions
		      deactivate-mark)
		    varlist))
     (prog1 (progn ,@body)
       (and (not modified)
	    (buffer-modified-p)
	    (set-buffer-modified-p nil)))))
(put 'c-save-buffer-state 'lisp-indent-function 1)

(defmacro c-forward-syntactic-ws (&optional limit)
  "Forward skip over syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.

LIMIT sets an upper limit of the forward movement, if specified.  If
LIMIT or the end of the buffer is reached inside a comment or
preprocessor directive, the point will be left there.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (if limit
      `(save-restriction
	 (narrow-to-region (point-min) (or ,limit (point-max)))
	 (c-forward-sws))
    '(c-forward-sws)))

(defmacro c-backward-syntactic-ws (&optional limit)
  "Backward skip over syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.

LIMIT sets a lower limit of the backward movement, if specified.  If
LIMIT is reached inside a line comment or preprocessor directive then
the point is moved into it past the whitespace at the end.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (if limit
      `(save-restriction
	 (narrow-to-region (or ,limit (point-min)) (point-max))
	 (c-backward-sws))
    '(c-backward-sws)))

(defmacro c-forward-sexp (&optional count)
  "Move forward across COUNT balanced expressions.
A negative COUNT means move backward.  Signal an error if the move
fails for any reason.

This is like `forward-sexp' except that it isn't interactive and does
not do any user friendly adjustments of the point and that it isn't
susceptible to user configurations such as disabling of signals in
certain situations.

This function does not do any hidden buffer changes."
  (or count (setq count 1))
  `(goto-char (or (scan-sexps (point) ,count)
		  ,(if (numberp count)
		       (if (> count 0) `(point-max) `(point-min))
		     `(if (> ,count 0) (point-max) (point-min))))))

(defmacro c-backward-sexp (&optional count)
  "See `c-forward-sexp' and reverse directions."
  (or count (setq count 1))
  `(c-forward-sexp ,(if (numberp count) (- count) `(- ,count))))

;; Wrappers for common scan-lists cases, mainly because it's almost
;; impossible to get a feel for how that function works.
;;
;; These functions do not do any hidden buffer changes.
(defmacro c-up-list-forward (pos)
  `(c-safe (scan-lists ,pos 1 1)))
(defmacro c-up-list-backward (pos)
  `(c-safe (scan-lists ,pos -1 1)))
(defmacro c-down-list-forward (pos)
  `(c-safe (scan-lists ,pos 1 -1)))
(defmacro c-down-list-backward (pos)
  `(c-safe (scan-lists ,pos -1 -1)))

(defmacro c-benign-error (format &rest args)
  ;; Formats an error message for the echo area and dings, i.e. like
  ;; `error' but doesn't abort.
  ;;
  ;; This function does not do any hidden buffer changes.
  `(progn
     (message ,format ,@args)
     (ding)))

(defmacro c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  ;;
  ;; This function does not do any hidden buffer changes.
  `(progn
     (setq c-auto-hungry-string
	   (if c-auto-newline
	       (if c-hungry-delete-key "/ah" "/a")
	     (if c-hungry-delete-key "/h" nil)))
     (force-mode-line-update)))

(defmacro c-with-syntax-table (table &rest code)
  ;; Temporarily switches to the specified syntax table in a failsafe
  ;; way to execute code.
  ;;
  ;; This function does not do any hidden buffer changes.
  `(let ((c-with-syntax-table-orig-table (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table ,table)
	   ,@code)
       (set-syntax-table c-with-syntax-table-orig-table))))
(put 'c-with-syntax-table 'lisp-indent-function 1)

(defmacro c-skip-ws-forward (&optional limit)
  "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace and line
continuations.

This function does not do any hidden buffer changes."
  (if limit
      `(let ((limit (or ,limit (point-max))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-forward " \t\n\r\f\v" limit)
		  (when (and (eq (char-after) ?\\)
			     (< (point) limit))
		    (forward-char)
		    (or (eolp)
			(progn (backward-char) nil))))))
    '(while (progn
	      (skip-chars-forward " \t\n\r\f\v")
	      (when (eq (char-after) ?\\)
		(forward-char)
		(or (eolp)
		    (progn (backward-char) nil)))))))

(defmacro c-skip-ws-backward (&optional limit)
  "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace and line
continuations.

This function does not do any hidden buffer changes."
  (if limit
      `(let ((limit (or ,limit (point-min))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-backward " \t\n\r\f\v" limit)
		  (and (eolp)
		       (eq (char-before) ?\\)
		       (> (point) limit)))
	   (backward-char)))
    '(while (progn
	      (skip-chars-backward " \t\n\r\f\v")
	      (and (eolp)
		   (eq (char-before) ?\\)))
       (backward-char))))

(defmacro c-major-mode-is (mode)
  "Return non-nil if the current CC Mode major mode is MODE.
MODE is either a mode symbol or a list of mode symbols.

This function does not do any hidden buffer changes."
  (if (eq (car-safe mode) 'quote)
      (let ((mode (eval mode)))
	(if (listp mode)
	    `(memq c-buffer-is-cc-mode ',mode)
	  `(eq c-buffer-is-cc-mode ',mode)))
    `(let ((mode ,mode))
       (if (listp mode)
	   (memq c-buffer-is-cc-mode mode)
	 (eq c-buffer-is-cc-mode mode)))))

(defmacro c-parse-sexp-lookup-properties ()
  ;; Return the value of the variable that says whether the
  ;; syntax-table property affects the sexp routines.  Always return
  ;; nil in (X)Emacsen without support for that.
  ;;
  ;; This function does not do any hidden buffer changes.
  (cond ((cc-bytecomp-boundp 'parse-sexp-lookup-properties)
	 `parse-sexp-lookup-properties)
	((cc-bytecomp-boundp 'lookup-syntax-properties)
	 `lookup-syntax-properties)
	(t nil)))

(defmacro c-clear-char-syntax (pos)
  ;; Remove the syntax-table property at POS if there is any.
  (if (and (fboundp 'make-extent)
	   (fboundp 'delete-extent)
	   (fboundp 'set-extent-properties))
      ;; XEmacs.  Check all the extent functions we'll use since some
      ;; packages might do compatibility aliases for some of them in
      ;; Emacs.
      `(let ((ext (extent-at ,pos nil 'syntax-table)))
	 (if ext (delete-extent ext)))
    ;; Emacs.
    `(let ((pos ,pos))
       ,(if (cc-bytecomp-boundp 'text-property-default-nonsticky)
	    ;; In Emacs 21 we got the rear-nonsticky property covered
	    ;; by `text-property-default-nonsticky'.
	    `(remove-text-properties pos (1+ pos) '(syntax-table nil))
	  ;; In Emacs 20 we have to mess with the rear-nonsticky property.
	  `(when (get-text-property pos 'syntax-table)
	     (remove-text-properties pos (1+ pos) '(syntax-table nil))
	     (put-text-property pos (1+ pos)
				'rear-nonsticky
				(delq 'syntax-table
				      (get-text-property
				       pos 'rear-nonsticky))))))))

(defmacro c-put-char-syntax (pos syntax)
  ;; Put a syntax-table property at POS and make it front and rear
  ;; nonsticky (or start and end open in XEmacs vocabulary).
  (cond ((and (fboundp 'make-extent)
	      (fboundp 'delete-extent)
	      (fboundp 'set-extent-properties))
	 ;; XEmacs.  Check all the extent functions we'll use since
	 ;; some packages might do compatibility aliases for some of
	 ;; them in Emacs.
	 `(let ((pos ,pos))
	    (set-extent-properties (make-extent pos (1+ pos))
				   (list 'syntax-table ,syntax
					 'start-open t
					 'end-open t))))
	((cc-bytecomp-boundp 'text-property-default-nonsticky)
	 ;; In Emacs 21 we got the rear-nonsticky property covered
	 ;; by `text-property-default-nonsticky'.
	 `(let ((pos ,pos))
	    (put-text-property pos (1+ pos)
			       'syntax-table ,syntax)))
	(t
	 ;; In Emacs 20 we have to mess with the rear-nonsticky property.
	 `(let* ((pos ,pos)
		 (prop (get-text-property pos 'rear-nonsticky)))
	    (put-text-property pos (1+ pos)
			       'syntax-table ,syntax)
	    (or (memq 'syntax-table prop)
		(put-text-property pos (1+ pos)
				   'rear-nonsticky
				   (cons 'syntax-table prop)))))))


;; Some fontification stuff.  It's necessary to put it here and not in
;; cc-fonts where it belongs, since it's also used in some functions
;; in cc-engine, and we don't want to require cc-fonts there.
;;
;; These things will be called in cc-engine only when
;; `c-fontify-types-and-refs' is set, and that will only happen when
;; called from cc-fonts.  Thus it's safe even though the code in
;; cc-engine might get references to the font-lock package, which it
;; doesn't require at run time.

(cc-eval-when-compile
  ;; Used at compile time to get the right definition in
  ;; `c-put-font-lock-face'.
  (require 'font-lock))

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


;; Make edebug understand the macros.
(eval-after-load "edebug"
  '(progn
     (def-edebug-spec c-point t)
     (def-edebug-spec c-safe t)
     (def-edebug-spec c-save-buffer-state let)
     (def-edebug-spec c-forward-syntactic-ws t)
     (def-edebug-spec c-backward-syntactic-ws t)
     (def-edebug-spec c-forward-sexp t)
     (def-edebug-spec c-backward-sexp t)
     (def-edebug-spec c-up-list-forward t)
     (def-edebug-spec c-up-list-backward t)
     (def-edebug-spec c-down-list-forward t)
     (def-edebug-spec c-down-list-backward t)
     (def-edebug-spec c-add-syntax t)
     (def-edebug-spec c-add-class-syntax t)
     (def-edebug-spec c-benign-error t)
     (def-edebug-spec c-with-syntax-table t)
     (def-edebug-spec c-skip-ws-forward t)
     (def-edebug-spec c-skip-ws-backward t)
     (def-edebug-spec c-major-mode-is t)
     (def-edebug-spec c-clear-char-syntax t)
     (def-edebug-spec c-put-char-syntax t)
     (def-edebug-spec cc-eval-when-compile t)
     (def-edebug-spec c-put-font-lock-face t)))


;;; Functions.

;; Note: All these after the macros, to be on safe side in avoiding
;; bugs where macros are defined too late.  These bugs often only show
;; when the files are compiled in a certain order within the same
;; session.

(defsubst c-beginning-of-defun-1 ()
  ;; Wrapper around beginning-of-defun.
  ;;
  ;; NOTE: This function should contain the only explicit use of
  ;; beginning-of-defun in CC Mode.  Eventually something better than
  ;; b-o-d will be available and this should be the only place the
  ;; code needs to change.  Everything else should use
  ;; (c-beginning-of-defun-1)
  ;;
  ;; This function does not do any hidden buffer changes.

  (if (and (fboundp 'buffer-syntactic-context-depth)
	   c-enable-xemacs-performance-kludge-p)
      ;; XEmacs only.  This can improve the performance of
      ;; c-parse-state to between 3 and 60 times faster when
      ;; braces are hung.  It can also degrade performance by
      ;; about as much when braces are not hung.
      (let (pos)
	(while (not pos)
	  (save-restriction
	    (widen)
	    (setq pos (scan-lists (point) -1
				  (buffer-syntactic-context-depth)
				  nil t)))
	  (cond
	   ((bobp) (setq pos (point-min)))
	   ((not pos)
	    (let ((distance (skip-chars-backward "^{")))
	      ;; unbalanced parenthesis, while illegal C code,
	      ;; shouldn't cause an infloop!  See unbal.c
	      (when (zerop distance)
		;; Punt!
		(beginning-of-defun)
		(setq pos (point)))))
	   ((= pos 0))
	   ((not (eq (char-after pos) ?{))
	    (goto-char pos)
	    (setq pos nil))
	   ))
	(goto-char pos))
    ;; Emacs, which doesn't have buffer-syntactic-context-depth
    (beginning-of-defun))
  ;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at the
  ;; open brace.
  (and defun-prompt-regexp
       (looking-at defun-prompt-regexp)
       (goto-char (match-end 0))))

(defsubst c-end-of-defun-1 ()
  ;; Replacement for end-of-defun that use c-beginning-of-defun-1.
  (let ((start (point)))
    ;; Skip forward into the next defun block. Don't bother to avoid
    ;; comments, literals etc, since beginning-of-defun doesn't do that
    ;; anyway.
    (skip-chars-forward "^}")
    (c-beginning-of-defun-1)
    (if (eq (char-after) ?{)
	(c-forward-sexp))
    (if (< (point) start)
	(goto-char (point-max)))))

(defconst c-template-open-syntax '(4 . ?>))

(defsubst c-mark-template-open (pos)
  ;; Mark the '<' at POS as a C++ template opener using the
  ;; syntax-table property.  Note that Emacs 19 and XEmacs <= 20
  ;; doesn't support syntax properties, so this function might not
  ;; have any effect.
  (c-put-char-syntax pos c-template-open-syntax))

(defconst c-template-close-syntax '(5 . ?<))

(defsubst c-mark-template-close (pos)
  ;; Mark the '>' at POS as a C++ template closer using the
  ;; syntax-table property.  Note that Emacs 19 and XEmacs <= 20
  ;; doesn't support syntax properties, so this function might not
  ;; have any effect.
  (c-put-char-syntax pos c-template-close-syntax))

(defsubst c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  ;;
  ;; This function does not do any hidden buffer changes.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defsubst c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  ;;
  ;; This function does not do any hidden buffer changes.
  (assq (car (c-intersect-lists list alist1)) alist2))

(defsubst c-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of LANGELEM's relpos.
Leaves point at the relpos unless PRESERVE-POINT is non-nil.

This function does not do any hidden buffer changes."
  (if (cdr langelem)
      (let ((here (point)))
	(goto-char (cdr langelem))
	(prog1 (current-column)
	  (if preserve-point
	      (goto-char here))
	  ))
    0))

(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in XEmacs.
  ;; This is not needed for Emacs.
  ;;
  ;; This function does not do any hidden buffer changes.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst c-region-is-active-p ()
  ;; Return t when the region is active.  The determination of region
  ;; activeness is different in both Emacs and XEmacs.
  ;;
  ;; This function does not do any hidden buffer changes.
  (cond
   ;; XEmacs
   ((and (fboundp 'region-active-p)
	 (boundp 'zmacs-regions)
	 zmacs-regions)
    (region-active-p))
   ;; Emacs
   ((boundp 'mark-active) mark-active)
   ;; fallback; shouldn't get here
   (t (mark t))))

(put 'c-mode    'c-mode-prefix "c-")
(put 'c++-mode  'c-mode-prefix "c++-")
(put 'objc-mode 'c-mode-prefix "objc-")
(put 'java-mode 'c-mode-prefix "java-")
(put 'idl-mode  'c-mode-prefix "idl-")
(put 'pike-mode 'c-mode-prefix "pike-")

(defsubst c-mode-symbol (suffix)
  "Prefix the current mode prefix (e.g. \"c-\") to SUFFIX and return
the corresponding symbol.

This function does not do any hidden buffer changes."
  (or c-buffer-is-cc-mode
      (error "Not inside a CC Mode based mode"))
  (let ((mode-prefix (get c-buffer-is-cc-mode 'c-mode-prefix)))
    (or mode-prefix
	(error "%S has no mode prefix known to `c-mode-symbol'"
	       c-buffer-is-cc-mode))
    (intern (concat mode-prefix suffix))))

(defsubst c-mode-var (suffix)
  "Prefix the current mode prefix (e.g. \"c-\") to SUFFIX and return
the value of the variable with that name.

This function does not do any hidden buffer changes."
  (symbol-value (c-mode-symbol suffix)))

(defsubst c-face-name-p (facename)
  ;; Return t if FACENAME is the name of a face.  This method is
  ;; necessary since facep in XEmacs only returns t for the actual
  ;; face objects (while it's only their names that are used just
  ;; about anywhere else) without providing a predicate that tests
  ;; face names.
  ;;
  ;; This function does not do any hidden buffer changes.
  (memq facename (face-list)))

(defsubst c-put-type-face (from to)
  ;; Put the face `font-lock-type-face' on the given region.  Does not
  ;; clobber match-data.
  (c-put-font-lock-face from to 'font-lock-type-face))

(defsubst c-put-reference-face (from to)
  ;; Put the face `font-lock-reference-face' on the given region.
  ;; Does not clobber match-data.  Note that the face is a variable
  ;; that is dereferenced, since it's an alias in Emacs.
  (c-put-font-lock-face from to font-lock-reference-face))

(defun c-make-keywords-re (adorn list)
  "Make a regexp that matches all the strings the list.
Duplicates in the list are removed.  The regexp may contain zero or
more submatch expressions.  If ADORN is non-nil there will be at least
one submatch which matches the whole keyword, and the regexp will also
not match a prefix of any identifier.  Adorned regexps cannot be
appended."
  (setq list (delete-duplicates list :test 'string-equal))
  (if list
      (let ((re (c-regexp-opt list)))
	;; Add our own grouping parenthesis around re instead of
	;; passing adorn to `regexp-opt', since in XEmacs it makes the
	;; top level grouping "shy".
	(if adorn
	    (concat "\\(" re "\\)"
		    "\\("
		    (get 'c-nonsymbol-key c-buffer-is-cc-mode)
		    "\\|$\\)")
	  re))
    "\\<\\>"				; Matches nothing.
    ))
(put 'c-make-keywords-re 'lisp-indent-function 1)


(cc-provide 'cc-defs)

;;; cc-defs.el ends here
