;;; cc-langs.el --- specific language support for CC Mode

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Authors:  1992-1997 Barry A. Warsaw
;;           1987 Dave Detlefs and Stewart Clamen
;;           1985 Richard M. Stallman
;; Created:  22-Apr-1997 (split from cc-mode.el)
;; Version:  5.00
;; Keywords: c languages oop

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



;; Support for C and base language support
(defvar c-mode-abbrev-table nil
  "Abbrev table in use in c-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    nil
  ;; TBD: should we even worry about naming this keymap. My vote: no,
  ;; because Emacs and XEmacs do it differently.
  (setq c-mode-map (make-sparse-keymap))
  ;; put standard keybindings into MAP
  ;; the following mappings correspond more or less directly to BOCM
  (define-key c-mode-map "{"         'c-electric-brace)
  (define-key c-mode-map "}"         'c-electric-brace)
  (define-key c-mode-map ";"         'c-electric-semi&comma)
  (define-key c-mode-map "#"         'c-electric-pound)
  (define-key c-mode-map ":"         'c-electric-colon)
  ;; Lucid Emacs 19.9 defined these two, the second of which was
  ;; commented out...
  ;; (define-key c-mode-map "\e{" 'c-insert-braces)
  ;; Commented out electric square brackets because nobody likes them.
  ;; (define-key c-mode-map "[" 'c-insert-brackets)
  (define-key c-mode-map "\C-c\C-m"  'c-mark-function)
  (define-key c-mode-map "\e\C-q"    'c-indent-exp)
  (define-key c-mode-map "\ea"       'c-beginning-of-statement)
  (define-key c-mode-map "\ee"       'c-end-of-statement)
  (define-key c-mode-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-map "\C-c\C-u"  'c-up-conditional)
  (define-key c-mode-map "\t"        'c-indent-command)
  (define-key c-mode-map "\177"      'c-electric-delete)
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key c-mode-map ","         'c-electric-semi&comma)
  (define-key c-mode-map "*"         'c-electric-star)
  (define-key c-mode-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-map "\C-c\C-\\" 'c-backslash-region)
  ;; TBD: where if anywhere, to put c-backward|forward-into-nomenclature
  (define-key c-mode-map "\C-c\C-a"  'c-toggle-auto-state)
  (define-key c-mode-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-map "\C-c\C-c"  'comment-region)
  (define-key c-mode-map "\C-c\C-d"  'c-toggle-hungry-state)
  (define-key c-mode-map "\C-c\C-e"  'c-macro-expand)
  (define-key c-mode-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-map "\C-c\C-s"  'c-show-syntactic-information)
  (define-key c-mode-map "\C-c\C-t"  'c-toggle-auto-hungry-state)
  (define-key c-mode-map "\C-c."     'c-set-style)
  ;; conflicts with OOBR
  ;;(define-key c-mode-map "\C-c\C-v"  'c-version)
  ;;
  (if (and
       ;; Infodock has it's own menu
       (not (memq 'infodock c-emacs-features))
       ;; Emacs 19 defines menus in the mode map. This call will
       ;; return t on Emacs 19, otherwise no-op and return nil.
       (not (c-mode-fsf-menu "CC Mode" c-mode-map))
       ;; In XEmacs 19, we want the menu to popup when the 3rd button
       ;; is hit.  In Lucid Emacs 19.10 and beyond this is done
       ;; automatically if we put the menu on mode-popup-menu
       ;; variable, see c-common-init. Emacs 19 uses C-Mouse-3 for
       ;; this, and it works with no special effort.
       (boundp 'current-menubar)
       (not (boundp 'mode-popup-menu)))
      (define-key c-mode-map 'button3 'c-popup-menu)))

(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table)
  ;; add extra comment syntax
  (modify-syntax-entry ?/  ". 14"  c-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  c-mode-syntax-table))



;;;###autoload
(defun c-mode ()
  "Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq comment-start "/* "
	comment-end   " */"
	comment-multi-line t
	c-conditional-key c-C-conditional-key
	c-class-key c-C-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-C-comment-start-regexp
	imenu-generic-expression cc-imenu-c-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c-mode-hook))

(setq c-list-of-mode-names (cons "C" c-list-of-mode-names))



(defun c-enable-//-in-c-mode ()
  "Enables // as a comment delimiter in `c-mode'.
ANSI C currently does *not* allow this, although many C compilers
support optional C++ style comments.  To use, call this function from
your `.emacs' file before you visit any C files.  The changes are
global and affect all future `c-mode' buffers."
  (c-setup-dual-comments c-mode-syntax-table)
  (setq-default c-C-comment-start-regexp c-C++-comment-start-regexp))




;; Support for C++

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in c++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c++-mode-map ()
  "Keymap used in c++-mode buffers.")
(if c++-mode-map
    nil
  (setq c++-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C++
  (define-key c++-mode-map "\C-c:"  'c-scope-operator)
  (define-key c++-mode-map "/"      'c-electric-slash)
  (define-key c++-mode-map "<"      'c-electric-lt-gt)
  (define-key c++-mode-map ">"      'c-electric-lt-gt))

(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments c++-mode-syntax-table)
  ;; TBD: does it make sense for colon to be symbol class in C++?
  ;; I'm not so sure, since c-label-key is busted on lines like:
  ;; Foo::bar( i );
  ;; maybe c-label-key should be fixed instead of commenting this out,
  ;; but it also bothers me that this only seems appropriate for C++
  ;; and not C.
  ;;(modify-syntax-entry ?: "_" c++-mode-syntax-table)
  )



;;;###autoload
(defun c++-mode ()
  "Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map c++-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	comment-multi-line nil
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
	c-class-key c-C++-class-key
	c-access-key c-C++-access-key
	c-double-slash-is-comments-p t
	c-recognize-knr-p nil
	imenu-generic-expression cc-imenu-c++-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c++-mode-hook))

(setq c-list-of-mode-names (cons "C++" c-list-of-mode-names))


;; Support for Objective-C


(defvar objc-mode-abbrev-table nil
  "Abbrev table in use in objc-mode buffers.")
(define-abbrev-table 'objc-mode-abbrev-table ())

(defvar objc-mode-map ()
  "Keymap used in objc-mode buffers.")
(if objc-mode-map
    nil
  (setq objc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Objective-C
  (define-key objc-mode-map "/"      'c-electric-slash))

(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments objc-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table)
  )



;;;###autoload
(defun objc-mode ()
  "Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `objc-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the hook `c-mode-common-hook'
is run first.

Key bindings:
\\{objc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table objc-mode-syntax-table)
  (setq major-mode 'objc-mode
	mode-name "ObjC"
	local-abbrev-table objc-mode-abbrev-table)
  (use-local-map objc-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end   ""
	comment-multi-line nil
	c-conditional-key c-C-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
 	c-class-key c-ObjC-class-key
	c-baseclass-key nil
	c-access-key c-ObjC-access-key
	c-double-slash-is-comments-p t
	c-method-key c-ObjC-method-key)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'objc-mode-hook))

(setq c-list-of-mode-names (cons "ObjC" c-list-of-mode-names))



;; Support for Java

(defvar java-mode-abbrev-table nil
  "Abbrev table in use in java-mode buffers.")
(define-abbrev-table 'java-mode-abbrev-table ())

(defvar java-mode-map ()
  "Keymap used in java-mode buffers.")
(if java-mode-map
    nil
  (setq java-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Java
  (define-key java-mode-map "/"      'c-electric-slash))

(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments java-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" java-mode-syntax-table)
  )



;;;###autoload
(defun java-mode ()
  "Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `java-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"java\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{java-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'java-mode
 	mode-name "Java"
 	local-abbrev-table java-mode-abbrev-table)
  (use-local-map java-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
 	comment-multi-line nil
 	c-conditional-key c-Java-conditional-key
 	c-comment-start-regexp c-Java-comment-start-regexp
  	c-class-key c-Java-class-key
	c-method-key c-Java-method-key
	c-double-slash-is-comments-p t
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key c-Java-access-key
	;defun-prompt-regexp c-Java-defun-prompt-regexp
	imenu-generic-expression cc-imenu-java-generic-expression
	)
  (c-set-style "java")
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'java-mode-hook))

(setq c-list-of-mode-names (cons "Java" c-list-of-mode-names))



(provide 'cc-langs)
;;; cc-langs.el ends here
