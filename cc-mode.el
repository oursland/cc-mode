;;; cc-mode.el --- major mode for editing C++ and C code

;; Authors: 1992 Barry A. Warsaw, Century Computing Inc. <bwarsaw@cen.com>
;;          1987 Dave Detlefs and Stewart Clamen
;;          1985 Richard M. Stallman
;; Maintainer: cc-mode-help@anthem.nlm.nih.gov
;; Created: a long, long, time ago. adapted from the original c-mode.el
;; Version:         $Revision: 3.202 $
;; Last Modified:   $Date: 1994-01-25 14:05:43 $
;; Keywords: C++ C editing major-mode

;; Copyright (C) 1992, 1993, 1994 Barry A. Warsaw
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is intended to be a nearly interchangeable replacement
;; for standard c-mode (a.k.a. BOCM -- "Boring Old C-Mode" :-).  There
;; are some important differences.  Briefly: complete K&R C, ANSI C,
;; and C++ support with consistent indentation across all modes, more
;; intuitive indentation controlling variables, compatibility across
;; all known Emacsen, nice new features, and tons of bug fixes.  This
;; package is called CC-MODE to distinguish it from BOCM and its
;; ancestor C++-MODE, but there really is no top-level CC-MODE (see
;; below).  cc-mode.el is not compatible with c-mode.el or
;; c++-mode.el.  You should use this file to edit all your C and C++
;; code. 

;; Details on CC-MODE are now (or will soon be) contained in an
;; accompanying texinfo manual (cc-mode.texi).  To submit bug reports,
;; hit "C-c C-b", and please try to include a code sample so I can
;; reproduce your problem.  If you have other questions contact me at
;; the following address: cc-mode-help@anthem.nlm.nih.gov.  Please
;; don't send bug reports to my personal account, I may not get it for
;; a long time.

;; YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS. They are the result of
;; the multi-Emacsen support. FSF 19, Lucid 19, and GNU 18 all do
;; things differently and there's no way to shut the byte-compiler up
;; at the necessary granularity.

;; If your Emacs is dumped with either c-mode.el or c++-mode.el, you
;; will need to add the following to your .emacs file before any other
;; reference to c-mode or c++-mode:
;;
;; (fmakunbound 'c-mode)
;; (makunbound 'c-mode-map)
;; (fmakunbound 'c++-mode)
;; (makunbound 'c++-mode-map)

;; There are two major mode entry points provided by this package, one
;; for editing C++ code and the other for editing C code (both K&R and
;; ANSI).  To use CC-MODE, add the following to your .emacs file.
;; This assumes you will use .cc or .C extensions for your C++ source,
;; and .c for your C code:
;;
;; (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;; (autoload 'c-mode   "cc-mode" "C Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.C$"  . c++-mode)
;;             ("\\.cc$" . c++-mode)
;;             ("\\.c$"  . c-mode)   ; to edit C code
;;             ("\\.h$"  . c-mode)   ; to edit C code
;;            ) auto-mode-alist))
;;
;; If you would like to join the beta testers list, send add/drop
;; requests to cc-mode-victims-request@anthem.nlm.nih.gov.
;; Discussions go to cc-mode-victims@anthem.nlm.nih.gov, but bug
;; reports and such should still be sent to cc-mode-help only (see
;; above).
;;
;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, and code contributions,
;; and encouragement cc-mode.el would be a far inferior package.

;; LCD Archive Entry:
;; cc-mode.el|Barry A. Warsaw|cc-mode-help@anthem.nlm.nih.gov
;; |Major mode for editing C++, and ANSI/K&R C code
;; |$Date: 1994-01-25 14:05:43 $|$Revision: 3.202 $|

;;; Code:


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar c-strict-semantics-p nil
  "*If non-nil, all semantic symbols must be found in `c-offsets-alist'.
If the semantic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the semantic symbol is ignored.")
(defvar c-echo-semantic-information-p nil
  "*If non-nil, semantic info is echoed when the line is indented.")
(defvar c-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `c-offsets-alist'.")
(defvar c-offsets-alist
  '((string                . -1000)
    (c                     . c-lineup-C-comments)
    (defun-open            . 0)
    (defun-close           . 0)
    (class-open            . 0)
    (class-close           . 0)
    (inline-open           . +)
    (inline-close          . 0)
    (c++-funcdecl-cont     . -)
    (knr-argdecl-intro     . +)
    (knr-argdecl           . 0)
    (topmost-intro         . 0)
    (topmost-intro-cont    . 0)
    (member-init-intro     . +)
    (member-init-cont      . 0)
    (inher-intro           . +)
    (inher-cont            . c-lineup-multi-inher)
    ;;some people like this behavior instead
    ;;(block-open            . c-adaptive-block-open)
    (block-open            . 0)
    (block-close           . 0)
    (brace-list-open       . 0)
    (brace-list-close      . 0)
    (brace-list-intro      . +)
    (brace-list-entry      . 0)
    (statement             . 0)
    (statement-cont        . +)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (substatement          . +)
    (case-label            . 0)
    (access-label          . -)
    (label                 . 2)
    (do-while-closure      . 0)
    (else-clause           . 0)
    (comment-intro         . c-indent-for-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close         . +)
    (stream-op             . c-lineup-streamop)
    (inclass               . +)
    (cpp-macro             . -1000)
    )
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-ELEMENT . OFFSET)

When a line is indented, cc-mode first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `c-semantics' contains the context list for the
line being indented.  Each element in this list is actually a cons
cell of the syntactic symbol and a buffer position.  This buffer
position is call the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, cc-mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-ELEMENT's in `c-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `c-offsets-alist',
an error is generated if `c-strict-semantics-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, or the symbol `+' or
`-', the latter designating positive or negative values of
`c-basic-offset'. If OFFSET is a function, it is called with a single
argument containing the cons of the syntactic element symbol and the
relative indent point.  The function should return an integer offset.

Here is the current list of valid semantic element symbols:

 string                 -- inside multi-line string
 c                      -- inside a multi-line C style block comment
 defun-open             -- brace that opens a function definition
 defun-close            -- brace that closes a function definition
 class-open             -- brace that opens a class definition
 class-close            -- brace that closes a class definition
 inline-open            -- brace that opens an in-class inline method
 inline-close           -- brace that closes an in-class inline method
 c++-funcdecl-cont      -- the nether region between a C++ function
                           declaration and the defun opening brace
 knr-argdecl-intro      -- first line of a K&R C argument declaration
 knr-argdecl            -- subsequent lines in a K&R C argument declaration
 topmost-intro          -- the first line in a topmost construct definition
 topmost-intro-cont     -- topmost definition continuation lines
 member-init-intro      -- first line in a member initialization list
 member-init-cont       -- subsequent member initialization list lines
 inher-intro            -- first line of a multiple inheritance list
 inher-cont             -- subsequent multiple inheritance lines
 block-open             -- statement block open brace
 block-close            -- statement block close brace
 brace-list-open        -- open brace of an enum or static array list
 brace-list-close       -- close brace of an enum or static array list
 brace-list-intro       -- first line in an enum or static array list
 brace-list-entry       -- subsequent lines in an enum or static array list
 statement              -- a C/C++ statement
 statement-cont         -- a continuation of a C/C++ statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case `block'
 substatement           -- the first line after an if/while/for/do/else
 case-label             -- a case or default label
 access-label           -- C++ private/protected/public access label
 label                  -- any non-special C/C++ label
 do-while-closure       -- the `while' that ends a do/while construct
 else-clause            -- the `else' of an if/else construct
 comment-intro          -- a line containing only a comment introduction
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 stream-op              -- lines continuing a stream operator construct
 inclass                -- the construct is nested inside a class definition
 cpp-macro              -- the start of a cpp macro
")

(defvar c-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a real tab character.
If other than nil or t, then tab is inserted only within literals
-- defined as comments and strings -- and inside preprocessor
directives, but line is always reindented.")

(defvar c-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)")

(defvar c-block-comments-indent-p nil
  "*Specifies how to re-indent C style block comments.

4 styles of C block comments are supported.  If this variable is nil,
then styles 1-3 are supported.  If this variable is non-nil, style 4
only is supported.  Note that this currently has *no* effect on how
comments are lined up or whether stars are inserted when C comments
are auto-filled.  In any case, you still have to insert the stars
manually.

 style 1:       style 2:       style 3:       style 4:
 /*             /*             /*             /*
    blah         * blah        ** blah        blah
    blah         * blah        ** blah        blah
    */           */            */             */")

(defvar c-cleanup-list '(scope-operator)
  "*List of various C/C++ constructs to \"clean up\".
These clean ups only take place when the auto-newline feature is turned
on, as evidenced by the `/a' or `/ah' appearing next to the mode name.
Valid symbols are:

 brace-else-brace    -- cleans up `} else {' constructs by placing entire
                        construct on a single line.  This clean up only
                        takes place when there is nothing but white
                        space between the braces and the `else'.  Clean
			up occurs when the open-brace after the `else'
			is typed.
 empty-defun-braces  -- cleans up empty defun braces by placing the
                        braces on the same line.  Clean up occurs when
			the defun closing brace is typed.
 defun-close-semi    -- cleans up the terminating semi-colon on defuns
			by placing the semi-colon on the same line as
			the closing brace.  Clean up occurs when the
			semi-colon is typed.
 list-close-comma    -- cleans up commas following braces in array
                        and aggregate initializers.  Clean up occurs
			when the comma is typed.
 scope-operator      -- cleans up double colons which may designate
			a C++ scope operator split across multiple
			lines. Note that certain C++ constructs can
			generate ambiguous situations.  This clean up
			only takes place when there is nothing but
			whitespace between colons. Clean up occurs
			when the second colon is typed.")

(defvar c-hanging-braces-alist '((brace-list-open))
  "*Controls the insertion of newlines before and after open braces.
This variable contains an association list with elements of the
following form: (SYNTACTIC-ELEMENT . (NL-LIST)).

SYNTACTIC-ELEMENT can be any of: defun-open, class-open, inline-open,
block-open, or brace-list-open. See `c-offsets-alist' for details.

NL-LIST can contain any combination of the symbols `before' or
`after'. It also be nil.  When an open brace is inserted, the
syntactic context it defines is looked up in this list, and if found, the
NL-LIST is used to determine where newlines are inserted.  If the
language element for this brace is not found in this list, the default
behavior is to insert a newline both before and after the brace.")

(defvar c-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (SYNTACTIC-ELEMENT . (NL-LIST)).

SYNTACTIC-ELEMENT can be any of: member-init-intro, inher-intro,
case-label, label, and access-label. See `c-offsets-alist' for
details.

NL-LIST can contain any combination of the symbols `before' or
`after'. It also be nil.  When a colon is inserted, the language
element that it defines is looked up in this list, and if found, the
NL-LIST is used to determine where newlines are inserted.  If the
language element for the colon is not found in this list, the default
behavior is to not insert any newlines.")

(defvar c-untame-characters '(?\')
  "*Utilize a backslashing workaround of an Emacs18 syntax deficiency.
If non-nil, this variable should contain a list of characters which
are prepended by a backslash in comment regions.  By default, the list
contains only the most troublesome character, the single quote.  To be
completely safe, set this variable to:

    '(?\( ?\) ?\' ?\{ ?\} ?\[ ?\])

This variable has no effect under Emacs 19. For details on why this is
necessary in GNU Emacs 18, please refer to the cc-mode texinfo manual.")

;; c-mode defines c-backslash-column as 48.
(defvar c-default-macroize-column 78
  "*Column to insert backslashes when macroizing a region.")
(defvar c-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")
(defvar c-delete-function 'backward-delete-char-untabify
  "*Function called by `c-electric-delete' when deleting a single char.")
(defvar c-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is `alignleft'.")
(defvar c-backscan-limit 2000
  "*Character limit for looking back while skipping syntactic whitespace.
This variable has no effect under Emacs 19.  For details on why this
is necessary under GNU Emacs 18, please refer to the texinfo manual.")

(defconst c-style-alist
  '(("GNU"
     ;(c-indent-level               .  2)
     ;(c-argdecl-indent             .  5)
     ;(c-brace-offset               .  0)
     ;(c-label-offset               . -2)
     ;(c-continued-statement-offset .  2)
     (c-basic-offset . 2)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (block-open . 0)
			 (label . -)
			 (statement-cont . +)
			 ))
     )
    ("K&R"
     ;(c-indent-level               .  5)
     ;(c-argdecl-indent             .  0)
     ;(c-brace-offset               . -5)
     ;(c-label-offset               . -5)
     ;(c-continued-statement-offset .  5)
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (block-open . -)
			 (label . -)
			 (statement-cont . +)
			 ))
     )
    ("BSD"
     ;(c-indent-level               .  4)
     ;(c-argdecl-indent             .  4)
     ;(c-brace-offset               . -4)
     ;(c-label-offset               . -4)
     ;(c-continued-statement-offset .  4)
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (block-open . -)
			 (label . -)
			 (statement-cont . +)
			 ))
     )
    ("Stroustrup"
     ;(c-indent-level               . 4)
     ;(c-continued-statement-offset . 4)
     ;(c-brace-offset               . -4)
     ;(c-argdecl-indent             . 0)
     ;(c-label-offset               . -4)
     ;(c-auto-newline               . t)
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (block-open . -)
			 (label . -)
			 (statement-cont . +)
			 ))
     )
    ("Whitesmith"
     ;(c-indent-level               .  4)
     ;(c-argdecl-indent             .  4)
     ;(c-brace-offset               .  0)
     ;(c-label-offset               . -4)
     ;(c-continued-statement-offset .  4)
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (block-open . 0)
			 (label . -)
			 (statement-cont . +)
			 ))

     ))
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any cc-mode variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `c-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-ELEMENT . VALUE)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.")



;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst c-emacs-features
  (let ((mse-spec 'no-dual-comments)
	(scanner 'v18)
	flavor)
    ;; vanilla GNU18/Epoch 4 uses default values
    (if (= 8 (length (parse-partial-sexp (point) (point))))
	;; we know we're using v19 style dual-comment specifications.
	;; All Lemacsen use 8-bit modify-syntax-entry flags, as do all
	;; patched FSF19 (obsolete), GNU18, Epoch4's.  Only vanilla
	;; FSF19 uses 1-bit flag.  Lets be as smart as we can about
	;; figuring this out.
	(let ((table (copy-syntax-table)))
	  (modify-syntax-entry ?a ". 12345678" table)
	  (if (= (logand (lsh (aref table ?a) -16) 255) 255)
	      (setq mse-spec '8-bit)
	    (setq mse-spec '1-bit))
	  ;; we also know we're using a quicker, built-in comment
	  ;; scanner, but we don't know if its old-style or new.
	  ;; Fortunately we can ask emacs directly
	  (if (fboundp 'forward-comment)
	      (setq scanner 'v19)
	    ;; we no longer support older Lemacsen
	    (error "cc-mode no longer supports pre 19.8 Lemacsen. Upgrade!"))
	  ;; find out what flavor of Emacs 19 we're using
	  (if (string-match "Lucid" emacs-version)
	      (setq flavor 'Lucid)
	    (setq flavor 'FSF))
	  ))
    ;; now cobble up the necessary list
    (list mse-spec scanner flavor))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by cc-mode.  Here's the current
supported list, along with the values for this variable:

 Vanilla GNU 18/Epoch 4:   (no-dual-comments v18)
 GNU 18/Epoch 4 (patch2):  (8-bit v19 FSF)
 Lucid Emacs 19:           (8-bit v19 Lucid)
 FSF Emacs 19:             (1-bit v19 FSF).")

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in c++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c-mode-abbrev-table nil
  "Abbrev table in use in c-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    ()
  ;; TBD: should we even worry about naming this keymap. My vote: no,
  ;; because FSF and Lucid do it differently.
  (setq c-mode-map (make-sparse-keymap))
  ;; put standard keybindings into MAP
  ;; the following mappings correspond more or less directly to BOCM
  (define-key c-mode-map "{"         'c-electric-brace)
  (define-key c-mode-map "}"         'c-electric-brace)
  (define-key c-mode-map ";"         'c-electric-semi&comma)
  (define-key c-mode-map "#"         'c-electric-pound)
  (define-key c-mode-map ":"         'c-electric-colon)
  ;; Lemacs 19.9 defines these two, the second of which is commented out
  ;; (define-key c-mode-map "\e{" 'c-insert-braces)
  ;; Commented out electric square brackets because nobody likes them.
  ;; (define-key c-mode-map "[" 'c-insert-brackets)
  (define-key c-mode-map "\e\C-h"    'c-mark-function)
  (define-key c-mode-map "\e\C-q"    'c-indent-exp)
  (define-key c-mode-map "\ea"       'c-beginning-of-statement)
  (define-key c-mode-map "\ee"       'c-end-of-statement)
  ;; I'd rather use an adaptive fill program instead of this.
  (define-key c-mode-map "\eq"       'c-fill-paragraph)
  (define-key c-mode-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-map "\C-c\C-u"  'c-up-conditional)
  (define-key c-mode-map "\t"        'c-indent-command)
  (define-key c-mode-map "\177"      'c-electric-delete)
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key c-mode-map ","         'c-electric-semi&comma)
  (define-key c-mode-map "/"         'c-electric-slash)
  (define-key c-mode-map "*"         'c-electric-star)
  (define-key c-mode-map "\e\C-x"    'c-indent-defun)
  (define-key c-mode-map "\C-c\C-\\" 'c-macroize-region)
  ;; TBD: where if anywhere, to put c-backward|forward-into-nomenclature
  (define-key c-mode-map "\C-c\C-a"  'c-toggle-auto-state)
  (define-key c-mode-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-map "\C-c\C-c"  'c-comment-region)
  (define-key c-mode-map "\C-c\C-h"  'c-toggle-hungry-state)
  (define-key c-mode-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-map "\C-c\C-s"  'c-show-semantic-information)
  (define-key c-mode-map "\C-c\C-t"  'c-toggle-auto-hungry-state)
  (define-key c-mode-map "\C-c\C-v"  'c-version)
  ;; old Emacsen need to tame certain characters
  (if (memq 'v18 c-emacs-features)
      (progn
	(define-key c-mode-map "\C-c'" 'c-tame-comments)
	(define-key c-mode-map "'"     'c-tame-insert)
	(define-key c-mode-map "["     'c-tame-insert)
	(define-key c-mode-map "]"     'c-tame-insert)
	(define-key c-mode-map "("     'c-tame-insert)
	(define-key c-mode-map ")"     'c-tame-insert)))
  ;; FSF Emacs 19 defines menus in the mode map
  (if (memq 'FSF c-emacs-features)
      (progn
	(define-key c-mode-map [menu-bar] (make-sparse-keymap))

	(define-key c-mode-map [menu-bar c]
	  (cons "C/C++" (make-sparse-keymap "C/C++")))

	(define-key c-mode-map [menu-bar c comment-region]
	  '("Comment Out Region" . comment-region))
	(define-key c-mode-map [menu-bar c c-macro-expand]
	  '("Macro Expand Region" . c-macro-expand))
	(define-key c-mode-map [menu-bar c c-backslash-region]
	  '("Backslashify" . c-macroize-region))
	(define-key c-mode-map [menu-bar c indent-exp]
	  '("Indent Expression" . c-indent-exp))
	(define-key c-mode-map [menu-bar c indent-line]
	  '("Indent Line" . c-indent-command))
	(define-key c-mode-map [menu-bar c fill]
	  '("Fill Comment Paragraph" . c-fill-paragraph))
	(define-key c-mode-map [menu-bar c up]
	  '("Up Conditional" . c-up-conditional))
	(define-key c-mode-map [menu-bar c backward]
	  '("Backward Conditional" . c-backward-conditional))
	(define-key c-mode-map [menu-bar c forward]
	  '("Forward Conditional" . c-forward-conditional))
	(define-key c-mode-map [menu-bar c backward-stmt]
	  '("Backward Statement" . c-beginning-of-statement))
	(define-key c-mode-map [menu-bar c forward-stmt]
	  '("Forward Statement" . c-end-of-statement))

	;; I do not know how to make FSF Emacs pop up a menu on 3rd
	;; button down.
	)
    ;; in Lucid 19, we want the menu to popup when the 3rd button is
    ;; hit
    (if (memq 'Lucid c-emacs-features)
	(define-key c-mode-map 'button3 'c-popup-menu)))
  )

(defvar c++-mode-map ()
  "Keymap used in c++-mode buffers.")
(if c++-mode-map
    ()
  ;; In Emacs 19, it makes more sense to inherit c-mode-map
  (if (memq 'v19 c-emacs-features)
      ;; Lucid and FSF 19 do this differently
      (if (memq 'FSF c-emacs-features)
	  (setq c++-mode-map (cons 'keymap c-mode-map))
	(setq c++-mode-map (make-sparse-keymap))
	(set-keymap-parent c++-mode-map c-mode-map))
    ;; Do it the hard way for GNU18 -- given by JWZ
    (setq c++-mode-map (nconc (make-sparse-keymap) c-mode-map)))
  ;; add bindings which are only useful for C++
  (define-key c++-mode-map "\C-c\C-;"  'c-scope-operator)
  )

(defun c-populate-syntax-table (table)
  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
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
  ;; comment syntax
  (cond
   ((eq major-mode 'c-mode)
    (modify-syntax-entry ?/  ". 14"  table)
    (modify-syntax-entry ?*  ". 23"  table))
   ;; the rest are for C++'s dual comments
   ((memq '8-bit c-emacs-features)
    ;; Lucid emacs has the best implementation
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table))
   ((memq '1-bit c-emacs-features)
    ;; FSF19 does things differently, but we can work with it
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table))
   (t
    ;; Vanilla GNU18 doesn't support mult-style comments.  We'll do
    ;; the best we can, but some strange behavior may be encountered.
    ;; PATCH or UPGRADE!
    (modify-syntax-entry ?/  ". 124" table)
    (modify-syntax-entry ?*  ". 23"  table)
    (modify-syntax-entry ?\n ">"     table))
   ))

(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table))

(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table))

(defvar c-hungry-delete-key nil
  "Internal state of hungry delete key feature.")
(defvar c-auto-newline nil
  "Internal state of auto newline feature.")
(defvar c-auto-hungry-string nil
  "Internal auto-newline/hungry-delete designation string for mode line.")
(defvar c-semantics nil
  "Variable containing semantics list during indentation.")

(make-variable-buffer-local 'c-auto-newline)
(make-variable-buffer-local 'c-hungry-delete-key)
(make-variable-buffer-local 'c-auto-hungry-string)

;; cmacexp is lame because it uses no preprocessor symbols.
;; It isn't very extensible either -- hardcodes /lib/cpp.
;; [I add it here only because c-mode has it -- BAW]]
(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)


;; constant regular expressions for looking at various constructs
(defconst c-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a C/C++ symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")
(defconst c-class-key
  (concat
   "\\(\\(extern\\|typedef\\)\\s +\\)?"
   "\\(template\\s *<[^>]*>\\s *\\)?"
   "\\<\\(class\\|struct\\|union\\)\\>")
  "Regexp describing a class declaration, including templates.")
(defconst c-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  c-class-key "[ \t]+" c-symbol-key
	  "\\([ \t]*:[ \t]*\\)?\\s *[^;]")
  "Regexp describing a class inheritance declaration.")
(defconst c-protection-key
  "\\<\\(public\\|protected\\|private\\)\\>"
  "Regexp describing protection keywords.")
(defconst c-baseclass-key
  (concat
   ":?[ \t]*\\(virtual[ \t]+\\)?\\("
   c-protection-key "[ \t]+\\)" c-symbol-key)
  "Regexp describing base classes in a derived class definition.")
(defconst c-switch-label-key
  "\\(\\(case[ \t]+\\(\\w\\|\\s_\\|[-']\\)+\\)\\|default\\)[ \t]*:"
  "Regexp describing a switch's case or default label")
(defconst c-access-key
  (concat c-protection-key ":")
  "Regexp describing access specification keywords.")
(defconst c-label-key
  (concat c-symbol-key ":\\([^:]\\|$\\)")
  "Regexp describing any label.")
(defconst c-conditional-key
  "\\<\\(for\\|if\\|do\\|else\\|while\\|switch\\)\\>"
  "Regexp describing a conditional control.")

;; main entry points for the modes
;;;###autoload
(defun c++-mode ()
  "Major mode for editing C++ code.
cc-mode Revision: $Revision: 3.202 $
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducable test case and send the
message.

Note that the details of configuring c++-mode have been moved to
the accompanying texinfo manual.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the common hook
c-mode-common-hook is run first,  both by this defun, and `c-mode'.

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
	comment-end "")
  (run-hooks 'c++-mode-hook))

;;;###autoload
(defun c-mode ()
  "Major mode for editing K&R and ANSI C code.
cc-mode Revision: $Revision: 3.202 $
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducable test case and send the message.

Note that the details of configuring c-mode have been moved to
the accompanying texinfo manual.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the common hook
c-mode-common-hook is run first, both by this defun and `c++-mode'.

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
	comment-end   " */")
  (run-hooks 'c-mode-hook))

(defun c-common-init ()
  ;; Common initializations for c++-mode and c-mode.
  ;; make local variables
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
  ;; now set their values
  (setq paragraph-start (concat "^$\\|" page-delimiter)
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments (not (memq 'v18 c-emacs-features))
	indent-line-function 'c-indent-via-language-element
	indent-region-function 'c-indent-region
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *")
  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (if (boundp 'comment-indent-function)
      (progn
	   (make-local-variable 'comment-indent-function)
	   (setq comment-indent-function 'c-comment-indent))
    (make-local-variable 'comment-indent-hook)
    (setq comment-indent-hook 'c-comment-indent))
  ;; put C menu into menubar for Lucid 19. I think this happens
  ;; automatically for FSF 19.
  (if (memq 'Lucid c-emacs-features)
      (and current-menubar
	   (progn
	     (set-buffer-menubar default-menubar)
	     (add-menu nil "C/C++" c-mode-menu))))
  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist)))
  (run-hooks 'c-mode-common-hook))


;; macros must be defined before first use
(defmacro c-point (position)
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
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  (` (let ((here (point)))
       (,@ (cond
	    ((eq position 'bol)  '((beginning-of-line)))
	    ((eq position 'eol)  '((end-of-line)))
	    ((eq position 'bod)  '((beginning-of-defun)))
	    ((eq position 'boi)  '((back-to-indentation)))
	    ((eq position 'bonl) '((forward-line 1)))
	    ((eq position 'bopl) '((forward-line -1)))
	    ((eq position 'iopl)
	     '((forward-line -1)
	       (back-to-indentation)))
	    ((eq position 'ionl)
	     '((forward-line 1)
	       (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position))
	    ))
       (prog1
	   (point)
	 (goto-char here))
       ;; workaround for an Emacs18 bug -- blech! Well, at least it
       ;; doesn't hurt for v19
       (,@ nil)
       )))

(defmacro c-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  (` (and c-auto-newline
	  (not (c-in-literal))
	  (not (newline)))))

(defmacro c-safe (body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 ((,@ body))
       (error nil))))

(defmacro c-insert-and-tame (arg)
  ;; insert last-command-char in the buffer and possibly tame it
  (` (progn
       (and (memq 'v18 c-emacs-features)
	  (memq literal '(c c++))
	  (memq last-command-char c-untame-characters)
	  (insert "\\"))
       (self-insert-command (prefix-numeric-value arg))
       )))


;; This is used by indent-for-comment to decide how much to indent a
;; comment in C code based on its context.
(defun c-comment-indent ()
  (if (looking-at "^\\(/\\*\\|//\\)")
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: A comment following a solitary close-brace should
	 ;; have only one space.
	 ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\|//\\)")
	  (search-forward "}")
	  (1+ (current-column)))
	 ;; CASE 2: 2 spaces after #endif
	 ((or (looking-at "^#[ \t]*endif[ \t]*")
	      (looking-at "^#[ \t]*else[ \t]*"))
	  7)
	 ;; CASE 3: use comment-column if previous line is a
	 ;; comment-only line indented to the left of comment-column
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at "/\\*\\|//")
	      (setq placeholder (point))))
	  (goto-char placeholder)
	  (if (< (current-column) comment-column)
	      comment-column
	    (current-column)))
	 ;; CASE 4: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 5: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))


;; active regions, and auto-newline/hungry delete key
(defun c-keep-region-active ()
  ;; do whatever is necessary to keep the region active. ignore
  ;; byte-compiler warnings you might see
  (if (boundp 'zmacs-region-stays)
      (setq zmacs-region-stays t)
    (if (boundp 'deactivate-mark)
	(setq deactivate-mark (not mark-active))
      )))

(defun c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  (setq c-auto-hungry-string
	(if c-auto-newline
	    (if c-hungry-delete-key "/ah" "/a")
	  (if c-hungry-delete-key "/h" nil)))
  ;; updates the modeline for all Emacsen
  (if (memq 'v19 c-emacs-features)
      (force-mode-line-update)
    (set-buffer-modified-p (buffer-modified-p))))

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun c-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a' or
`/ah' on the modeline after the mode name) newlines are automatically
inserted after special characters such as brace, comma, semi-colon,
and colon."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h' or `/ah' on the modeline after the mode name) the delete key
gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `c-toggle-auto-state' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))


;; COMMANDS
(defun c-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-delete-function' is called."
  (interactive "P")
  (if (or (not c-hungry-delete-key)
	  arg
	  (c-in-literal))
      (funcall c-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c-delete-function 1)
	))))

(defun c-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "P")
  (if (or (c-in-literal)
	  arg
	  (not (memq 'alignleft c-electric-pound-behavior)))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-electric-brace (arg)
  "Insert a brace.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after open braces based on the value of `c-hanging-braces-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the brace, or
the brace is inserted inside a literal."
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 ;; we want to inhibit blinking the paren since this will be
	 ;; most disruptive. we'll blink it ourselves later on
	 (old-blink-paren (if (boundp 'blink-paren-function)
			      blink-paren-function
			    blink-paren-hook))
	 blink-paren-function		; emacs19
	 blink-paren-hook		; emacs18
	 semantics newlines
	 ;; shut this up
	 (c-echo-semantic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(c-insert-and-tame arg)
      (setq semantics (progn
			(newline)
			(self-insert-command (prefix-numeric-value arg))
			(c-guess-basic-semantics bod))
	    newlines (and
		      c-auto-newline
		      (or (assq (car (or (assq 'defun-open semantics)
					 (assq 'class-open semantics)
					 (assq 'inline-open semantics)
					 (assq 'brace-list-open semantics)
					 (assq 'block-open semantics)))
				c-hanging-braces-alist)
			  (if (= last-command-char ?{)
			      '(ignore before after)
			    '(ignore after)))))
      ;; does a newline go before the open brace?
      (if (memq 'before newlines)
	  ;; we leave the newline we've put in there before,
	  ;; but we need to re-indent the line above
	  (let ((pos (- (point-max) (point))))
	    (forward-line -1)
	    (c-indent-via-language-element bod)
	    (goto-char (- (point-max) pos)))
	;; must remove the newline we just stuck in
	(delete-region (- (point) 2) (1- (point)))
	;; since we're hanging the brace, we need to recalculate
	;; semantics
	(setq semantics (c-guess-basic-semantics bod)))
      ;; now adjust the line's indentation
      (c-indent-via-language-element bod semantics)
      ;; Do all appropriate clean ups
      (let ((here (point))
	    (pos (- (point-max) (point)))
	    mbeg mend)
	;; clean up empty defun braces
	(if (and c-auto-newline
		 (memq 'empty-defun-braces c-cleanup-list)
		 (= last-command-char ?\})
		 (or (assq 'defun-close semantics)
		     (assq 'class-close semantics)
		     (assq 'inline-close semantics))
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?\{))
		 ;; make sure matching open brace isn't in a comment
		 (not (c-in-literal)))
	    (delete-region (point) (1- here)))
	;; clean up brace-else-brace
	(if (and c-auto-newline
		 (memq 'brace-else-brace c-cleanup-list)
		 (= last-command-char ?\{)
		 (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		 (progn
		   (setq mbeg (match-beginning 0)
			 mend (match-end 0))
		   (= mend here))
		 (not (c-in-literal)))
	    (progn
	      (delete-region mbeg mend)
	      (insert "} else {")))
	(goto-char (- (point-max) pos))
	)
      ;; does a newline go after the brace?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (c-indent-via-language-element)))
      ;; blink the paren
      (and (= last-command-char ?\})
	   old-blink-paren
	   (save-excursion
	     (c-backward-syntactic-ws bod)
	     (if (boundp 'blink-paren-function)
		 (funcall old-blink-paren)
	       (run-hooks old-blink-paren))))
      )))
      
(defun c-electric-slash (arg)
  "Insert a slash character.
If slash is second of a double-slash C++ style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (let ((indentp (and (eq major-mode 'c++-mode)
		      (not arg)
		      (= (preceding-char) ?/)
		      (= last-command-char ?/)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-semantic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-via-language-element))))

(defun c-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (or (and (memq (c-in-literal) '(c))
			       (save-excursion
				 (skip-chars-backward "* \t")
				 (bolp)))
			  (= (preceding-char) ?/))))
	;; shut this up
	(c-echo-semantic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-via-language-element))))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline is inserted after
semicolons, but not commas.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon."
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 (here (point))
	 ;; shut this up
	 (c-echo-semantic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(c-insert-and-tame arg)
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups, reindentations, and newline insertions, but
      ;; only if c-auto-newline is turned on
      (if (not c-auto-newline) nil
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(= last-command-char ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(= last-command-char ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (= (preceding-char) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	(c-indent-via-language-element bod)
	;; newline only after semicolon, but only if that semicolon is
	;; not inside a parenthesis list (e.g. a for loop statement)
	(and (= last-command-char ?\;)
	     (condition-case nil
		 (save-excursion
		   (up-list -1)
		   (/= (following-char) ?\())
	       (error t))
	     (progn (newline) t)
	     (c-indent-via-language-element bod))
	))))

(defun c-electric-colon (arg)
  "Insert a colon.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after colons based on the value of `c-hanging-colons-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal.

This function cleans up double colon scope operators based on the
value of `c-cleanup-list'."
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 semantics newlines
	 ;; shut this up
	 (c-echo-semantic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(c-insert-and-tame arg)
      ;; lets do some special stuff with the colon character
      (setq semantics (progn
			(self-insert-command (prefix-numeric-value arg))
			(c-guess-basic-semantics bod))
	    ;; some language elements can only be determined by
	    ;; checking the following line.  Lets first look for ones
	    ;; that can be found when looking on the line with the
	    ;; colon
	    newlines
	    (and c-auto-newline
		 (or
		  (let ((langelem (or (assq 'case-label semantics)
				      (assq 'label semantics)
				      (assq 'access-label semantics))))
		    (and langelem
			 (assq (car langelem) c-hanging-colons-alist)))
		  (prog2
		      (insert "\n")
		      (let* ((semantics (c-guess-basic-semantics bod))
			     (langelem
			      (or (assq 'member-init-intro semantics)
				  (assq 'inher-intro semantics))))
			(and langelem
			     (assq (car langelem) c-hanging-colons-alist)))
		    (delete-char -1))
		  )))
      ;; indent the current line
      (c-indent-via-language-element bod semantics)
      ;; does a newline go before the colon?
      (if (memq 'before newlines)
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-via-language-element bod)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (c-indent-via-language-element bod)))
      ;; we may have to clean up double colons
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and c-auto-newline
		 (memq 'scope-operator c-cleanup-list)
		 (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?:))
		 (not (c-in-literal))
		 (not (= (char-after (- (point) 2)) ?:)))
	    (delete-region (point) (1- here)))
	(goto-char (- (point-max) pos)))
      )))

(defun c-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`c-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Langelem symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)
			))
		     c-offsets-alist)
		    nil (not current-prefix-arg))
		   ))
	  (oldoff (cdr-safe (assq langelem c-offsets-alist)))
	  (offset (read-string "Offset: " (format "%s" oldoff))))
     (list langelem (cond
		     ((string-equal "+" offset) '+)
		     ((string-equal "-" offset) '-)
		     ((string-match "^-?[0-9]+$" offset)
		      (string-to-int offset))
		     ;; must be a function symbol
		     (t (intern offset))
		     )
	   current-prefix-arg)))
  (let ((entry (assq symbol c-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq c-offsets-alist (cons (cons symbol offset) c-offsets-alist))
	(error "%s is not a valid syntactic symbol." symbol))))
  (c-keep-region-active))

(defun c-set-style (style &optional global)
  "Set cc-mode variables to use one of several different indentation styles.
The arguments are a string representing the desired style and a flag
which, if non-nil, means to set the style globally.  Interactively,
the flag comes from the prefix argument.  The styles are chosen from
the `c-style-alist' variable."
  (interactive (list (completing-read "Use which C indentation style? "
                                      c-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style c-style-alist))))
    (or vars
	(error "Invalid C indentation style `%s'" style))
    ;; set all the variables
    (mapcar
     (function
      (lambda (varentry)
	(let ((var (car varentry))
	      (val (cdr varentry)))
	  (or global
	      (make-local-variable var))
	  ;; special case for c-offsets-alist
	  (if (not (eq var 'c-offsets-alist))
	      (set var val)
	    (mapcar
	     (function
	      (lambda (langentry)
		(let ((langelem (car langentry))
		      (offset (cdr langentry)))
		  (c-set-offset langelem offset)
		  )))
	     val))
	  )))
     vars)))

(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations."
  (interactive "P")
  (let* (comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t\n")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point))))))
    (if (and (eq major-mode 'c++-mode)
	     (save-excursion
	       (beginning-of-line)
	       (looking-at ".*//")))
	(let (fill-prefix
	      (paragraph-start
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next to.
	       (concat 
		paragraph-start
		"\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[ \t/*]*$"))
	      (paragraph-separate
	       (concat
		paragraph-separate
		"\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[ \t/*]*$")))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp)) (looking-at "[ \t]*//"))
	      (forward-line -1))
	    (if (not (looking-at ".*//"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.
	    (let ((endcol (current-column)))
	      (skip-chars-backward " \t")
	      (setq fill-prefix
		    (concat (make-string (- (current-column) 2) ?\ )
			    "//"
			    (make-string (- endcol (current-column)) ?\ ))))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (point)
				(save-excursion
				  (forward-line 1)
				  (while (looking-at "[ \t]*//"))
				  (forward-line 1))
				(point)))
	    (insert fill-prefix)
	    (fill-paragraph arg)
	    (delete-region (point-min)
			   (+ (point-min) (length fill-prefix))))))
    (if (or first-line
	    ;; t if we enter a comment between start of function and this line.
	    (eq (calculate-c-indent) t)
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
			  ;; calculate-c-indent-within-comment bases its value
			  ;; on the indentation of previous lines; if they're
			  ;; indented specially, it could return a column
			  ;; that's well into the current line's text.  So
			  ;; we'll take at most that many space, tab, or *
			  ;; characters, and use that as our fill prefix.
			  (let ((max-prefix-end
				 (progn
				   (move-to-column
				    (calculate-c-indent-within-comment t)
				    t)
				   (point))))
			    (beginning-of-line)
			    (skip-chars-forward " \t*" max-prefix-end)
			    (point)))

		       ;; If the comment is only one line followed by a blank
		       ;; line, calling move-to-column above may have added
		       ;; some spaces and tabs to the end of the line; the
		       ;; fill-paragraph function will then delete it and the
		       ;; newline following it, so we'll lose a blank line
		       ;; when we shouldn't.  So delete anything
		       ;; move-to-column added to the end of the line.  We
		       ;; record the line width instead of the position of the
		       ;; old line end because move-to-column might break a
		       ;; tab into spaces, and the new characters introduced
		       ;; there shouldn't be deleted.

		       ;; If you can see a better way to do this, please make
		       ;; the change.  This seems very messy to me.
		       (delete-region (progn (move-to-column line-width)
					     (point))
				      (progn (end-of-line) (point))))))))

	      (paragraph-start
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next to.
	       (concat 
		paragraph-start
		"\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[ \t/*]*$"))
	      (paragraph-separate
	       (concat
		paragraph-separate
		"\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[ \t/*]*$"))
	      (chars-to-delete 0))
	  (save-restriction
	    ;; Don't fill the comment together with the code following it.
	    ;; So temporarily exclude everything before the comment start,
	    ;; and everything after the line where the comment ends.
	    ;; If comment-start-place is non-nil, the comment starter is there.
	    ;; Otherwise, point is inside the comment.
	    (narrow-to-region (save-excursion
				(if comment-start-place
				    (goto-char comment-start-place)
				  (search-backward "/*"))
				;; Protect text before the comment start 
				;; by excluding it.  Add spaces to bring back 
				;; proper indentation of that point.
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
	      ;; Find the comment ender (should be on last line of buffer,
	      ;; given the narrowing) and don't leave it on its own line.
	      (goto-char (point-max))
	      (forward-line -1)
	      (search-forward "*/" nil 'move)
	      (beginning-of-line)
	      (if (looking-at "[ \t]*\\*/")
		  (delete-indentation)))))
      ;; Outside of comments: do ordinary filling.
      (fill-paragraph arg))))

;; better movement routines for ThisStyleOfVariablesCommonInCPlusPlus
;; originally contributed by Terry_Glanfield.Southern@rxuk.xerox.com
(defun c-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With arg, to it arg times."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward "\\W*\\([A-Z]*[a-z0-9]*\\)" (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\W\\w+\\)"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (c-keep-region-active))

(defun c-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (c-forward-into-nomenclature (- (or arg 1)))
  (c-keep-region-active))

(defun c-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive)
  (insert "::"))

;; TBD: These are from Lucid Emacs 19.9's version of c-mode.el
;;(defun c-insert-brackets ()
;;  (interactive)
;;  (insert ?[)
;;  (save-excursion
;;    (insert ?])))

;;(defun c-insert-braces ()
;;  (interactive)
;;  (setq last-command-char ?{)
;;  (electric-c-brace 1)
;;  (newline)
;;  (c-indent-line)
;;  (save-excursion
;;    (newline)
;;    (insert ?})
;;    (c-indent-line)))


(defun c-beginning-of-statement (&optional count lim)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 2 optional args: the
prefix arg, and a buffer position limit which is the farthest back to
search."
  (interactive "p")
  (let ((here (point))
	(count (or count 1))
	(lim (or lim (c-point 'bod)))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and (interactive-p)
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))
		 (save-excursion
		   (skip-chars-backward " \t")
		   (goto-char (- (point) 2))
		   (looking-at "\\*/"))))
	(forward-sentence (- count))
      (while (> count 0)
	(c-beginning-of-statement-1)
	(setq count (1- count)))
      (while (< count 0)
	(c-end-of-statement-1)
	(setq count (1+ count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    ))

(defun c-end-of-statement (&optional count lim)
  "Go to the end of the innermost C statement.

With prefix arg, go forward N - 1 statements.  Move forward to end of
the next statement if already at end.  If within a string or comment,
move by sentences instead of statements.

When called from a program, this function takes 2 optional args: the
prefix arg, and a buffer position limit which is the farthest back to
search."
  (interactive "p")
  (c-beginning-of-statement (- (or count 1) lim)))

(defun c-beginning-of-statement-1 ()
  (let ((last-begin (point))
	(first t))
    (condition-case ()
	(progn
	  (while (and (progn (c-backward-syntactic-ws) t)
		      (not (bobp))
		      (progn
			(backward-sexp 1)
			(or first
			    (not (re-search-forward "[;{}]" last-begin t))
			    ))
		      (not (or (c-in-literal)
			       (looking-at c-conditional-key)))
		      )
	    (if (and (not (c-in-literal))
		     (not (looking-at c-label-key)))
		(setq last-begin (point)
		      first nil)))
	  (cond
	   ;; CASE 1: we're in the middle of an else-if clause
	   ((save-excursion
	      (c-safe (forward-sexp -1))
	      (looking-at "\\<else\\>[ \t]+\\<if\\>"))
	    (forward-sexp -1))
	   ;; CASE 2: we're looking at any other conditional clause
	   ((looking-at c-conditional-key))
	   ;; CASE 3: anything else
	   (t (goto-char last-begin))))
      ;; error for condition-case
      (error (if first
		 (backward-up-list 1)
	       (goto-char last-begin)
	       ;; skip over any unary operators, or other special
	       ;; characters appearing at front of identifier 
	       (skip-chars-backward "-+!*&:.~")
	       ))
      )))

(defun c-end-of-statement-1 ()
  (condition-case ()
      (progn
	(while (and (not (eobp))
		    (let ((beg (point)))
		      (forward-sexp 1)
		      (let ((end (point)))
			(save-excursion
			  (goto-char beg)
			  (not (re-search-forward "[;{}]" end t)))))))
	(re-search-backward "[;}]")
	(forward-char 1))
    (error 
     (let ((beg (point)))
       (backward-up-list -1)
       (let ((end (point)))
	 (goto-char beg)
	 (search-forward ";" end 'move))))))


(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.
When going backwards, `#elif' is treated like `#else' followed by
`#if'.  When going forwards, `#elif' is ignored."
  (interactive "p")
  (c-forward-conditional (- count) t))

(defun c-backward-conditional (count &optional up-flag)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional."
  (interactive "p")
  (c-forward-conditional (- count) up-flag))

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
			(setq found (point)))))))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (push-mark)
    (goto-char new)))


;; Workarounds for GNU Emacs 18 scanning deficiencies
(defun c-tame-insert (arg)
  "Safely inserts certain troublesome characters in comment regions.
This function is only necessary in GNU Emacs 18.  For details, refer
to the accompanying texinfo manual.

See also the variable `c-untame-characters'."
  (interactive "P")
  (let ((literal (c-in-literal)))
    (c-insert-and-tame arg)))

(defun c-tame-comments ()
  "Backslashifies all untamed in comment regions found in the buffer.
This function is only necessary in GNU Emacs 18. For details, refer to
the accompanying texinfo manual.

See also the variable `c-untame-characters'."
  (interactive)
  ;; make the list into a valid charset, escaping where necessary
  (let ((charset (concat "^" (mapconcat
			      (function
			       (lambda (char)
				 (if (memq char '(?\\ ?^ ?-))
				     (concat "\\" (char-to-string char))
				   (char-to-string char))))
			      c-untame-characters ""))))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
	(skip-chars-forward charset)
	(if (and (not (zerop (following-char)))
		 (memq (c-in-literal) '(c c++))
		 (/= (preceding-char) ?\\ ))
	    (insert-char  ?\\ 1))
	(if (not (eobp))
	    (forward-char 1)))))
  (c-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional whole-exp)
  "Indent current line as C++ code, or in some cases insert a tab character.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.  If other than nil
or t, then tab is inserted only within literals (comments and strings)
and inside preprocessor directives, but line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (let ((bod (c-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c-indent-via-language-element bod))
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
	      (indent-code-rigidly beg end shift-amt "#")))
      ;; No arg supplied, use c-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (insert-tab)
	  (c-indent-via-language-element bod)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(c-indent-via-language-element bod))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-in-literal bod)
	    (insert-tab))
	(c-indent-via-language-element bod)
	))))
  (c-keep-region-active))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in block following pont.
Optional SHUTUP-P if non-nil, inhibits message printing and error checking."
  (interactive "P")
  (or (memq (following-char) '(?\( ?\[ ?\{))
      shutup-p
      (error "Character under point does not start an expression."))
  (let ((start (point))
	(bod (c-point 'bod))
	(end (progn
	       (condition-case nil
		   (forward-sexp 1)
		 (error (error "Cannot indent an unclosed expression.")))
	       (point-marker)))
	;; keep quiet for speed
	(c-echo-semantic-information-p nil))
    (or shutup-p
	(message "indenting expression... (this may take a while)"))
    (goto-char start)
    (beginning-of-line)
    (while (< (point) end)
      (if (not (looking-at "[ \t]*$"))
	  (c-indent-via-language-element bod))
      (forward-line 1))
    (or shutup-p
	(message "indenting expression... done."))
    (goto-char start)
    (set-marker end nil))
  (c-keep-region-active))

(defun c-indent-defun ()
  "Indents the current function def, struct or class declaration."
  (interactive)
  (let ((here (point-marker))
	(c-echo-semantic-information-p nil))
    (beginning-of-defun)
    (c-indent-exp)
    (goto-char here)
    (set-marker here nil))
  (c-keep-region-active))

(defun c-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (message "indenting region... (this may take a while)")
  (save-excursion
    (goto-char start)
    (let ((endmark (copy-marker end))
	  (c-tab-always-indent t)
	  (c-echo-semantic-information-p nil)
	  (lim (c-point 'bod)))
      (while (< (point) endmark)
	;; Indent one line as with TAB.
	(let (nextline sexpend sexpstart)
	  ;; indent the current line
	  (c-indent-via-language-element lim)
	  ;; Find beginning of following line.
	  (setq nextline (c-point 'bonl))
	  ;; Find first beginning-of-sexp for sexp extending past this line.
	  (beginning-of-line)
	  (while (< (point) nextline)
	    (condition-case nil
		(progn
		  (setq sexpstart (point))
		  (forward-sexp 1)
		  (setq sexpend (point-marker)))
	      (error (setq sexpend nil)
		     (goto-char nextline)))
	    (c-forward-syntactic-ws))
	  ;; If that sexp ends within the region,
	  ;; indent it all at once, fast.
	  (if (and sexpend
		   (> sexpend nextline)
		   (<= sexpend endmark))
	      (progn
		(goto-char sexpstart)
		(c-indent-exp 'shutup)
		(goto-char sexpend)))
	  ;; Move to following line and try again.
	  (and sexpend
	       (set-marker sexpend nil))
	  (forward-line 1)))
      (set-marker endmark nil)))
  (message "indenting region... done.")
  (c-keep-region-active))

(defun c-mark-function ()
  "Put mark at end of a C/C++ defun, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  ;; emacs 18/19 compatibility -- blech!
  (if (memq 'v18 c-emacs-features)
      (funcall 'push-mark)
    (funcall 'push-mark (point) nil t))
  (beginning-of-defun)
  (backward-paragraph))


;; Skipping of "syntactic whitespace" for all known Emacsen.
;; Syntactic whitespace is defined as lexical whitespace, C and C++
;; style comments, and preprocessor directives.  Search no farther
;; back or forward than optional LIM.  If LIM is omitted,
;; `beginning-of-defun' is used for backward skipping, point-max is
;; used for forward skipping.
;;
;; Emacs 19 has nice built-in functions to do this, but Emacs 18 does
;; not.  Also note that only Emacs19 implementation currently has
;; support for multi-line macros, and this feature is imposes the
;; restriction that backslashes at the end of a line can only occur on
;; multi-line macro lines.

;; This is the best we can do in vanilla GNU 18 Emacsen.
(defun c-emacs18-fsws (&optional lim)
  ;; Forward skip syntactic whitespace for Emacs 18.
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; c++ comment
       ((looking-at "//") (end-of-line))
       ;; c comment
       ((looking-at "/\\*") (re-search-forward "*/" lim 'noerror))
       ;; preprocessor directive
       ((and (= (c-point 'boi) (point))
	     (= (following-char) ?#))
	(end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(defun c-emacs18-bsws (&optional lim)
  ;; Backward skip syntactic whitespace for Emacs 18."
  (let ((lim (or lim (c-point 'bod)))
	literal stop)
    (if (and c-backscan-limit
	     (> (- (point) lim) c-backscan-limit))
	(setq lim (- (point) c-backscan-limit)))
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      ;; c++ comment
      (if (eq (setq literal (c-in-literal lim)) 'c++)
	  (progn
	    (skip-chars-backward "^/" lim)
	    (skip-chars-backward "/" lim)
	    (while (not (or (and (= (following-char) ?/)
				 (= (char-after (1+ (point))) ?/))
			    (<= (point) lim)))
	      (skip-chars-backward "^/" lim)
	      (skip-chars-backward "/" lim)))
	;; c comment
	(if (eq literal 'c)
	    (progn
	      (skip-chars-backward "^*" lim)
	      (skip-chars-backward "*" lim)
	      (while (not (or (and (= (following-char) ?*)
				   (= (preceding-char) ?/))
			      (<= (point) lim)))
		(skip-chars-backward "^*" lim)
		(skip-chars-backward "*" lim))
	      (or (bobp) (forward-char -1)))
	  ;; preprocessor directive
	  (if (eq literal 'pound)
	      (progn
		(beginning-of-line)
		(setq stop (<= (point) lim)))
	    ;; just outside of c block
	    (if (and (= (preceding-char) ?/)
		     (= (char-after (- (point) 2)) ?*))
		(progn
		  (skip-chars-backward "^*" lim)
		  (skip-chars-backward "*" lim)
		  (while (not (or (and (= (following-char) ?*)
				       (= (preceding-char) ?/))
				  (<= (point) lim)))
		    (skip-chars-backward "^*" lim)
		    (skip-chars-backward "*" lim))
		  (or (bobp) (forward-char -1)))
	      ;; none of the above
	      (setq stop t))))))))


(defun c-emacs19-accurate-fsws (&optional lim)
  ;; Forward skip of syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment 1)
	;; skip preprocessor directives
	(if (and (= (following-char) ?#)
		 (= (c-point 'boi) (point)))
	    (end-of-line)
	  )))))

(defun c-emacs19-accurate-bsws (&optional lim)
  ;; Backward skip over syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (c-point 'bod)))
	   (here lim))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment -1)
	      (if (eq (c-in-literal lim) 'pound)
		  (beginning-of-line))
	      )))
      )))


;; Return `c' if in a C-style comment, `c++' if in a C++ style
;; comment, `string' if in a string literal, `pound' if on a
;; preprocessor line, or nil if not in a comment at all.  Optional LIM
;; is used as the backward limit of the search.  If omitted, or nil,
;; `beginning-of-defun' is used."
(defun c-emacs18-il (&optional lim)
  ;; Determine if point is in a C/C++ literal
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (c-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "\\(/[/*]\\)\\|[\"']\\|\\(^[ \t]*#\\)"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a C++ style comment
	       ((string= "//" match)
		(if (<= here (progn (end-of-line) (point))) 'c++))
	       ;; looking at the opening of a C block comment
	       ((string= "/*" match)
		(if (not (re-search-forward "*/" here 'move)) 'c))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       ((string-match "[ \t]*#" match)
		(if (<= here (progn (end-of-line) (point))) 'pound))
	       (t nil)))
	) ; end-while
      state)))

;; This is for all Emacsen supporting 8-bit syntax (Lucid 19, patched GNU18)
(defun c-8bit-il (&optional lim)
  ;; Determine if point is in a C++ literal
  (save-excursion
    (let* ((lim (or lim (c-point 'bod)))
	   (here (point))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) (if (nth 7 state) 'c++ 'c))
       ((progn
	  (goto-char here)
	  (beginning-of-line)
	  (looking-at "[ \t]*#"))
	'pound)
       (t nil)))))

;; This is for all 1-bit emacsen (FSF Emacs 19)
(defun c-1bit-il (&optional lim)
  ;; Determine if point is in a C++ literal
  (save-excursion
    (let* ((lim  (or lim (c-point 'bod)))
	   (here (point))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) (if (nth 7 state) 'c++ 'c))
       ((progn
	  (goto-char here)
	  (beginning-of-line)
	  (looking-at "[ \t]*#"))
	'pound)
       (t nil)))))

;; set the compatibility for all the different Emacsen. wish we didn't
;; have to do this!  Note that pre-19.8 lemacsen are no longer
;; supported.
(fset 'c-forward-syntactic-ws
      (cond
       ((memq 'v18 c-emacs-features)     'c-emacs18-fsws)
       ((memq 'old-v19 c-emacs-features)
	(error "Old Lemacsen are no longer supported. Upgrade!"))
       ((memq 'v19 c-emacs-features)     'c-emacs19-accurate-fsws)
       (t (error "Bad c-emacs-features: %s" c-emacs-features))
       ))
(fset 'c-backward-syntactic-ws
      (cond
       ((memq 'v18 c-emacs-features)     'c-emacs18-bsws)
       ((memq 'old-v19 c-emacs-features)
	(error "Old Lemacsen are no longer supported. Upgrade!"))
       ((memq 'v19 c-emacs-features)     'c-emacs19-accurate-bsws)
       (t (error "Bad c-emacs-features: %s" c-emacs-features))
       ))
(fset 'c-in-literal
      (cond
       ((memq 'no-dual-comments c-emacs-features) 'c-emacs18-il)
       ((memq '8-bit c-emacs-features)            'c-8bit-il)
       ((memq '1-bit c-emacs-features)            'c-1bit-il)
       (t (error "Bad c-emacs-features: %s" c-emacs-features))
       ))


;; utilities for moving and querying around semantic elements
(defun c-parse-state (&optional lim)
  ;; Determinate the syntactic state of the code at point.
  ;; Iteratively uses `parse-partial-sexp' from point to LIM and
  ;; returns the result of `parse-partial-sexp' at point.  LIM is
  ;; optional and defaults to `point-max'."
  (let ((lim (or lim (point-max)))
	state)
    (while (< (point) lim)
      (setq state (parse-partial-sexp (point) lim 0)))
    state))

(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let ((lim (or lim (c-point 'bod)))
	(placeholder (progn
		       (back-to-indentation)
		       (point))))
    (c-backward-syntactic-ws lim)
    (while (and (> (point) lim)
		(memq (preceding-char) '(?, ?:))
		(progn
		  (beginning-of-line)
		  (setq placeholder (point))
		  (skip-chars-forward " \t")
		  (not (looking-at c-class-key))
		  ))
      (c-backward-syntactic-ws lim))
    (goto-char placeholder)
    (skip-chars-forward "^:" (c-point 'eol))))

(defun c-beginning-of-macro (&optional lim)
  ;; Go to the beginning of the macro. Right now we don't support
  ;; multi-line macros too well
  (back-to-indentation))

(defun c-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (c-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (= (preceding-char) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>"))
	  (c-backward-syntactic-ws)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(if (and (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws)
		   (looking-at "\\s *:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (= (preceding-char) ?\))
      )))

;; defuns to look backwards for things
(defun c-backward-to-start-of-do (&optional lim)
  ;; Move to the start of the last "unbalanced" do expression.
  ;; Optional LIM is the farthest back to search.
  (let ((do-level 1)
	(case-fold-search nil)
	(lim (or lim (c-point 'bod))))
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case nil
	  (progn
	    (backward-sexp 1)
	    (cond
	     ((memq (c-in-literal lim) '(c c++)))
	     ((looking-at "while\\b")
	      (setq do-level (1+ do-level)))
	     ((looking-at "do\\b")
	      (setq do-level (1- do-level)))
	     ((< (point) lim)
	      (setq do-level 0)
	      (goto-char lim))))
	(error
	 (goto-char lim)
	 (setq do-level 0))))))

(defun c-backward-to-start-of-if (&optional lim)
  ;; Move to the start of the last "unbalanced" if and return t.  If
  ;; none is found, and we are looking at an if clause, nil is
  ;; returned.  If none is found and we are looking at an else clause,
  ;; an error is thrown.
  (let ((if-level 1)
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(at-if (looking-at "if\\b")))
    (catch 'orphan-if
      (while (and (not (bobp))
		  (not (zerop if-level)))
	(c-backward-syntactic-ws)
	(condition-case nil
	    (backward-sexp 1)
	  (error
	   (if at-if
	       (throw 'orphan-if nil)
	     (error "Orphaned `else' clause encountered."))))
	(cond
	 ((looking-at "else\\b")
	  (setq if-level (1+ if-level)))
	 ((looking-at "if\\b")
	  (setq if-level (1- if-level)))
	 ((< (point) lim)
	  (setq if-level 0)
	  (goto-char lim))
	 ))
      t)))

(defun c-search-uplist-for-classkey (&optional search-end)
  ;; search upwards for a classkey, but only as far as we need to.
  ;; this should properly find the inner class in a nested class
  ;; situation, and in a func-local class declaration.  it should not
  ;; get confused by forward declarations.
  ;;
  ;; if a classkey was found, return a cons cell containing the point
  ;; of the class's opening brace in the car, and the class's
  ;; declaration start in the cdr, otherwise return nil.
  (condition-case nil
      (save-excursion
	(let ((search-end (or search-end (point)))
	      (lim (save-excursion
		     (beginning-of-defun)
		     (c-point 'bod)))
	      donep foundp cop state)
	  (goto-char search-end)
	  (while (not donep)
	    (setq foundp (re-search-backward c-class-key lim t))
	    (save-excursion
	      (if (and
		   foundp
		   (not (c-in-literal))
		   (setq cop (c-safe (scan-lists foundp 1 -1)))
		   (setq state (c-safe (parse-partial-sexp cop search-end)))
		   (<= cop search-end)
		   (<= 0 (nth 6 state))
		   (<= 0 (nth 0 state)))
		  (progn
		    (goto-char foundp)
		    (setq donep t
			  foundp (cons (1- cop) (c-point 'boi)))
		    )
		(setq donep (not foundp))) ;end if
	      ))			;end while
	  foundp))			;end s-e
    (error nil)))

(defun c-inside-bracelist-p (containing-sexp)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren
  (let (donep bufpos)
    (save-excursion
      (or
       ;; this will pick up enum lists
       (progn (goto-char (1- containing-sexp))
	      (c-beginning-of-statement)
	      (setq bufpos (point))
	      (and (< bufpos containing-sexp)
		   (looking-at "\\<enum\\>")))
       ;; this will pick up array/aggregate init lists, even if they
       ;; are nested
       (progn (goto-char containing-sexp)
	      (while (not donep)
		(c-backward-syntactic-ws)
		(cond
		 ;; CASE 1: we've hit the beginning of the aggregate list
		 ((= (preceding-char) ?=)
		  (c-beginning-of-statement)
		  (setq donep t
			bufpos (point)))
		 ;; CASE 2: maybe we're in a nested aggregate?
		 ((= (preceding-char) ?{)
		  (c-safe (forward-char -1)))
		 ;; CASE 3: in a nested list, after the first one
		 ;; perhaps?
		 ((and (= (preceding-char) ?,)
		       (= (char-after (- (point) 2)) ?}))
		  (forward-char -1)
		  (backward-sexp 1))
		 ;; CASE 4: nope, we're done
		 (t (setq donep t
			  bufpos nil))
		 )))
       ))
    bufpos))


;; defuns for calculating the semantic state and indenting a single
;; line of C/C++ code
(defmacro c-add-semantics (symbol &optional relpos)
  ;; a simple macro to append the semantics in symbol to the semantics
  ;; list.  try to increase performance by using this macro
  (` (setq semantics (cons (cons (, symbol) (, relpos)) semantics))))

(defun c-guess-basic-semantics (&optional lim)
  ;; guess the semantic description of the current line of C++ code.
  ;; Optional LIM is the farthest back we should search
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state literal
	  containing-sexp char-before-ip char-after-ip
	  (lim (or lim (c-point 'bod)))
	  semantics placeholder inclass-p
	  )				;end-let
      ;; narrow out the enclosing class
      (save-restriction
	(if (and (eq major-mode 'c++-mode)
		 (setq inclass-p (c-search-uplist-for-classkey)))
	    (progn
	      (narrow-to-region
	       (progn
		 (goto-char (1+ (car inclass-p)))
		 (c-forward-syntactic-ws indent-point)
		 (c-point 'bol))
	       (progn
		 (goto-char indent-point)
		 (c-point 'eol)))
	      (setq lim (point-min))))
	;; parse the state of the line
	(goto-char lim)
	(setq state (c-parse-state indent-point)
	      containing-sexp (nth 1 state)
	      literal (c-in-literal lim))
	;; cache char before and after indent point, and move point to
	;; the most like position to perform regexp tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (following-char))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; now figure out semantic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((memq literal '(string))
	  (c-add-semantics 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  (c-add-semantics literal (c-point 'bopl)))
	 ;; CASE 3: in a cpp preprocessor
	 ((eq literal 'pound)
	  (c-beginning-of-macro lim)
	  (c-add-semantics 'cpp-macro (c-point 'boi)))
	 ;; CASE 4: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 4A: we are looking at a defun, class, or
	   ;; inline-inclass method opening brace
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 4A.1: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (and (eq major-mode 'c++-mode)
				 (c-search-uplist-for-classkey (point)))))
		  (and decl
		       (setq placeholder (cdr decl)))
		  ))
	      (c-add-semantics 'class-open placeholder))
	     ;; CASE 4A.2: inline defun open
	     (inclass-p
	      (c-add-semantics 'inline-open (cdr inclass-p)))
	     ;; CASE 4A.3: brace list open
	     ((save-excursion
		(c-beginning-of-statement nil lim)
		(setq placeholder (point))
		(or (looking-at "\\<enum\\>")
		    (= char-before-ip ?=)))
	      (c-add-semantics 'brace-list-open placeholder))
	     ;; CASE 4A.4: ordinary defun open
	     (t
	      (c-add-semantics 'defun-open (c-point 'bol))
	      )))
	   ;; CASE 4B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
	    (cond
	     ;; CASE 4B.1: a member init
	     ((or (= char-before-ip ?:)
		  (= char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      (if (= char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      (c-add-semantics 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 4B.2: nether region after a C++ func decl
	     ((eq major-mode 'c++-mode)
	      (c-add-semantics 'c++-funcdecl-cont (c-point 'boi))
	      (and inclass-p (c-add-semantics 'inclass (cdr inclass-p))))
	     ;; CASE 4B.3: K&R arg decl intro
	     (t
	      (c-add-semantics 'knr-argdecl-intro (c-point 'boi))
	      (and inclass-p (c-add-semantics 'inclass (cdr inclass-p))))
	     ))
	   ;; CASE 4C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((looking-at c-baseclass-key)
	    (cond
	     ;; CASE 4C.1: non-hanging colon on an inher intro
	     ((= char-after-ip ?:)
	      (c-backward-syntactic-ws lim)
	      (c-add-semantics 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 4C.2: hanging colon on an inher intro
	     ((= char-before-ip ?:)
	      (c-add-semantics 'inher-intro (c-point 'boi))
	      (and inclass-p (c-add-semantics 'inclass (cdr inclass-p))))
	     ;; CASE 4C.3: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-semantics 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 4D: this could be a top-level compound statement or a
	   ;; member init list continuation
	   ((= char-before-ip ?,)
	    (goto-char indent-point)
	    (c-backward-syntactic-ws lim)
	    (while (and (< lim (point))
			(= (preceding-char) ?,))
	      ;; this will catch member inits with multiple
	      ;; line arglists
	      (forward-char -1)
	      (c-backward-syntactic-ws (c-point 'bol))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      ;; now continue checking
	      (beginning-of-line)
	      (c-backward-syntactic-ws lim))
	    (cond
	     ;; CASE 4D.1: hanging member init colon
	     ((= (preceding-char) ?:)
	      (goto-char indent-point)
	      (c-backward-syntactic-ws lim)
	      (c-add-semantics 'member-init-cont (c-point 'boi))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 4D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(= (following-char) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-semantics 'member-init-cont (point)))
	     ;; CASE 4D.3: perhaps a multiple inheritance line?
	     ((looking-at c-inher-key)
	      (c-add-semantics 'inher-cont-1 (c-point 'boi)))
	     ;; CASE 4D.4: perhaps a template list continuation?
	     ((save-excursion
		(skip-chars-backward "^<" lim)
		(= (preceding-char) ?<))
	      ;; we can probably indent it just like and arglist-cont
	      (c-add-semantics 'arglist-cont (point)))
	     ;; CASE 4D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement nil lim)
	      (c-add-semantics 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 4E: we are looking at a access specifier
	   ((and inclass-p
		 (looking-at c-access-key))
	    (c-add-semantics 'access-label (c-point 'bonl))
	    (c-add-semantics 'inclass (cdr inclass-p)))
	   ;; CASE 4F: we are looking at the brace which closes the
	   ;; enclosing class decl
	   ((and inclass-p
		 (= char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and
		      (condition-case nil
			  (progn (backward-sexp 1) t)
			(error nil))
		      (= (point) (car inclass-p))
		      ))))
	    (save-restriction
	      (widen)
	      (goto-char (cdr inclass-p))
	      (c-add-semantics 'class-close (c-point 'boi))))
	   ;; CASE 4G: we could be looking at subsequent knr-argdecls
	   ((and (eq major-mode 'c-mode)
		 (save-excursion
		   (c-backward-syntactic-ws lim)
		   (while (memq (preceding-char) '(?\; ?,))
		     (beginning-of-line)
		     (setq placeholder (point))
		     (c-backward-syntactic-ws lim))
		   (= (preceding-char) ?\))))
	    (goto-char placeholder)
	    (c-add-semantics 'knr-argdecl (c-point 'boi)))
	   ;; CASE 4H: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  (= (preceding-char) ?:)
			  (save-excursion
			    (backward-sexp 1)
			    (looking-at c-access-key)))
		(backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (preceding-char) '(?\; ?\}))))
	    (c-add-semantics 'topmost-intro (c-point 'bol))
	    (and inclass-p (c-add-semantics 'inclass (cdr inclass-p))))
	   ;; CASE 4I: we are at a topmost continuation line
	   (t
	    (c-add-semantics 'topmost-intro-cont (c-point 'boi))
	    (and inclass-p (c-add-semantics 'inclass (cdr inclass-p))))
	   ))				; end CASE 4
	 ;; CASE 5: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((/= (char-after containing-sexp) ?{)
	  (c-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at the first argument in an empty
	   ;; argument list
	   ((= char-before-ip ?\()
	    (goto-char containing-sexp)
	    (c-add-semantics 'arglist-intro (c-point 'boi)))
	   ;; CASE 5B: we are looking at the arglist closing paren
	   ((and (/= char-before-ip ?,)
		 (= char-after-ip ?\)))
	    (goto-char containing-sexp)
	    (c-add-semantics 'arglist-close (c-point 'boi)))
	   ;; CASE 5C: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((save-excursion
	     (goto-char containing-sexp)
	     (and (c-safe (progn (forward-sexp -1) t))
		  (looking-at "\\<for\\>")))
	    (c-beginning-of-statement 1 containing-sexp)
	    (if (= char-before-ip ?\;)
		(c-add-semantics 'statement (point))
	      (c-add-semantics 'statement-cont (point))
	      ))
	   ;; CASE 5D: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp)))
		 (save-excursion
		   (c-beginning-of-statement)
		   (skip-chars-backward " \t(")
		   (<= (point) containing-sexp)))
	    (c-add-semantics 'arglist-cont-nonempty containing-sexp))
	   ;; CASE 5E: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-add-semantics 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 6: func-local multi-inheritance line
	 ((save-excursion
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (looking-at c-baseclass-key))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 6A: non-hanging colon on an inher intro
	   ((= char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-semantics 'inher-intro (c-point 'boi)))
	   ;; CASE 6B: hanging colon on an inher intro
	   ((= char-before-ip ?:)
	    (c-add-semantics 'inher-intro (c-point 'boi)))
	   ;; CASE 6C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-semantics 'inher-cont (point))
	    )))
	 ;; CASE 7: we are inside a brace-list
	 ((setq placeholder (c-inside-bracelist-p containing-sexp))
	  (cond
	   ;; CASE 7A: we're looking at the first line in a brace-list
	   ((save-excursion
	      (goto-char indent-point)
	      (c-backward-syntactic-ws containing-sexp)
	      (= (point) (1+ containing-sexp)))
	    (goto-char containing-sexp)
	    (c-add-semantics 'brace-list-intro (c-point 'boi))
	    (if (= char-after-ip ?{)
		(c-add-semantics 'block-open)))
	   ;; CASE 7B: brace-list-close brace
	   ((and (= char-after-ip ?})
		 (c-safe (progn (forward-char 1)
				(backward-sexp 1)
				t))
		 (= (point) containing-sexp))
	    (c-add-semantics 'brace-list-close (c-point 'boi)))
	   ;; CASE 7C: this is just a later brace-list-entry
	   (t (goto-char (1+ containing-sexp))
	      (c-forward-syntactic-ws indent-point)
	      (c-add-semantics 'brace-list-entry (point))
	      (if (= char-after-ip ?{)
		  (c-add-semantics 'block-open)))
	   ))
	 ;; CASE 8: A continued statement
	 ((and (not (memq char-before-ip '(?\; ?} ?:)))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement 1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: substatement
	   ((save-excursion
	      (goto-char placeholder)
	      (and (looking-at c-conditional-key)
		   (c-safe (progn (forward-sexp
				   ;; else if()
				   (if (looking-at "\\<else\\>[ \t]+\\<if\\>")
				       3
				     ;; do and else aren't followed by parens
				     (if (looking-at "\\<\\(do\\|else\\)\\>")
					 1 2)))
				  t))
		   (progn (c-forward-syntactic-ws)
			  (>= (point) indent-point))))
	    (goto-char placeholder)
	    (c-add-semantics 'substatement (c-point 'boi))
	    (if (= char-after-ip ?{)
		(c-add-semantics 'block-open)))
	   ;; CASE 8B: open braces for class or brace-lists
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 8B.1: class-open
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (and (eq major-mode 'c++-mode)
				 (c-search-uplist-for-classkey (point)))))
		  (and decl
		       (setq placeholder (cdr decl)))
		  ))
	      (c-add-semantics 'class-open placeholder))
	     ;; CASE 8B.2: brace-list-open
	     ((or (save-excursion
		    (goto-char placeholder)
		    (looking-at "\\<enum\\>"))
		  (= char-before-ip ?=))
	      (c-add-semantics 'brace-list-open placeholder))
	     (t
	      ;; some other type of block open. one example I know of
	      ;; is a try block open but as exceptions aren't
	      ;; supported yet, I'll just this until further notice
	      (c-add-semantics 'try-block-open placeholder))
	     ))
	   ;; CASE 8C: iostream insertion or extraction operator
	   ((looking-at "<<\\|>>")
	    (goto-char placeholder)
	    (while (and (re-search-forward "<<\\|>>" indent-point 'move)
			(c-in-literal)))
	    (c-add-semantics 'stream-op (c-point 'boi)))
	   ;; CASE 8D: continued statement. find the accurate
	   ;; beginning of statement or substatement
	   (t
	    (c-beginning-of-statement nil
	     (save-excursion
	       (goto-char placeholder)
	       (and (looking-at c-conditional-key)
		    (c-safe (progn (forward-sexp 2) t))
		    (c-forward-syntactic-ws))
	       (point)))
	    (c-add-semantics 'statement-cont (point)))
	   ))
	 ;; CASE 9: an else clause?
	 ((looking-at "\\<else\\>")
	  (c-backward-to-start-of-if containing-sexp)
	  (c-add-semantics 'else-clause (c-point 'boi)))
	 ;; CASE 10: Statement. But what kind?  Lets see if its a
	 ;; while closure of a do/while construct
	 ((progn
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (and (looking-at "while\\b")
		 (save-excursion
		   (c-backward-to-start-of-do containing-sexp)
		   (setq placeholder (point))
		   (looking-at "do\\b"))
		 ))
	  (c-add-semantics 'do-while-closure placeholder))
	 ;; CASE 11: A case or default label
	 ((looking-at c-switch-label-key)
	  (goto-char containing-sexp)
	  ;; for a case label, we set relpos the first non-whitespace
	  ;; char on the line containing the switch opening brace. this
	  ;; should handle hanging switch opening braces correctly.
	  (c-add-semantics 'case-label (c-point 'boi)))
	 ;; CASE 12: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (c-add-semantics 'label (c-point 'boi)))
	 ;; CASE 13: block close brace, possibly closing the defun or
	 ;; the class
	 ((= char-after-ip ?})
	  (let ((relpos (save-excursion
			  (goto-char containing-sexp)
			  (if (/= (point) (c-point 'boi))
			      (c-beginning-of-statement nil lim))
			  (point))))
	    ;; lets see if we close a top-level construct.
	    (goto-char indent-point)
	    (skip-chars-forward " \t}")
	    (if (zerop (car (parse-partial-sexp lim (point))))
		(if inclass-p
		    (progn (goto-char relpos)
			   (c-add-semantics 'inline-close (c-point 'boi)))
		  (c-add-semantics 'defun-close relpos))
	      (goto-char relpos)
	      (c-add-semantics 'block-close (c-point 'boi))
	      )))
	 ;; CASE 14: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (c-forward-syntactic-ws indent-point)
	  ;; we want to ignore labels when skipping forward
	  (let ((ignore-re (concat c-switch-label-key "\\|" c-label-key))
		inswitch-p)
	    (while (looking-at ignore-re)
	      (if (looking-at c-switch-label-key)
		  (setq inswitch-p t))
	      (forward-line 1)
	      (c-forward-syntactic-ws indent-point))
	    (cond
	     ;; CASE 14.A: we saw a case/default statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case or default label
	     ((and inswitch-p
		   (save-excursion
		     (goto-char indent-point)
		     (c-backward-syntactic-ws containing-sexp)
		     (back-to-indentation)
		     (setq placeholder (point))
		     (looking-at c-switch-label-key)))
	      (c-add-semantics 'statement-case-intro placeholder))
	     ;; CASE 14.B: continued statement
	     ((= char-before-ip ?,)
	      (c-add-semantics 'statement-cont (c-point 'boi)))
	     ;; CASE 14.C: a question/colon construct?  But make sure
	     ;; what came before was not a label, and what comes after
	     ;; is not a globally scoped function call!
	     ((or (and (memq char-before-ip '(?: ??))
		       (save-excursion
			 (goto-char indent-point)
			 (c-backward-syntactic-ws lim)
			 (back-to-indentation)
			 (not (looking-at c-label-key))))
		  (and (memq char-after-ip '(?: ??))
		       (not (looking-at "[ \t]*::"))))
	      (c-add-semantics 'statement-cont (c-point 'boi)))
	     ;; CASE 14.D: any old statement
	     ((< (point) indent-point)
	      (c-add-semantics 'statement (c-point 'boi))
	      (if (= char-after-ip ?{)
		  (c-add-semantics 'block-open)))
	     ;; CASE 14.E: first statement in a block
	     (t (goto-char containing-sexp)
		(if (/= (point) (c-point 'boi))
		    (c-beginning-of-statement))
		(c-add-semantics 'statement-block-intro (c-point 'boi))
		(if (= char-after-ip ?{)
		    (c-add-semantics 'block-open)))
	     )))
	 ))				; end save-restriction
      ;; now we need to look at any langelem modifiers
      (goto-char indent-point)
      (skip-chars-forward " \t")
      (cond
       ;; CASE M1: look for a comment only line
       ((looking-at "\\(//\\|/\\*\\)")
	(c-add-semantics 'comment-intro))
       )
      ;; return the semantics
      semantics)))


;; indent via semantic language elements
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
      (if c-strict-semantics-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+) (setq offset c-basic-offset))
     ((eq offset '-) (setq offset (- c-basic-offset)))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (c-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

(defun c-indent-via-language-element (&optional lim semantics)
  ;; indent the curent line as C/C++ code. Optional LIM is the
  ;; farthest point back to search. Optional SEMANTICS is the semantic
  ;; information for the current line. Returns the amount of
  ;; indentation change
  (let* ((lim (or lim (c-point 'bod)))
	 (c-semantics (or semantics (c-guess-basic-semantics lim)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'c-get-offset c-semantics)))
	 (shift-amt  (- (current-indentation) indent)))
    (and c-echo-semantic-information-p
	 (message "semantics: %s, indent= %d" c-semantics indent))
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

(defun c-show-semantic-information ()
  "Show semantic information for current line."
  (interactive)
  (message "semantics: %s" (c-guess-basic-semantics))
  (c-keep-region-active))


;; Standard indentation line-ups
(defun c-lineup-arglist (langelem)
  ;; lineup the current arglist line with the arglist appearing just
  ;; after the containing paren which starts the arglist.
  (save-excursion
    (let ((containing-sexp (cdr langelem))
	  cs-curcol)
    (goto-char containing-sexp)
    (setq cs-curcol (current-column))
    (or (eolp)
	(progn
	  (forward-char 1)
	  (c-forward-syntactic-ws (c-point 'eol))
	  ))
    (if (eolp)
	2
      (- (current-column) cs-curcol)
      ))))

(defun c-lineup-streamop (langelem)
  ;; lineup stream operators
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (curcol (progn (goto-char relpos)
			  (current-column))))
      (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
      (goto-char (match-beginning 0))
      (- (current-column) curcol))))

(defun c-lineup-multi-inher (langelem)
  ;; line up multiple inheritance lines
  (save-excursion
    (let (cs-curcol
	  (eol (c-point 'eol))
	  (here (point)))
      (goto-char (cdr langelem))
      (setq cs-curcol (current-column))
      (skip-chars-forward "^:" eol)
      (skip-chars-forward " \t:" eol)
      (if (or (eolp)
	      (looking-at "/\\*\\|//"))
	  (c-forward-syntactic-ws here))
      (- (current-column) cs-curcol)
      )))

(defun c-lineup-C-comments (langelem)
  ;; line up C block comment continuation lines
  (save-excursion
    (let ((stars (progn
		   (beginning-of-line)
		   (skip-chars-forward " \t")
		   (if (looking-at "\\*\\*?")
		       (- (match-end 0) (match-beginning 0))
		     0))))
      (goto-char (cdr langelem))
      (back-to-indentation)
      (if (re-search-forward "/\\*[ \t]*" (c-point 'eol) t)
	  (goto-char (+ (match-beginning 0)
			(cond
			 (c-block-comments-indent-p 0)
			 ((= stars 1) 1)
			 ((= stars 2) 0)
			 (t (- (match-end 0) (match-beginning 0)))))))
      (current-column))))

(defun c-adaptive-block-open (langelem)
  ;; when substatement is on semantics list, return negative
  ;; c-basic-offset, otherwise return zero
  (if (assq 'substatement c-semantics)
      (- c-basic-offset)
    0))

(defun c-indent-for-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; c-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    (if (not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset)
      (or (cdr-safe c-comment-only-line-offset)
	  (car-safe c-comment-only-line-offset)
	  -1000				;jam it against the left side
	  ))))


;; commands for "macroizations" -- making C++ parameterized types via
;; macros. Also commands for commentifying regions

(defun c-backslashify-current-line (doit)
  ;; Backslashifies current line if DOIT is non-nil, otherwise
  ;; unbackslashifies the current line.
  (end-of-line)
  (if doit
      ;; Note that "\\\\" is needed to get one backslash.
      (if (not (save-excursion
		 (forward-char -1)
		 (looking-at "\\\\")))
	  (progn
	    (if (>= (current-column) c-default-macroize-column)
		(insert " \\")
	      (while (<= (current-column) c-default-macroize-column)
		(insert "\t")
		(end-of-line))
	      (delete-char -1)
	      (while (< (current-column) c-default-macroize-column)
		(insert " ")
		(end-of-line))
	      (insert "\\"))))
    (if (not (bolp))
	(progn
	  (forward-char -1)
	  (if (looking-at "\\\\")
	      (let ((kill-lines-magic nil))
		(skip-chars-backward " \t")
		(kill-line)))))
      ))

(defun c-macroize-region (beg end arg)
  "Insert backslashes at end of every line in region.
Useful for defining cpp macros.  If called with a prefix argument,
it trailing backslashes are removed."
  (interactive "r\nP")
  (save-excursion
    (let ((do-lastline-p (progn (goto-char end) (not (bolp)))))
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (not (save-excursion
		      (forward-line 1)
		      (eobp)))
	  (c-backslashify-current-line (null arg))
	  (forward-line 1)))
      (and do-lastline-p
	   (progn (goto-char end)
		  (c-backslashify-current-line (null arg))))
      ))
  (c-keep-region-active))

(defun c-comment-region (beg end arg)
  "Comment out all lines in a region between mark and current point.
This is done by inserting `comment-start' in front of each line.  With
optional universal arg (\\[universal-argument]), uncomment the
region."
  (interactive "*r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (not arg)
	  (progn
	    (while (not (eobp))
	      (insert comment-start)
	      (forward-line 1))
	    (if (eq major-mode 'c-mode)
		(insert comment-end)))
	(let ((comment-regexp
	       (if (eq major-mode 'c-mode)
		   (concat "\\s *\\(" (regexp-quote comment-start)
			   "\\|"      (regexp-quote comment-end)
			   "\\)")
		 (concat "\\s *" (regexp-quote comment-start)))))
	  (while (not (eobp))
	    (if (looking-at comment-regexp)
		(delete-region (match-beginning 0) (match-end 0)))
	    (forward-line 1)))
	)))
  (c-keep-region-active))


;; defuns for submitting bug reports

(defconst c-version "$Revision: 3.202 $"
  "cc-mode version number.")
(defconst c-mode-help-address "cc-mode-help@anthem.nlm.nih.gov"
  "Address accepting submission of bug reports.")

(defun c-version ()
  "Echo the current version of cc-mode in the minibuffer."
  (interactive)
  (message "Using cc-mode version %s" c-version)
  (c-keep-region-active))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on cc-mode."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a report on cc-mode? ")
   (reporter-submit-bug-report
    c-mode-help-address
    (concat "cc-mode version " c-version " (editing "
	    (if (eq major-mode 'c++-mode) "C++" "C")
	    " code)")
    (list
     ;; report only the vars that affect indentation
     'c-emacs-features
     'c-backscan-limit
     'c-basic-offset
     'c-offsets-alist
     'c-block-comments-indent-p
     'c-cleanup-list
     'c-comment-only-line-offset
     'c-default-macroize-column
     'c-delete-function
     'c-electric-pound-behavior
     'c-hanging-braces-alist
     'c-hanging-colons-alist
     'c-tab-always-indent
     'c-untame-characters
     'tab-width
     )
    (function
     (lambda ()
       (insert
	(if c-special-indent-hook
	    (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		    "c-special-indent-hook is set to '"
		    (format "%s" c-special-indent-hook)
		    ".\nPerhaps this is your problem?\n"
		    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	  "\n")
	)))
    ))
  (c-keep-region-active))


;; menus for Lucid
(defvar c-mode-menu
  '(["Comment Out Region"     comment-region (mark)]
    ["Macro Expand Region"    c-macro-expand (mark)]
    ["Backslashify"           c-macroize-region (mark)]
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
  "Menu for C/C++ modes.")

(defun c-popup-menu (e)
  "Pops up the C/C++ menu."
  (interactive "@e")
  (popup-menu (cons "C/C++ Mode Commands" c-mode-menu)))
    

;; fsets for compatibility with BOCM
(fset 'electric-c-brace      'c-electric-brace)
(fset 'electric-c-semi       'c-electric-semi&comma)
(fset 'electric-c-sharp-sign 'c-electric-pound)
;; there is no cc-mode equivalent for electric-c-terminator
(fset 'mark-c-function       'c-mark-function)
(fset 'indent-c-exp          'c-indent-exp)
(fset 'set-c-style           'c-set-style)
(fset 'c-backslash-region    'c-macroize-region)
;; lemacs 19.9 + font-lock + cc-mode - c++-mode lossage
(fset 'c++-beginning-of-defun 'beginning-of-defun)

;; set up bc warnings for obsolete variables, but for now lets not
;; worry about obsolete functions.  maybe later some will be important
;; to flag
(and (memq 'v19 c-emacs-features)
     (let* ((na "Nothing appropriate.")
	    (vars
	     (list
	      (cons 'c++-c-mode-syntax-table 'c-mode-syntaxt-table)
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
	      (cons 'c++-C-block-comments-indent-p 'c-block-comments-indent-p)
	      (cons 'c++-cleanup-list 'c-cleanup-list)
	      (cons 'c++-hanging-braces 'c-hanging-braces-alist)
	      (cons 'c++-hanging-member-init-colon 'c-hanging-colons-alist)
	      (cons 'c++-auto-hungry-initial-state
		    "Use `c-auto-newline' and `c-hungry-delete-key' instead.")
	      (cons 'c++-auto-hungry-toggle na)
	      (cons 'c++-relative-offset-p na)
	      (cons 'c++-untame-characters 'c-untame-characters)
	      (cons 'c++-default-macroize-column 'c-default-macroize-column)
	      (cons 'c++-special-indent-hook 'c-special-indent-hook)
	      (cons 'c++-delete-function 'c-delete-function)
	      (cons 'c++-electric-pound-behavior 'c-electric-pound-behavior)
	      (cons 'c++-backscan-limit 'c-backscan-limit)
	      (cons 'c++-hungry-delete-key 'c-hungry-delete-key)
	      (cons 'c++-auto-newline 'c-auto-newline)
	      (cons 'c++-match-header-strongly na)
	      (cons 'c++-defun-header-strong-struct-equivs na)
	      (cons 'c++-version 'c-version)
	      (cons 'c++-mode-help-address 'c-mode-help-address)
	      (cons 'c-backslash-column 'c-default-macroize-column)
	      (cons 'c-indent-level 'c-basic-offset)
	      (cons 'c-brace-imaginary-offset na)
	      (cons 'c-brace-offset 'c-offsets-alist)
	      (cons 'c-argdecl-indent 'c-offsets-alist)
	      (cons 'c-label-offset 'c-offsets-alist)
	      (cons 'c-continued-statement-offset 'c-offsets-alist)
	      (cons 'c-continued-brace-offset 'c-offsets-alist)
	      )))
       (mapcar
	(function
	 (lambda (elt)
	   (make-obsolete-variable (car elt) (cdr elt))))
	vars)))

(provide 'cc-mode)
;;; cc-mode.el ends here
