;;; cc-mode.el --- major mode for editing C++ and C code

;; Authors: 1992 Barry A. Warsaw, Century Computing Inc. <bwarsaw@cen.com>
;;          1987 Dave Detlefs and Stewart Clamen
;;          1985 Richard M. Stallman
;; Maintainer: cc-mode-help@anthem.nlm.nih.gov
;; Created: a long, long, time ago. adapted from the original c-mode.el
;; Version:         $Revision: 3.69 $
;; Last Modified:   $Date: 1993-11-20 18:10:17 $
;; Keywords: C++ C editing major-mode

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

;; All information about cc-mode is now contained in an accompanying
;; texinfo manual.  To submit bug reports, hit "C-c C-b" in a cc-mode
;; buffer, and please try to include a code sample so I can reproduce
;; your problem.  If you have other questions contact me at the
;; following address: cc-mode-help@anthem.nlm.nih.gov.  Please don't
;; send bug reports to my personal account, I may not get it for a
;; long time.

;; There are two major mode entry points provided by this package, one
;; for editing C++ code and the other for editing C code (both K&R and
;; ANSI sytle).  To use cc-mode, add the following to your .emacs
;; file.  This assumes you will use .cc or .C extensions for your C++
;; source, and .c for your C code:
;;
;; (autoload 'cc-c++-mode "cc-mode" "C++ Editing Mode" t)
;; (autoload 'cc-c-mode   "cc-mode" "C Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.C$"  . cc-c++-mode)
;;             ("\\.cc$" . cc-c++-mode)
;;             ("\\.c$"  . cc-c-mode)   ; to edit C code
;;             ("\\.h$"  . cc-c-mode)   ; to edit C code
;;            ) auto-mode-alist))
;;
;; If you want to use the default c-mode for editing C code, then just
;; omit the lines marked "to edit C code".

;; If you would like to join the beta testers list, send add/drop
;; requests to cc-mode-victims-request@anthem.nlm.nih.gov.
;; Discussions go to cc-mode-victims@anthem.nlm.nih.gov, but bug
;; reports and such should still be sent to cc-mode-help only.
;;
;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, and code contribution,
;; cc-mode.el would be a far inferior package.

;; LCD Archive Entry:
;; cc-mode|Barry A. Warsaw|cc-mode-help@anthem.nlm.nih.gov
;; |Major mode for editing C++, and ANSI/K&R C code
;; |$Date: 1993-11-20 18:10:17 $|$Revision: 3.69 $|

;;; Code:


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar cc-strict-semantics-p t
  "*If non-nil, all semantic symbols must be found in `cc-offsets-alist'.
If the semantic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the semantic symbol is ignored.")
(defvar cc-echo-semantic-information-p nil
  "*If non-nil, semantic info is echoed when the line is indented.")
(defvar cc-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `cc-offset-alist'.")
(defvar cc-offsets-alist
  '((string                . +)
    (c                     . cc-lineup-C-comments)
    (defun-open            . 0)
    (defun-close           . 0)
    (class-open            . 0)
    (class-close           . 0)
    (inline-open           . +)
    (inline-close          . 0)
    (c++-funcdecl-cont     . -)
    (knr-argdecl-intro     . +)
    (topmost-intro         . 0)
    (topmost-intro-cont    . 0)
    (member-init-intro     . +)
    (member-init-cont      . 0)
    (inher-intro           . +)
    (inher-cont            . cc-lineup-multi-inher)
    (block-open            . +)
    (block-close           . 0)
    (statement             . 0)
    (statement-cont        . +)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (case-label            . 0)
    (label                 . 2)
    (access-label          . -)
    (do-while-closure      . 0)
    (else-clause           . 0)
    (comment-intro         . cc-indent-for-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . cc-lineup-arglist)
    (arglist-close         . 0)
    (stream-op             . cc-lineup-streamop)
    (inclass               . +)
    (cpp-macro             . -1000)
    )
  "*Association list of semantic symbols and indentation offsets.
Each element in this list is a cons cell of the form:

    (SEMSYM . OFFSET)

Where SEMSYM is a semantic symbol and OFFSET is the additional offset
applied to a line containing the semantic symbol.  Here is the current
list of valid semantic symbols:

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
 topmost-intro          -- the first line in a topmost construct definition
 topmost-intro-cont     -- topmost definition continuation lines
 member-init-intro      -- first line in a member initialization list
 member-init-cont       -- subsequent member initialization list lines
 inher-intro            -- first line of a multiple inheritance list
 inher-cont             -- subsequent multiple inheritance lines
 block-open             -- statement block open brace
 block-close            -- statement block close brace
 statement              -- a C/C++ statement
 statement-cont         -- a continuation of a C/C++ statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case `block'
 case-label             -- a case or default label
 label                  -- any non-special C/C++ label
 access-label           -- C++ private/protected/public access label
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

(defvar cc-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a real tab character.
If other than nil or t, then tab is inserted only within literals
(comments and strings) and inside preprocessor directives, but line is
always reindented.")
(defvar cc-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)")
(defvar cc-C-block-comments-indent-p nil
  "*Specifies how to re-indent C style block comments.

4 styles of C block comments are supported.  If this variable is nil,
then styles 1-3 are supported.  If this variable is non-nil, style 4
only is supported.  Note that this currently has *no* effect on how
comments are lined up or whether stars are inserted when C comments
are auto-filled.  In any case, you will still have to insert the stars
manually.

 style 1:       style 2:       style 3:       style 4:
 /*             /*             /*             /*
    blah         * blah        ** blah        blah
    blah         * blah        ** blah        blah
    */           */            */             */
")
(defvar cc-cleanup-list '(scope-operator)
  "*List of various C/C++ constructs to \"clean up\".
These cleanups only take place when the auto-newline feature is turned
on, as evidenced by the `/a' or `/ah' appearing next to the mode name.
Valid symbols are:

 brace-else-brace    -- clean up `} else {' constructs by placing entire
                        construct on a single line.  This cleanup only
                        takes place when there is nothing but white
                        space between the braces and the else.  
 empty-defun-braces  -- cleans up empty defun braces by placing the
                        braces on the same line.
 defun-close-semi    -- cleans up the terminating semi-colon on defuns
			by placing the semi-colon on the same line as
			the closing brace.
 list-close-comma    -- cleans up commas following braces in array
                        and aggregate initializers.
 scope-operator      -- cleans up double colons which may be designate
                        a C++ scope operator split across multiple
			lines. Note that certain C++ constructs can
			generate ambiguous situations.")

(defvar cc-hanging-braces-alist nil
  "*Controls the insertion of newlines before and after open braces.
This variable contains an association list with elements of the
following form: (LANGSYM . (NL-LIST)).

LANGSYSM can be any of: defun-open, class-open, inline-open, and
block-open (as defined by the `cc-offset-alist' variable).

NL-LIST can contain any combination of the symbols `before' or
`after'. It also be nil.  When an open brace is inserted, the language
element that it defines is looked up in this list, and if found, the
NL-LIST is used to determine where newlines are inserted.  If the
language element for this brace is not found in this list, the default
behavior is to insert a newline both before and after the brace.")

(defvar cc-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (LANGSYM . (NL-LIST)).

LANGSYSM can be any of: member-init-intro, inher-intro, case-label,
label, and access-label (as defined by the `cc-offset-alist' variable).

NL-LIST can contain any combination of the symbols `before' or
`after'. It also be nil.  When a colon is inserted, the language
element that it defines is looked up in this list, and if found, the
NL-LIST is used to determine where newlines are inserted.  If the
language element for the colon is not found in this list, the default
behavior is to not insert any newlines.")

(defvar cc-auto-hungry-initial-state 'none
  "*Initial state of auto/hungry features when buffer is first visited.
Valid symbol values are:

 none         -- no auto-newline or hungry-delete-key feature
 auto-only    -- auto-newline, but not hungry-delete-key
 hungry-only  -- hungry-delete-key, but not auto-hungry
 auto-hungry  -- both auto-newline and hungry-delete-key enabled

Nil is synonymous for `none' and t is synonymous for `auto-hungry'.")

(defvar cc-untame-characters '(?\')
  "*Utilize a backslashing workaround of an Emacs18 syntax deficiency.
If non-nil, this variable should contain a list of characters which
will be prepended by a backslash in comment regions.  By default, the
list contains only the most troublesome character, the single quote.
To be completely safe, set this variable to:

    '(?\( ?\) ?\' ?\{ ?\} ?\[ ?\])

This variable has no effect under Emacs 19. For details on why this is
necessary in GNU Emacs 18, please refer to the cc-mode texinfo manual.")

(defvar cc-default-macroize-column 78
  "*Column to insert backslashes when macroizing a region.")
(defvar cc-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")
(defvar cc-delete-function 'backward-delete-char-untabify
  "*Function called by `cc-electric-delete' when deleting a single char.")
(defvar cc-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is `alignleft'.")
(defvar cc-backscan-limit 2000
  "*Character limit for looking back while skipping syntactic whitespace.
This variable has no effect under Emacs 19.  For details on why this
is necessary under GNU Emacs 18, please refer to the texinfo manual.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst cc-emacs-features
  (let ((mse-spec 'no-dual-comments)
	(scanner 'v18))
    ;; vanilla GNU18/Epoch 4 uses default values
    (if (= 8 (length (parse-partial-sexp (point) (point))))
	;; we know we're using v19 style dual-comment specifications.
	;; All Lemacsen use 8-bit modify-syntax-entry flags, as do all
	;; patched FSF19, GNU18, Epoch4's.  Only vanilla FSF19 uses
	;; 1-bit flag.  Lets be as smart as we can about figuring this
	;; out.
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
	    (error "cc-mode no longer supports pre 19.8 Lemacsen. Upgrade!")
	    )))
    ;; now cobble up the necessary list
    (list mse-spec scanner))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out on the net, each with different
features supporting those needed by cc-mode.  Here's the current
known list, along with the values for this variable:

 Vanilla GNU 18/Epoch 4:   (no-dual-comments v18)
 GNU 18/Epoch 4 (patch2):  (8-bit v19)
 Lemacs 19.8 and beyond:   (8-bit v19)
 FSFmacs 19:               (1-bit v19)

Note that older, pre-19.8 Lemacsen, and version 1 patches for
GNU18/Epoch4 are no longer supported.  If cc-mode generates an error
when loaded, you should upgrade your Emacs.")

(defvar cc-c++-mode-abbrev-table nil
  "Abbrev table in use in cc-mode C++ buffers.")
(define-abbrev-table 'cc-c++-mode-abbrev-table ())

(defvar cc-c-mode-abbrev-table nil
  "Abbrev table in use in cc-mode C buffers.")
(define-abbrev-table 'cc-c-mode-abbrev-table ())

(defvar cc-mode-map ()
  "Keymap used in cc-mode buffers.")
(if cc-mode-map
    ()
  (setq cc-mode-map (make-sparse-keymap))
  (define-key cc-mode-map "{"         'cc-electric-brace)
  (define-key cc-mode-map "}"         'cc-electric-brace)
  (define-key cc-mode-map ";"         'cc-electric-semi&comma)
  (define-key cc-mode-map ","         'cc-electric-semi&comma)
  (define-key cc-mode-map "#"         'cc-electric-pound)
  (define-key cc-mode-map "\e\C-h"    'cc-mark-function)
  (define-key cc-mode-map "\e\C-q"    'cc-indent-exp)
  (define-key cc-mode-map "\t"        'cc-indent-command)
  (define-key cc-mode-map "\C-c\C-\\" 'cc-macroize-region)
  (define-key cc-mode-map "\C-c\C-c"  'cc-comment-region)
  (define-key cc-mode-map "\C-c\C-u"  'cc-uncomment-region)
  (define-key cc-mode-map "\C-c\C-x"  'cc-match-paren)
  (define-key cc-mode-map "\e\C-x"    'cc-indent-defun)
  (define-key cc-mode-map "/"         'cc-electric-slash)
  (define-key cc-mode-map "*"         'cc-electric-star)
  (define-key cc-mode-map ":"         'cc-electric-colon)
  (define-key cc-mode-map "\C-c\C-;"  'cc-scope-operator)
  (define-key cc-mode-map "\C-c\C-s"  'cc-show-semantic-information)
  (define-key cc-mode-map "\C-c\C-o"  'cc-change-semantic-symbol-offset)
  (define-key cc-mode-map "\177"      'cc-electric-delete)
  (define-key cc-mode-map "\C-c\C-t"  'cc-toggle-auto-hungry-state)
  (define-key cc-mode-map "\C-c\C-h"  'cc-toggle-hungry-state)
  (define-key cc-mode-map "\C-c\C-a"  'cc-toggle-auto-state)
  (if (memq 'v18 cc-emacs-features)
      (progn
	(define-key cc-mode-map "\C-c'"     'cc-tame-comments)
	(define-key cc-mode-map "'"         'cc-tame-insert)
	(define-key cc-mode-map "["         'cc-tame-insert)
	(define-key cc-mode-map "]"         'cc-tame-insert)
	(define-key cc-mode-map "("         'cc-tame-insert)
	(define-key cc-mode-map ")"         'cc-tame-insert)))
  (define-key cc-mode-map "\C-c\C-b"  'cc-submit-bug-report)
  (define-key cc-mode-map "\C-c\C-v"  'cc-version)
  )

(defvar cc-c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if cc-c++-mode-syntax-table
    ()
  (setq cc-c++-mode-syntax-table (make-syntax-table))
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?\\ "\\"    cc-c++-mode-syntax-table)
  (modify-syntax-entry ?+  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?-  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?=  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?%  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?<  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?>  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?&  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?|  "."     cc-c++-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    cc-c++-mode-syntax-table)
  ;; comment syntax
  (cond
   ((memq '8-bit cc-emacs-features)
    ;; Lucid emacs has the best implementation
    (modify-syntax-entry ?/  ". 1456" cc-c++-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   cc-c++-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    cc-c++-mode-syntax-table))
   ((memq '1-bit cc-emacs-features)
    ;; FSF19 does things differently, but we can work with it
    (modify-syntax-entry ?/  ". 124" cc-c++-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23b" cc-c++-mode-syntax-table)
    (modify-syntax-entry ?\n ">"     cc-c++-mode-syntax-table))
   (t
    ;; Vanilla GNU18 doesn't support mult-style comments.  We'll do
    ;; the best we can, but some strange behavior may be encountered.
    ;; PATCH or UPGRADE!
    (modify-syntax-entry ?/  ". 124" cc-c++-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"  cc-c++-mode-syntax-table)
    (modify-syntax-entry ?\n ">"     cc-c++-mode-syntax-table))
   ))

(defvar cc-c-mode-syntax-table nil
  "Syntax table used in c++-c-mode buffers.")
(if cc-c-mode-syntax-table
    ()
  (setq cc-c-mode-syntax-table (make-syntax-table))
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?\\ "\\"    cc-c-mode-syntax-table)
  (modify-syntax-entry ?+  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?-  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?=  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?%  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?<  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?>  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?&  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?|  "."     cc-c-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    cc-c-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14"  cc-c-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  cc-c-mode-syntax-table)
  )

(defvar cc-hungry-delete-key nil
  "Internal state of hungry delete key feature.")
(defvar cc-auto-newline nil
  "Internal state of auto newline feature.")

(make-variable-buffer-local 'cc-auto-newline)
(make-variable-buffer-local 'cc-hungry-delete-key)


;; constant regular expressions for looking at various constructs
(defconst cc-symbol-key "\\(\\w\\|_\\)+"
  "Regexp describing a C/C++ symbol.
We cannot use just `w' syntax class since `_' cannot be in word class.
Putting underscore in word class breaks forward word movement behavior
that users are familiar with.")
(defconst cc-class-key
  (concat
   "\\(\\(extern\\|typedef\\)\\s +\\)?"
   "\\(template\\s *<[^>]*>\\s *\\)?"
   "\\<\\(class\\|struct\\|union\\)\\>")
  "Regexp describing a class declaration, including templates.")
(defconst cc-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  cc-class-key
	  "[ \t]+"
	  cc-symbol-key
	  "\\([ \t]*:[ \t]*\\)?\\s *[^;]")
  "Regexp describing a class inheritance declaration.")
(defconst cc-baseclass-key
  (concat
   ":?[ \t]*\\(virtual[ \t]+\\)?"
   "\\(\\(public\\|private\\|protected\\)[ \t]+\\)"
   cc-symbol-key)
  "Regexp describing base classes in a derived class definition.")
(defconst cc-case-statement-key
  (concat "\\(case[ \t]+\\(\\w\\|[_']\\)+[ \t]*\\)"
	  "\\|\\(default[ \t]*\\):")
  "Regexp describing a switch's case or default label")
(defconst cc-access-key "\\<\\(public\\|protected\\|private\\)\\>:"
  "Regexp describing access specification keywords.")


;; main entry points for the modes
(defun cc-c++-mode ()
  "Major mode for editing C++ code.  $Revision: 3.69 $
To submit a problem report, enter `\\[cc-submit-bug-report]' from a
cc-c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducable test case and send the message.

Note that the details of configuring cc-c++-mode have been moved to
the accompanying texinfo manual.

The hook variable `cc-c++-mode-hook' is run with no args, if that
value is non-nil.  Also the common hook cc-mode-hook is run both by
this defun, and `cc-c-mode'.

Key bindings:
\\{cc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cc-c++-mode-syntax-table)
  (setq major-mode 'cc-c++-mode
	mode-name "C++"
	local-abbrev-table cc-c++-mode-abbrev-table)
  (cc-common-init)
  (setq comment-start "// "
	comment-end "")
  (run-hooks 'cc-c++-mode-hook)
  (cc-set-auto-hungry-state
   (memq cc-auto-hungry-initial-state '(auto-only   auto-hungry t))
   (memq cc-auto-hungry-initial-state '(hungry-only auto-hungry t))))

(defun cc-c-mode ()
  "Major mode for editing K&R and ANSI C code.  $Revision: 3.69 $
To submit a problem report, enter `\\[cc-submit-bug-report]' from a
cc-c-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducable test case and send the message.

Note that the details of configuring cc-c-mode have been moved to
the accompanying texinfo manual.

The hook variable `cc-c-mode-hook' is run with no args, if that
value is non-nil.  Also the common hook cc-mode-hook is run both by
this defun, and `cc-c++-mode'.

Key bindings:
\\{cc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cc-c-mode-syntax-table)
  (setq major-mode 'cc-c-mode
	mode-name "C"
	local-abbrev-table cc-c-mode-abbrev-table)
  (cc-common-init)
  (setq comment-start "/* "
	comment-end   " */")
  (run-hooks 'cc-c-mode-hook)
  (cc-set-auto-hungry-state
   (memq cc-auto-hungry-initial-state '(auto-only   auto-hungry t))
   (memq cc-auto-hungry-initial-state '(hungry-only auto-hungry t))))

(defun cc-common-init ()
  ;; Common initializations for cc-c++-mode and cc-c-mode.
  (use-local-map cc-mode-map)
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
  ;; don't worry about byte-compiler complaints on the following
  (make-local-variable
   (if (boundp 'comment-indent-function)
       'comment-indent-function
     'comment-indent-hook))
  ;; now set their values
  (setq paragraph-start (concat "^$\\|" page-delimiter)
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments (not (memq 'v18 cc-emacs-features))
	indent-line-function 'cc-indent-via-language-element
	indent-region-function 'cc-indent-region
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *")
  ;; don't worry about byte-compiler complaints on the following
  (if (boundp 'comment-indent-function)
      (setq comment-indent-function 'cc-comment-indent)
    (setq comment-indent-hook 'cc-comment-indent))
  ;; hack auto-hungry designators into mode-line-format, but do it
  ;; only once
  (and (listp mode-line-format)
       (memq major-mode '(cc-c++-mode cc-c-mode))
       (not (get 'mode-line-format 'cc-hacked-mode-line))
       (let ((name (memq 'mode-name mode-line-format))
	     (hack '((cc-hungry-delete-key
		      (cc-auto-newline "/ah" "/h")
		      (cc-auto-newline "/a")))))
	 (setcdr name (append hack (cdr name)))
	 (put 'mode-line-format 'cc-hacked-mode-line t)
	 ))
  (run-hooks 'cc-mode-hook))


;; indentation functions to hook into Emacs generic variables
(defun cc-comment-indent ()
  ;; Used by `indent-for-comment' to decide how much to indent a
  ;; comment in C++ code based on its context
  (if (looking-at "^\\(/\\*\\|//\\)")
      0					; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max
       ;; leave at least one space on non-empty lines.
       (if (zerop (current-column))
	   0
	 (1+ (current-column)))
       ;; use comment-column if previous line is comment only line
       ;; indented to the left of comment-column
       (save-excursion
	 (beginning-of-line)
	 (if (not (bobp))
	     (forward-line -1))
	 (skip-chars-forward " \t")
	 (if (looking-at "/\\*\\|//")
	     (if (< (current-column) comment-column)
		 comment-column
	       (current-column))
	   0))
       (let ((cur-pt (point)))
	 (beginning-of-line 0)
	 ;; If previous line had a comment, use it's indent
	 (if (re-search-forward comment-start-skip cur-pt t)
	     (progn
	       (goto-char (match-beginning 0))
	       (current-column))
	   comment-column))))))		; otherwise indent at comment column.


;; auto-newline/hungry delete key
(defmacro cc-keep-region-active ()
  ;; macro to keep region active in Emacs 19
  (cond
   ((boundp 'zmacs-region-stays)
    (` (if (interactive-p) (setq zmacs-region-stays t))))
   ((boundp 'deactivate-mark)
    (` (if (interactive-p) (setq deactivate-mark (not mark-active)))))
   ))

(defun cc-set-auto-hungry-state (auto-p hungry-p)
  ;; Set auto/hungry to state indicated by AUTO-P and HUNGRY-P, and
  ;; update the mode line accordingly
  (setq cc-auto-newline auto-p
	cc-hungry-delete-key hungry-p)
  ;; hack to get mode line updated. Emacs19 should use
  ;; force-mode-line-update, but that isn't portable to Emacs18
  ;; and this at least works for both
  (set-buffer-modified-p (buffer-modified-p)))

(defun cc-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero."
  (interactive "P")
  (cc-set-auto-hungry-state
   ;; calculate the auto-newline state
   (if (or (not arg)
	   (zerop (setq arg (prefix-numeric-value arg))))
       (not cc-auto-newline)
     (> arg 0))
   cc-hungry-delete-key)
  (cc-keep-region-active))

(defun cc-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero."
  (interactive "P")
  (cc-set-auto-hungry-state
   cc-auto-newline
   ;; calculate hungry delete state
   (if (or (not arg)
	   (zerop (setq arg (prefix-numeric-value arg))))
       (not cc-hungry-delete-key)
     (> arg 0)))
  (cc-keep-region-active))

(defun cc-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional argument has the following meanings when supplied:
  \\[universal-argument]
        resets features to cc-auto-hungry-initial-state.
  negative number
        turn off both auto-newline and hungry-delete-key features.
  positive number
        turn on both auto-newline and hungry-delete-key features.
  zero
        toggle both features."
  (interactive "P")
  (let ((numarg (prefix-numeric-value arg)))
    (cc-set-auto-hungry-state
     ;; calculate auto newline state
     (if (or (not arg)
	     (zerop numarg))
	 (not cc-auto-newline)
       (if (consp arg)
	   (memq cc-auto-hungry-initial-state '(auto-only auto-hungry t))
	 (> arg 0)))
     ;; calculate hungry delete state
     (if (or (not arg)
	     (zerop numarg))
	 (not cc-hungry-delete-key)
       (if (consp arg)
	   (memq cc-auto-hungry-initial-state '(hungry-only auto-hungry t))
	 (> arg 0)))
     ))
  (cc-keep-region-active))


;; macro definitions
(defmacro cc-point (position)
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
  (` (let ((here (point)))
       (,@ (let ((position (eval position)))
	     (cond
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
	      )))
       (prog1
	   (point)
	 (goto-char here))
       )))

(defmacro cc-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  (` (and cc-auto-newline
	  (not (cc-in-literal))
	  (not (newline)))))


;; COMMANDS
(defun cc-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `cc-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `cc-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`cc-delete-function' is called."
  (interactive "P")
  (if (or (not cc-hungry-delete-key)
	  arg
	  (cc-in-literal))
      (funcall cc-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall cc-delete-function 1)
	))))

(defun cc-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`cc-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "P")
  (if (or (cc-in-literal)
	  arg
	  (not (memq 'alignleft cc-electric-pound-behavior)))
      ;; do nothing special
      (self-insert-command arg)
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point))))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (goto-char (- (point-max) pos))
      )))

(defmacro cc-insert-and-tame (arg)
  ;; insert last-command-char in the buffer and possibly tame it
  (` (progn
       (and (memq 'v18 cc-emacs-features)
	  (memq literal '(c c++))
	  (memq last-command-char cc-untame-characters)
	  (insert "\\"))
       (self-insert-command (prefix-numeric-value arg))
       )))

(defun cc-electric-brace (arg)
  "Electric brace insertion.
Inserts a brace and possibly some newlines based on the value of
`cc-hanging-braces-alist'.  It may also correct the line's
indentation. If a numeric ARG is supplied, or if point is inside a
literal, nothing special happens."
  (interactive "P")
  (let* ((bod (cc-point 'bod))
	 (literal (cc-in-literal bod))
	 ;; we want to inhibit blinking the paren since this will be
	 ;; most disruptive. we'll blink it ourselves later on
	 (old-blink-paren-function blink-paren-function)
	 (blink-paren-function nil)
	 semantics newlines)
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$"))
	    (not cc-auto-newline))
	(cc-insert-and-tame arg)
      (setq semantics (progn
			(newline)
			(self-insert-command (prefix-numeric-value arg))
			(cc-guess-basic-semantics bod))
	    newlines (or (assq (car (or (assq 'defun-open semantics)
					(assq 'class-open semantics)
					(assq 'inline-open semantics)
					(assq 'block-open semantics)))
			       cc-hanging-braces-alist)
			 (if (= last-command-char ?{)
			     '(ignore before after)
			   '(ignore after))))
      ;; does a newline go before the open brace?
      (if (memq 'before newlines)
	  ;; we leave the newline we've put in there before,
	  ;; but we need to re-indent the line above
	  (let ((pos (- (point-max) (point))))
	    (forward-line -1)
	    (cc-indent-via-language-element bod)
	    (goto-char (- (point-max) pos)))
	;; must remove the newline we just stuck in
	(delete-region (- (point) 2) (1- (point)))
	;; since we're hanging the brace, we need to recalculate
	;; semantics
	(setq semantics (cc-guess-basic-semantics bod)))
      ;; now adjust the line's indentation
      (cc-indent-via-language-element bod semantics)
      ;; Do all appropriate clean ups
      (let ((here (point))
	    (pos (- (point-max) (point)))
	    mbeg mend)
	;; clean up empty defun braces
	(if (and (memq 'empty-defun-braces cc-cleanup-list)
		 (= last-command-char ?\})
		 (or (assq 'defun-close semantics)
		     (assq 'class-close semantics)
		     (assq 'inline-close semantics))
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?\{))
		 ;; make sure matching open brace isn't in a comment
		 (not (cc-in-literal)))
	    (delete-region (point) (1- here)))
	;; clean up brace-else-brace
	(if (and (memq 'brace-else-brace cc-cleanup-list)
		 (= last-command-char ?\{)
		 (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		 (progn
		   (setq mbeg (match-beginning 0)
			 mend (match-end 0))
		   (= mend here))
		 (not (cc-in-literal)))
	    (delete-region mbeg mend))
	(goto-char (- (point-max) pos))
	)
      ;; does a newline go after the brace?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (cc-indent-via-language-element)))
      ;; blink the paren
      (and (= last-command-char ?\})
	   old-blink-paren-function
	   (save-excursion
	     (cc-backward-syntactic-ws bod)
	     (funcall old-blink-paren-function)))
      )))
      
(defun cc-electric-slash (arg)
  "Insert slash, possibly indenting line as a comment.
If slash is second of a double-slash comment introducing construct,
and we are on a comment-only-line, indent line as comment.  If numeric
ARG is supplied, indentation is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (= (preceding-char) ?/)
		      (= last-command-char ?/)
		      (not (cc-in-literal)))))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(cc-indent-via-language-element))))

(defun cc-electric-star (arg)
  "Insert a start, possibly indenting line as a C block comment.
If numeric ARG is supplied, indentation is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (or (and (memq (cc-in-literal) '(c))
			       (save-excursion
				 (skip-chars-backward "* \t")
				 (bolp)))
			  (= (preceding-char) ?/)))))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(cc-indent-via-language-element))))

(defun cc-electric-semi&comma (arg)
  "Insert a comma or semicolon, possibly re-indenting line.
If numeric ARG is supplied, indentation is inhibited."
  (interactive "P")
  (let* ((bod (cc-point 'bod))
	 (literal (cc-in-literal bod))
	 (here (point)))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$"))
	    (not cc-auto-newline))
	(cc-insert-and-tame arg)
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      (let ((pos (- (point-max) (point))))
	;; possibly do some cleanups
	(if (and (or (and
		      (= last-command-char ?,)
		      (memq 'list-close-comma cc-cleanup-list))
		     (and
		      (= last-command-char ?\;)
		      (memq 'defun-close-semi cc-cleanup-list)))
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?}))
		 ;; make sure matching open brace isn't in a comment
		 (not (cc-in-literal)))
	    (delete-region (point) here))
	(goto-char (- (point-max) pos)))
      ;; re-indent line
      (cc-indent-via-language-element bod)
      ;; newline only after semicolon, but only if that semicolon is
      ;; not inside a parenthesis list (e.g. a for loop statement)
      (and (= last-command-char ?\;)
	   (condition-case nil
	       (save-excursion
		 (up-list -1)
		 (/= (following-char) ?\())
	     (error t))
	   (progn (newline) t)
	   (cc-indent-via-language-element bod))
      )))

(defun cc-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive)
  (insert "::"))

(defun cc-electric-colon (arg)
  "Insert a colon, possible reindenting a line.
Will also cleanup double colon scope operators."
  (interactive "P")
  (let* ((bod (cc-point 'bod))
	 (literal (cc-in-literal bod))
	 semantics newlines)
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$"))
	    (not cc-auto-newline))
	(cc-insert-and-tame arg)
      ;; lets do some special stuff with the colon character
      (setq semantics (progn
			(self-insert-command (prefix-numeric-value arg))
			(cc-guess-basic-semantics bod))
      ;; some language elements can only be determined by checking the
      ;; following line.  Lets first look for ones that can be found
      ;; when looking on the line with the colon
	    newlines (or
		      (let ((langelem (or (assq 'case-label semantics)
					  (assq 'label semantics)
					  (assq 'access-label semantics))))
			(and langelem
			     (assq (car langelem) cc-hanging-colons-alist)))
		      (prog2
			(insert "\n")
			(let* ((semantics (cc-guess-basic-semantics bod))
			       (langelem
				(or (assq 'member-init-intro semantics)
				    (assq 'inher-intro semantics))))
			  (and langelem
			       (assq (car langelem) cc-hanging-colons-alist)))
			(delete-char -1))
		      ))
      ;; indent the current line
      (cc-indent-via-language-element bod semantics)
      ;; does a newline go before the colon?
      (if (memq 'before newlines)
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (cc-indent-via-language-element bod)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (cc-indent-via-language-element bod)))
      ;; we may have to clean up double colons
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and (memq 'scope-operator cc-cleanup-list)
		 (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?:))
		 (not (cc-in-literal))
		 (not (= (char-after (- (point) 2)) ?:)))
	    (delete-region (point) (1- here)))
	(goto-char (- (point-max) pos)))
      )))

(defun cc-change-semantic-symbol-offset (symbol offset)
  (interactive "SSemantic symbol: \nnOffset: ")
  (setcdr (or (assq symbol cc-offsets-alist)
	      (error "%s is not a valid semantic symbol." symbol))
	  offset)
  (cc-keep-region-active))


;; Workarounds for GNU Emacs 18 scanning deficiencies
(defun cc-tame-insert (arg)
  "Safely inserts certain troublesome characters in comment regions.
This function is only necessary in GNU Emacs 18.  For details, refer
to the accompanying texinfo manual.

See also the variable `cc-untame-characters'."
  (interactive "P")
  (let ((literal (cc-in-literal)))
    (cc-insert-and-tame arg)))

(defun cc-tame-comments ()
  "Backslashifies all untamed in comment regions found in the buffer.
This function is only necessary in GNU Emacs 18. For details, refer to
the accompanying texinfo manual.

See also the variable `cc-untame-characters'."
  (interactive)
  ;; make the list into a valid charset, escaping where necessary
  (let ((charset (concat "^" (mapconcat
			      (function
			       (lambda (char)
				 (if (memq char '(?\\ ?^ ?-))
				     (concat "\\" (char-to-string char))
				   (char-to-string char))))
			      cc-untame-characters ""))))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
	(skip-chars-forward charset)
	(if (and (not (zerop (following-char)))
		 (memq (cc-in-literal) '(c c++))
		 (/= (preceding-char) ?\\ ))
	    (insert-char  ?\\ 1))
	(if (not (eobp))
	    (forward-char 1)))))
  (cc-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun cc-indent-command (&optional whole-exp)
  "Indent current line as C++ code, or in some cases insert a tab character.

If `cc-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.  If other than nil
or t, then tab is inserted only within literals (comments and strings)
and inside preprocessor directives, but line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (let ((bod (cc-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (cc-indent-via-language-element bod))
	      beg end)
	  (save-excursion
	    (if (eq cc-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end shift-amt "#")))
      ;; No arg supplied, use cc-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not cc-tab-always-indent)
	(if (or (< (point) (cc-point 'boi))
		(= (cc-point 'boi) (cc-point 'eol)))
	    (cc-indent-via-language-element bod)
	  (insert-tab)))
       ;; CASE 2: just indent the line
       ((eq cc-tab-always-indent t)
	(cc-indent-via-language-element bod))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (cc-in-literal bod)
	    (insert-tab))
	(cc-indent-via-language-element bod)
	))))
  (cc-keep-region-active))

(defun cc-indent-exp (&optional shutup-p)
  "Indent each line in block following pont.
Optional SHUTUP-P if non-nil, inhibits message printing."
  (interactive "P")
  (let ((start (point))
	(end (progn
	       (forward-sexp 1)
	       (point-marker)))
	(bod (cc-point 'bod))
	;; keep quiet for speed
	(cc-echo-semantic-information-p nil))
    (or shutup-p
	(message "indenting expression... (this may take a while)"))
    (goto-char start)
    (while (< (point) end)
      (cc-indent-via-language-element bod)
      (forward-line 1))
    (or shutup-p
	(message "indenting expression... done."))
    (goto-char start)
    (set-marker end nil))
  (cc-keep-region-active))

(defun cc-indent-defun ()
  "Indents the current function def, struct or class declaration."
  (interactive)
  (let ((here (point-marker))
	(cc-echo-semantic-information-p nil))
    (beginning-of-defun)
    (cc-indent-exp)
    (goto-char here)
    (set-marker here nil))
  (cc-keep-region-active))

(defun cc-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (message "indenting region... (this may take a while)")
  (save-excursion
    (goto-char start)
    (let ((endmark (copy-marker end))
	  (cc-tab-always-indent t)
	  (cc-echo-semantic-information-p nil)
	  (lim (cc-point 'bod)))
      (while (< (point) endmark)
	;; Indent one line as with TAB.
	(let ((shift-amt (cc-indent-via-language-element lim))
	      nextline sexpend sexpstart)
	  ;; Find beginning of following line.
	  (setq nextline (cc-point 'bonl))
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
	    (cc-forward-syntactic-ws))
	  ;; If that sexp ends within the region,
	  ;; indent it all at once, fast.
	  (if (and sexpend
		   (> sexpend nextline)
		   (<= sexpend endmark))
	      (progn
		(goto-char sexpstart)
		(cc-indent-exp 'shutup)
		(goto-char sexpend)))
	  ;; Move to following line and try again.
	  (and sexpend
	       (set-marker sexpend nil))
	  (forward-line 1)))
      (set-marker endmark nil)))
  (message "indenting region... done.")
  (cc-keep-region-active))

(defun cc-mark-function ()
  "Put mark at end of a C/C++ defun, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point) nil t)
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
(defun cc-emacs18-fsws (&optional lim)
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
       ((and (= (cc-point 'boi) (point))
	     (= (following-char) ?#))
	(end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(defun cc-emacs18-bsws (&optional lim)
  ;; Backward skip syntactic whitespace for Emacs 18."
  (let ((lim (or lim (cc-point 'bod)))
	literal stop)
    (if (and cc-backscan-limit
	     (> (- (point) lim) cc-backscan-limit))
	(setq lim (- (point) cc-backscan-limit)))
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      ;; c++ comment
      (if (eq (setq literal (cc-in-literal lim)) 'c++)
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


(defun cc-emacs19-accurate-fsws (&optional lim)
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
		 (= (cc-point 'boi) (point)))
	    (end-of-line)
	  )))))

(defun cc-emacs19-accurate-bsws (&optional lim)
  ;; Backward skip over syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (cc-point 'bod)))
	   (here lim))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment -1)
	      (if (eq (cc-in-literal lim) 'pound)
		  (beginning-of-line))
	      )))
      )))


;; Return `c' if in a C-style comment, `c++' if in a C++ style
;; comment, `string' if in a string literal, `pound' if on a
;; preprocessor line, or nil if not in a comment at all.  Optional LIM
;; is used as the backward limit of the search.  If omitted, or nil,
;; `beginning-of-defun' is used."
(defun cc-emacs18-il (&optional lim)
  ;; Determine if point is in a C/C++ literal
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (cc-point 'bod))))
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
(defun cc-8bit-il (&optional lim)
  ;; Determine if point is in a C++ literal
  (save-excursion
    (let* ((lim (or lim (cc-point 'bod)))
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

;; This is for all 1-bit emacsen (FSFmacs 19)
(defun cc-1bit-il (&optional lim)
  ;; Determine if point is in a C++ literal
  (save-excursion
    (let* ((lim  (or lim (cc-point 'bod)))
	   (here (point))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) (if (nth 7 state) 'c 'c++))
       ((progn
	  (goto-char here)
	  (beginning-of-line)
	  (looking-at "[ \t]*#"))
	'pound)
       (t nil)))))

;; set the compatibility for all the different Emacsen. wish we didn't
;; have to do this!  Note that pre-19.8 lemacsen are no longer
;; supported.
(fset 'cc-forward-syntactic-ws
      (cond
       ((memq 'v18 cc-emacs-features)     'cc-emacs18-fsws)
       ((memq 'old-v19 cc-emacs-features)
	(error "Old Lemacsen are no longer supported. Upgrade!"))
       ((memq 'v19 cc-emacs-features)     'cc-emacs19-accurate-fsws)
       (t (error "Bad cc-emacs-features: %s" cc-emacs-features))
       ))
(fset 'cc-backward-syntactic-ws
      (cond
       ((memq 'v18 cc-emacs-features)     'cc-emacs18-bsws)
       ((memq 'old-v19 cc-emacs-features)
	(error "Old Lemacsen are no longer supported. Upgrade!"))
       ((memq 'v19 cc-emacs-features)     'cc-emacs19-accurate-bsws)
       (t (error "Bad cc-emacs-features: %s" cc-emacs-features))
       ))
(fset 'cc-in-literal
      (cond
       ((memq 'no-dual-comments cc-emacs-features) 'cc-emacs18-il)
       ((memq '8-bit cc-emacs-features)            'cc-8bit-il)
       ((memq '1-bit cc-emacs-features)            'cc-1bit-il)
       (t (error "Bad cc-emacs-features: %s" cc-emacs-features))
       ))


;; utilities for moving and querying around semantic elements
(defun cc-parse-state (&optional lim)
  ;; Determinate the syntactic state of the code at point.
  ;; Iteratively uses `parse-partial-sexp' from point to LIM and
  ;; returns the result of `parse-partial-sexp' at point.  LIM is
  ;; optional and defaults to `point-max'."
  (let ((lim (or lim (point-max)))
	state)
    (while (< (point) lim)
      (setq state (parse-partial-sexp (point) lim 0)))
    state))

(defmacro cc-back-block ()
  ;; move up one block, returning t if successful, otherwise returning
  ;; nil
  (` (or (condition-case nil
	     (progn (up-list -1) t)
	   (error nil))
	 (condition-case nil
	     (progn (down-list -1) t)
	   (error nil))
	 )))

(defun cc-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let ((lim (or lim (cc-point 'bod)))
	(here (point))
	(placeholder (progn
		       (back-to-indentation)
		       (point))))
    (cc-backward-syntactic-ws lim)
    (while (and (> (point) lim)
		(memq (preceding-char) '(?, ?:))
		(progn
		  (beginning-of-line)
		  (setq placeholder (point))
		  (skip-chars-forward " \t")
		  (not (looking-at cc-class-key))
		  ))
      (cc-backward-syntactic-ws lim))
    (goto-char placeholder)
    (skip-chars-forward "^:" (cc-point 'eol))))

(defun cc-beginning-of-statement (&optional lim)
  ;; Go to the beginning of the innermost C/C++ statement.  Optional
  ;; LIM is the farthest back to search; if not provided,
  ;; beginning-of-defun is used.
  (let ((charlist '(nil ?\000 ?\, ?\; ?\} ?\: ?\{))
	(lim (or lim (cc-point 'bod)))
	(here (point))
	stop)
    (beginning-of-line)
    (cc-backward-syntactic-ws lim)
    (while (not stop)
      (if (or (memq (preceding-char) charlist)
	      (<= (point) lim))
	  (setq stop t)
	;; catch multi-line function calls
	(if (= (preceding-char) ?\))
	    (forward-sexp -1))
	;; check for compound statements
	(back-to-indentation)
	(setq here (point))
	(if (looking-at "\\<\\(for\\|if\\|do\\|else\\|while\\)\\>")
	    (setq stop t)
	  (cc-backward-syntactic-ws lim)
	  )))
    (if (< (point) lim)
	(goto-char lim)
      (goto-char here)
      (back-to-indentation))
    ))

(defun cc-beginning-of-macro (&optional lim)
  ;; Go to the beginning of the macro. Right now we don't support
  ;; multi-line macros too well
  (back-to-indentation))

(defun cc-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (cc-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (= (preceding-char) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>"))
	  (cc-backward-syntactic-ws)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(if (and (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (cc-backward-syntactic-ws)
		   (looking-at "\\s *:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (= (preceding-char) ?\))
      )))

(defun cc-search-uplist-for-classkey (&optional search-end)
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
	(let (search-start donep foundp)
	  (and search-end
	       (goto-char search-end))
	  (while (not donep)
	    ;; go backwards to the most enclosing C block
	    (while (not search-end)
	      (if (not (cc-back-block))
		  (setq search-end (goto-char (cc-point 'bod)))
		(if (memq (following-char) '(?} ?{))
		    (setq search-end (point))
		  (forward-char 1)
		  (backward-sexp 1)
		  )))
	    ;; go backwards from here to the next most enclosing block
	    (while (not search-start)
	      (if (not (cc-back-block))
		  (setq search-start (goto-char (cc-point 'bod)))
		(if (memq (following-char) '(?} ?{))
		    (setq search-start (point))
		  (forward-char 1)
		  (backward-sexp 1)
		  )))
	    (cond
	     ;; CASE 1: search-end is a close brace. we cannot find the
	     ;; enclosing brace
	     ((= (char-after search-end) ?})
	      (setq donep t))
	     ;; CASE 2: we have exhausted all our possible searches
	     ((= search-start search-end)
	      (setq donep t))
	     ;; CASE 3: now look for class key, but make sure its not in a
	     ;; literal
	     (t
	      (while (and (re-search-forward cc-class-key search-end t)
			  (cc-in-literal)))
	      (if (and (/= (point) search-end)
		       (/= (point) search-start)
		       (not (cc-in-literal)))
		  (setq donep t
			foundp t)
		;; not found in this region. reset search extent and try
		;; again
		(setq search-end search-start
		      search-start nil)
		(goto-char search-end))
	      )
	     ))
	  ;; we've search as much as we can.  if we've found a classkey,
	  ;; then search-end should be at the class's opening brace
	  (and foundp (cons search-end (cc-point 'boi)))
	  ))
    (error nil)))

;; defuns to look backwards for things
(defun cc-backward-to-start-of-do (&optional lim)
  ;; Move to the start of the last "unbalanced" do expression.
  ;; Optional LIM is the farthest back to search.
  (let ((do-level 1)
	(case-fold-search nil)
	(lim (or lim (cc-point 'bod))))
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case err
	  (progn
	    (backward-sexp 1)
	    (cond
	     ((memq (cc-in-literal lim) '(c c++)))
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

(defun cc-backward-to-start-of-if (&optional lim)
  ;; Move to the start of the last "unbalanced" if and return t.  If
  ;; none is found, and we are looking at an if clause, nil is
  ;; returned.  If none is found and we are looking at an else clause,
  ;; an error is thrown.
  (let ((if-level 1)
	(case-fold-search nil)
	(lim (or lim (cc-point 'bod)))
	(at-if (looking-at "if\\b")))
    (catch 'orphan-if
      (while (and (not (bobp))
		  (not (zerop if-level)))
	(cc-backward-syntactic-ws)
	(condition-case errcond
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


;; defuns for calculating the semantic state and indenting a single
;; line of C/C++ code
(defmacro cc-add-semantics (symbol &optional relpos)
  ;; a simple macro to append the semantics in symbol to the semantics
  ;; list.  try to increase performance by using this macro
  (` (setq semantics (cons (cons (, symbol) (, relpos)) semantics))))

(defun cc-guess-basic-semantics (&optional lim)
  ;; guess the semantic description of the current line of C++ code.
  ;; Optional LIM is the farthest back we should search
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state literal
	  containing-sexp char-before-ip char-after-ip
	  (lim (or lim (cc-point 'bod)))
	  semantics placeholder inclass-p
	  )				;end-let
      ;; narrow out the enclosing class
      (save-restriction
	(if (setq inclass-p (cc-search-uplist-for-classkey))
	    (progn
	      (narrow-to-region
	       (progn
		 (goto-char (1+ (car inclass-p)))
		 (cc-forward-syntactic-ws indent-point)
		 (cc-point 'bol))
	       (progn
		 (goto-char indent-point)
		 (cc-point 'eol)))
	      (setq lim (point-min))))
	;; parse the state of the line
	(goto-char lim)
	(setq state (cc-parse-state indent-point)
	      containing-sexp (nth 1 state)
	      literal (cc-in-literal lim))
	;; cache char before and after indent point, and move point to
	;; the most like position to perform regexp tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (following-char))
	(cc-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; now figure out semantic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((memq literal '(string))
	  (cc-add-semantics 'string (cc-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  (cc-add-semantics literal (cc-point 'bopl)))
	 ;; CASE 3: in a cpp preprocessor
	 ((eq literal 'pound)
	  (cc-beginning-of-macro lim)
	  (cc-add-semantics 'cpp-macro (cc-point 'boi)))
	 ;; CASE 4: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 4A: we are looking at a defun, class, or
	   ;; inline-inclass method opening brace
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 4A.1: we are looking at a class opening brace
	     ((save-excursion
		(let ((decl (cc-search-uplist-for-classkey indent-point)))
		  (and decl
		       (setq placeholder (cdr decl)))
		  ))
	      (cc-add-semantics 'class-open placeholder))
	     ;; CASE 4A.2: inline defun open
	     (inclass-p
	      (cc-add-semantics 'inline-open (cdr inclass-p)))
	     ;; CASE 4A.3: ordinary defun open
	     (t
	      (cc-add-semantics 'defun-open (cc-point 'bol))
	      )))
	   ;; CASE 4B: first K&R arg decl or member init
	   ((cc-just-after-func-arglist-p)
	    (cond
	     ;; CASE 4B.1: a member init
	     ((or (= char-before-ip ?:)
		  (= char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      (if (= char-before-ip ?:)
		  (forward-char -1))
	      (cc-backward-syntactic-ws lim)
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      (cc-add-semantics 'member-init-intro (cc-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 4B.2: nether region after a C++ func decl
	     ((eq major-mode 'cc-c++-mode)
	      (cc-add-semantics 'c++-funcdecl-cont (cc-point 'boi))
	      (and inclass-p (cc-add-semantics 'inclass (cdr inclass-p))))
	     ;; CASE 4B.3: K&R arg decl intro
	     (t
	      (cc-add-semantics 'knr-argdecl-intro (cc-point 'boi))
	      (and inclass-p (cc-add-semantics 'inclass (cdr inclass-p))))
	     ))
	   ;; CASE 4C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((looking-at cc-baseclass-key)
	    (cond
	     ;; CASE 4C.1: non-hanging colon on an inher intro
	     ((= char-after-ip ?:)
	      (cc-backward-syntactic-ws lim)
	      (cc-add-semantics 'inher-intro (cc-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 4C.2: hanging colon on an inher intro
	     ((= char-before-ip ?:)
	      (cc-add-semantics 'inher-intro (cc-point 'boi))
	      (and inclass-p (cc-add-semantics 'inclass (cdr inclass-p))))
	     ;; CASE 4C.3: a continued inheritance line
	     (t
	      (cc-beginning-of-inheritance-list lim)
	      (cc-add-semantics 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 4D: this could be a top-level compound statement or a
	   ;; member init list continuation
	   ((= char-before-ip ?,)
	    (goto-char indent-point)
	    (cc-backward-syntactic-ws lim)
	    (while (and (< lim (point))
			(= (preceding-char) ?,))
	      ;; this will catch member inits with multiple
	      ;; line arglists
	      (forward-char -1)
	      (cc-backward-syntactic-ws (cc-point 'bol))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      ;; now continue checking
	      (beginning-of-line)
	      (cc-backward-syntactic-ws lim))
	    (cond
	     ;; CASE 4D.1: hanging member init colon
	     ((= (preceding-char) ?:)
	      (goto-char indent-point)
	      (cc-backward-syntactic-ws lim)
	      (cc-add-semantics 'member-init-cont (cc-point 'boi))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 4D.2: non-hanging member init colon
	     ((progn
		(cc-forward-syntactic-ws indent-point)
		(= (following-char) ?:))
	      (skip-chars-forward " \t:")
	      (cc-add-semantics 'member-init-cont (point)))
	     ;; CASE 4D.3: perhaps a multiple inheritance line?
	     ((looking-at cc-inher-key)
	      (cc-add-semantics 'inher-cont-1 (cc-point 'boi)))
	     ;; CASE 4D.4: perhaps a template list continuation?
	     ((save-excursion
		(skip-chars-backward "^<" lim)
		(= (preceding-char) ?<))
	      ;; we can probably indent it just like and arglist-cont
	      (cc-add-semantics 'arglist-cont (point)))
	     ;; CASE 4D.5: perhaps a top-level statement-cont
	     (t (cc-beginning-of-statement lim)
		(cc-add-semantics 'statement-cont (cc-point 'boi)))
	     ))
	   ;; CASE 4E: we are looking at a access specifier
	   ((and inclass-p
		 (looking-at cc-access-key))
	    (cc-add-semantics 'access-label (cc-point 'bonl))
	    (cc-add-semantics 'inclass (cdr inclass-p)))
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
	      (goto-char (car inclass-p))
	      (cc-add-semantics 'class-close (cc-point 'boi))))
	   ;; CASE 4G: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (cc-backward-syntactic-ws lim)
	      (while (and inclass-p
			  (= (preceding-char) ?:)
			  (save-excursion
			    (backward-sexp 1)
			    (looking-at cc-access-key)))
		(backward-sexp 1)
		(cc-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (preceding-char) '(?\; ?\}))))
	    (cc-add-semantics 'topmost-intro (cc-point 'bol))
	    (and inclass-p (cc-add-semantics 'inclass (cdr inclass-p))))
	   ;; CASE 4H: we are at a topmost continuation line
	   (t
	    (cc-add-semantics 'topmost-intro-cont (cc-point 'boi))
	    (and inclass-p (cc-add-semantics 'inclass (cdr inclass-p))))
	   ))				; end CASE 4
	 ;; CASE 5: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((/= (char-after containing-sexp) ?{)
	  (cc-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at the first argument in an empty
	   ;; argument list
	   ((= char-before-ip ?\()
	    (goto-char containing-sexp)
	    (cc-add-semantics 'arglist-intro (cc-point 'boi)))
	   ;; CASE 5B: we are looking at the arglist closing paren
	   ((and (/= char-before-ip ?,)
		 (= char-after-ip ?\)))
	    (cc-add-semantics 'arglist-close (cc-point 'boi)))
	   ;; CASE 5C: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.
	   ((= (cc-point 'bol)
	       (save-excursion
		 (goto-char containing-sexp)
		 (cc-point 'bol)))
	    (cc-add-semantics 'arglist-cont-nonempty containing-sexp))
	   ;; CASE 5D: it is possible that the arglist is really a
	   ;; forloop expression, and thus maybe broken across
	   ;; multiple lines
	   ((not (memq char-before-ip '(?\; ?,)))
	    (cc-beginning-of-statement containing-sexp)
	    (cc-add-semantics 'statement-cont (point)))
	   ;; CASE 5E: we are looking at just a normal arglist
	   ;; continuation line
	   (t (cc-add-semantics 'arglist-cont (cc-point 'boi)))
	   ))
	 ;; CASE 6: func-local multi-inheritance line
	 ((save-excursion
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (looking-at cc-baseclass-key))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 6A: non-hanging colon on an inher intro
	   ((= char-after-ip ?:)
	    (cc-backward-syntactic-ws lim)
	    (cc-add-semantics 'inher-intro (cc-point 'boi)))
	   ;; CASE 6B: hanging colon on an inher intro
	   ((= char-before-ip ?:)
	    (cc-add-semantics 'inher-intro (cc-point 'boi)))
	   ;; CASE 6C: a continued inheritance line
	   (t
	    (cc-beginning-of-inheritance-list lim)
	    (cc-add-semantics 'inher-cont (point))
	    )))
	 ;; CASE 7: A continued statement
	 ((> (point)
	     (save-excursion
	       (cc-beginning-of-statement containing-sexp)
	       (setq placeholder (point))))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 7A: a continued statement introducing a new block
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 7A.1: could be a func-local class opening brace
	     ((save-excursion
		(let ((decl (cc-search-uplist-for-classkey indent-point)))
		  (and decl
		       (setq placeholder (cdr decl)))
		  ))
	      (cc-add-semantics 'class-open placeholder))
	     ;; CASE 7A.2: just an ordinary block opening brace
	     (t (cc-add-semantics 'block-open placeholder))
	     ))
	   ;; CASE 7B: iostream insertion or extraction operator
	   ((looking-at "<<\\|>>")
	    (cc-add-semantics 'stream-op placeholder))
	   ;; CASE 7C: hanging continued statement
	   (t (cc-add-semantics 'statement-cont placeholder))
	   ))
	 ;; CASE 8: an else clause?
	 ((looking-at "\\<else\\>")
	  (cc-backward-to-start-of-if containing-sexp)
	  (cc-add-semantics 'else-clause (cc-point 'boi)))
	 ;; CASE 9: Statement. But what kind?  Lets see if its a while
	 ;; closure of a do/while construct
	 ((progn
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (and (looking-at "while\\b")
		 (save-excursion
		   (cc-backward-to-start-of-do containing-sexp)
		   (setq placeholder (point))
		   (looking-at "do\\b"))
		 ))
	  (cc-add-semantics 'do-while-closure placeholder))
	 ;; CASE 10: A case or default label
	 ((looking-at cc-case-statement-key)
	  (goto-char containing-sexp)
	  ;; for a case label, we set relpos the first non-whitespace
	  ;; char on the line containing the switch opening brace. this
	  ;; should handle hanging switch opening braces correctly.
	  (cc-add-semantics 'case-label (cc-point 'boi)))
	 ;; CASE 11: any other label
	 ((looking-at (concat cc-symbol-key ":[^:]"))
	  (goto-char containing-sexp)
	  (cc-add-semantics 'label (cc-point 'boi)))
	 ;; CASE 12: block close brace, possibly closing the defun or
	 ;; the class
	 ((= char-after-ip ?})
	  (if (= containing-sexp lim)
	      (cc-add-semantics 'defun-close containing-sexp)
	    (goto-char containing-sexp)
	    (if inclass-p
		(cc-add-semantics 'inline-close containing-sexp)
	      (cc-add-semantics 'block-close (cc-point 'boi))
	      )))
	 ;; CASE 13: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (cc-forward-syntactic-ws indent-point)
	  ;; we want to ignore labels when skipping forward
	  (let ((ignore-re
		 (concat cc-case-statement-key "\\|" cc-symbol-key ":[^:]"))
		inswitch-p checkpnt)
	    (while (looking-at ignore-re)
	      (if (looking-at cc-case-statement-key)
		  (setq inswitch-p t))
	      (forward-line 1)
	      (cc-forward-syntactic-ws indent-point))
	    (cond
	     ;; CASE 13.A: we saw a case/default statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case or default label
	     ((and inswitch-p
		   (save-excursion
		     (goto-char indent-point)
		     (cc-backward-syntactic-ws containing-sexp)
		     (back-to-indentation)
		     (setq checkpnt (point))
		     (looking-at cc-case-statement-key)))
	      (cc-add-semantics 'statement-case-intro checkpnt))
	     ;; CASE 13.B: an embedded block open
	     ((= char-after-ip ?{)
	      (cc-add-semantics 'block-open (cc-point 'boi)))
	     ;; CASE 13.C: continued statement
	     ((= char-before-ip ?,)
	      (cc-add-semantics 'statement-cont (cc-point 'boi)))
	     ;; CASE 13.D: a question/colon construct?
	     ((or (memq char-before-ip '(?: ??))
		  (memq char-after-ip '(?: ??)))
	      (cc-add-semantics 'statement-cont (cc-point 'boi)))
	     ;; CASE 13.E: any old statement
	     ((< (point) indent-point)
	      (cc-add-semantics 'statement (cc-point 'boi)))
	     ;; CASE 13.F: first statement in a block
	     (t
	      (goto-char containing-sexp)
	      (cc-add-semantics 'statement-block-intro (cc-point 'boi)))
	     )))
	 ))				; end save-restriction
      ;; now we need to look at any special additional indentations
      (goto-char indent-point)
      ;; look for a comment only line
      (if (looking-at "\\s *\\(//\\|/\\*\\)")
	  (cc-add-semantics 'comment-intro))
      ;; return the semantics
      semantics)))


;; indent via semantic language elements
(defun cc-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; cc-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol cc-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if cc-strict-semantics-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+) (setq offset cc-basic-offset))
     ((eq offset '-) (setq offset (- cc-basic-offset)))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (cc-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

(defun cc-indent-via-language-element (&optional lim semantics)
  ;; indent the curent line as C/C++ code. Optional LIM is the
  ;; farthest point back to search. Optional SEMANTICS is the semantic
  ;; information for the current line. Returns the amount of
  ;; indentation change
  (let* ((lim (or lim (cc-point 'bod)))
	 (semantics (or semantics (cc-guess-basic-semantics lim)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'cc-get-offset semantics)))
	 (shift-amt  (- (current-indentation) indent)))
    (and cc-echo-semantic-information-p
	 (message "semantics: %s, indent= %d" semantics indent))
    (if (zerop shift-amt)
	nil
      (delete-region (cc-point 'bol) (cc-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (cc-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'cc-special-indent-hook)
    shift-amt))

(defun cc-show-semantic-information ()
  "Show semantic information for current line."
  (interactive)
  (message "semantics: %s" (cc-guess-basic-semantics))
  (cc-keep-region-active))


;; Standard indentation line-ups
(defun cc-lineup-arglist (langelem)
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
	  (cc-forward-syntactic-ws (cc-point 'eol))
	  ))
    (if (eolp)
	2
      (- (current-column) cs-curcol)
      ))))

(defun cc-lineup-streamop (langelem)
  ;; lineup stream operators
  (save-excursion
    (let ((containing-sexp (cdr langelem))
	  cs-curcol)
      (goto-char containing-sexp)
      (setq cs-curcol (current-column))
      (skip-chars-forward "^><\n")
      (- (current-column) cs-curcol))))

(defun cc-lineup-multi-inher (langelem)
  ;; line up multiple inheritance lines
  (save-excursion
    (let (cs-curcol
	  (eol (cc-point 'eol))
	  (here (point)))
      (goto-char (cdr langelem))
      (setq cs-curcol (current-column))
      (skip-chars-forward "^:" eol)
      (skip-chars-forward " \t:" eol)
      (if (eolp)
	  (cc-forward-syntactic-ws here))
      (- (current-column) cs-curcol)
      )))

(defun cc-lineup-C-comments (langelem)
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
      (if (re-search-forward "/\\*[ \t]*" (1- (cc-point 'eol)) t)
	  (goto-char (+ (match-beginning 0)
			(cond
			 (cc-C-block-comments-indent-p 0)
			 ((= stars 1) 1)
			 ((= stars 2) 0)
			 (t (- (match-end 0) (match-beginning 0)))))))
      (current-column))))

(defun cc-indent-for-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; cc-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    (if (not (bolp))
	(or (car-safe cc-comment-only-line-offset)
	    cc-comment-only-line-offset)
      (or (cdr-safe cc-comment-only-line-offset)
	  (car-safe cc-comment-only-line-offset)
	  -1000				;jam it against the left side
	  ))))


;; commands for "macroizations" -- making C++ parameterized types via
;; macros. Also commands for commentifying regions

(defun cc-backslashify-current-line (doit)
  ;; Backslashifies current line if DOIT is non-nil, otherwise
  ;; unbackslashifies the current line.
  (end-of-line 1)
  (if doit
      ;; Note that "\\\\" is needed to get one backslash.
      (if (not (save-excursion
		 (forward-char -1)
		 (looking-at "\\\\")))
	  (progn
	    (if (>= (current-column) cc-default-macroize-column)
		(insert " \\")
	      (while (<= (current-column) cc-default-macroize-column)
		(insert "\t")
		(end-of-line))
	      (delete-char -1)
	      (while (< (current-column) cc-default-macroize-column)
		(insert " ")
		(end-of-line))
	      (insert "\\"))))
    (forward-char -1)
    (if (looking-at "\\\\")
	(progn (skip-chars-backward " \t")
	       (kill-line)))))

(defun cc-macroize-region (from to arg)
  "Insert backslashes at end of every line in region.
Useful for defining cpp macros.  If called with a prefix argument,
it will remove trailing backslashes."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (beginning-of-line 1)
    (let ((line (count-lines (point-min) (point)))
	  (to-line (save-excursion
		     (goto-char to)
		     (count-lines (point-min) (point)))))
      (while (< line to-line)
	(cc-backslashify-current-line (null arg))
	(forward-line 1)
	(setq line (1+ line)))))
  (cc-keep-region-active))

(defun cc-comment-region (beg end)
  "Comment out all lines in a region between mark and current point by
inserting `comment-start' in front of each line."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (or (bolp) (forward-line 1)) (point)))
      (goto-char (point-min))
      (while (not (eobp))
	(insert comment-start)
	(forward-line 1))
      (if (eq major-mode 'cc-c-mode)
	  (insert comment-end))))
  (cc-keep-region-active))

(defun cc-uncomment-region (beg end)
  "Uncomment all lines in region between mark and current point by deleting
the leading `// ' from each line, if any."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point)))
      (goto-char (point-min))
      (let ((comment-regexp
	     (if (eq major-mode 'cc-c-mode)
		 (concat "\\s *\\(" (regexp-quote comment-start)
			 "\\|"      (regexp-quote comment-end)
			 "\\)")
	       (concat "\\s *" (regexp-quote comment-start)))))
	(while (not (eobp))
	  (if (looking-at comment-regexp)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (forward-line 1)))))
  (cc-keep-region-active))


;; defuns for submitting bug reports

(defconst cc-version "$Revision: 3.69 $"
  "cc-mode version number.")
(defconst cc-mode-help-address "cc-mode-help@anthem.nlm.nih.gov"
  "Address accepting submission of bug reports.")

(defun cc-version ()
  "Echo the current version of cc-mode in the minibuffer."
  (interactive)
  (message "Using cc-mode version %s" cc-version)
  (cc-keep-region-active))

(defun cc-submit-bug-report ()
  "Submit via mail a bug report on cc-mode."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a report on cc-mode? ")
   (reporter-submit-bug-report
    cc-mode-help-address
    (concat "cc-mode version " cc-version " (editing "
	    (if (eq major-mode 'cc-c++-mode) "C++" "C")
	    " code)")
    (list
     ;; report only the vars that affect indentation
     'cc-emacs-features
     'cc-auto-hungry-initial-state
     'cc-backscan-limit
     'cc-basic-offset
     'cc-offsets-alist
     'cc-C-block-comments-indent-p
     'cc-cleanup-list
     'cc-comment-only-line-offset
     'cc-default-macroize-column
     'cc-delete-function
     'cc-electric-pound-behavior
     'cc-hanging-braces-alist
     'cc-hanging-colon-alist
     'cc-tab-always-indent
     'cc-untame-characters
     'tab-width
     )
    (function
     (lambda ()
       (insert
	(if cc-special-indent-hook
	    (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		    "cc-special-indent-hook is set to '"
		    (format "%s" cc-special-indent-hook)
		    ".\nPerhaps this is your problem?\n"
		    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	  "\n")
	)))
    ))
  (cc-keep-region-active))


(provide 'cc-mode)
;;; cc-mode.el ends here
