;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: unix, languages

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

;; Sets up C-mode with support for awk-style #-comments and a lightly
;; hacked syntax table.

;;; Code:

(defvar awk-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\r ">   " st) ; ACM, 2002/3/24
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\# "<   " st)
    ;; / can delimit regexes or be a division operator.  We assume that it is
    ;; more commonly used for regexes and fix the remaining cases with
    ;; `font-lock-syntactic-keywords'.
    ;; NO! Do it the other way round.  ACM 2002/4/27.
                                        ;  (modify-syntax-entry ?/ "\"" st)
    (modify-syntax-entry ?/ "." st)     ; ACM 2002/4/27.  
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "_" st)
;;    (modify-syntax-entry ?\' "\"" st) ; ACM 2002/5/28. ' isn't a string character in awk!
    (modify-syntax-entry ?\' "." st)
    st)
  "Syntax table in use in `awk-mode' buffers.")

;; ACM, 2002/5/29: Start on:

;; c-awk-NL-prop text property.
;;
;; This text property specifies the status of any incomplete or complete
;; statements at the end of awk line.  It is set ONLY on EOLs.  It's purpose
;; is a sort of "cache", to allow cc-engine functions to check for
;; incomplete/complete statements which would be marked with a ; or } in C.
;;
;; It should be thought of as only really valid immediately after a buffer
;; change, not a permanently set property.
;;
;; The valid values for it are nil and the following single characters:
;;
;; nil The property is not currently set for this line.
;; ';' There is (at least part of) a statement on this line, and the last
;;     statement on the line is complete.
;; '#' There is NO statement on this line (at most a comment), and no open
;;     statement from a previous line which could have been completed on this
;;     line.
;; '{' There is an unfinished statement on this (or a previous) line which
;;     doesn't require \s to continue onto another line, e.g. the line ends
;;     with the && operator, or "if (condition)".  Note that even if the
;;     newline is redundantly escaped, it remains a '{' line.
;; '\' There is an escaped newline on this line after a partial statement.
;;     This '\' is essential to the syntax of the program.  (i.e. if it had
;;     been a frivolous \, it would have been ignored and the line been given
;;     one of the other property values.)

(defun c-awk-after-if-do-for-while-condition-p (&optional lim)
  ;; Are we just after the ) in "if/do/for/while (<condition>)"?
;; Note that at the end of the ) in a do .... while (<condition>) doesn't count.
  (and
   (eq (char-before) ?\))
   (save-excursion
     (goto-char (c-safe (scan-lists (point) -1 0))) ; back over "(...)"
     (c-backward-token-1)
     (or (looking-at "\\(if\\>\\|for\\>\\|do\\)\\>\\([^_]\\|$\\)")
         (and (looking-at "while\\>\\([^_]\\|$\\)") ; make sure the while isn't from a do-while.
              (not (eq (c-beginning-of-statement-1 lim)
                       'beginning)))))))

(defun c-awk-after-function-decl-param-list ()
  ;; Are we just after the ) in "function foo (bar)" ?
  (and (eq (char-before) ?\))
       (save-excursion
         (goto-char (c-safe (scan-lists (point) -1 0))) ; back over "(...)"
         (c-backward-token-1)
         (and (looking-at "[_a-zA-Z][_a-zA-Z0-9]*\\>")
              (progn (c-backward-token-1)
                     (looking-at "func\\(tion\\)?\\>")))))) ; Abbreviation for gawk 3.1, ACM 2002/5/29

(defun c-awk-after-continue-token ()
;; Are we just after a token which can be continued onto the next line without
  ;; a backslash?
  (save-excursion
    (c-backward-token-1)
    (if (looking-at "[&|]") (backward-char)) ; c-backward-token-1 doesn't do this :-(
    (looking-at "[,{?:]\\|&&\\|||\\|do\\>\\|else\\>")))

(defun c-awk-after-rbrace-or-statement-semicolon ()
  ;; Are we just after a } or a ; which closes a statement?
  ;; Be careful about ;s in for loop control bits.  They don't count!
  (or (eq (char-before) ?\})
      (and
       (eq (char-before) ?\;)
       (not (save-excursion
              (goto-char (c-safe (scan-lists (point) -1 1))) ; go back to containing (
              (and (looking-at "(")
                   (c-backward-token-1)
                   (looking-at "for\\>")))))))

(defun c-awk-back-to-contentful-text-or-NL-prop ()
  ;;  awk-mode: move point back to first found of either (i) the BOL after the
  ;;  first found EOL which has the c-awk-NL-prop text-property set.  In this
  ;;  case, return the value of that property; or (ii) just after non-ws text,
  ;;  in which case we return nil.  If we hit BOB, we also return nil.
  ;;  The calling function can best distinguish the cases with (bolp).
  ;;
  ;;  Note that an escaped eol counts as whitespace here.
  ;;
  ;;  Kludge: If c-backward-syntactic-ws gets stuck at a BOL, it is likely
  ;;  that the previous line contains an unterminated string (without \).  In
  ;;  this case, assume that the previous line's c-awk-NL-prop is a ;.
  ;; 
  ;;  Point must be at the start of a line when calling this function.  This
  ;;  is to ensure that the various backward-comment functions will work
  ;;  properly.
  (let ((nl-prop nil)
        bol-pos bsws-pos) ; "backward-syntactic-ws position"
    (while ;; We are at a BOL here.  Go back one line each iteration.
        (and
         (not (bobp))
         (not (setq nl-prop (get-text-property (1- (point)) 'c-awk-NL-prop)))
         (progn (setq bol-pos (c-point 'bopl))
                (setq bsws-pos (point))
                ;; N.B. the following function will not go back past an EOL if
                ;; there is an open string (without \) on the previous line.
                (or (/= (c-backward-syntactic-ws bol-pos) bsws-pos)
                    (progn (setq nl-prop ?\;)
                           nil)))
           ;; If we had a backslash at EOL, c-backward-syntactic-ws will
           ;; have gone backwards over it.  Check the backslash was "real".
         (progn
           (if (looking-at "[ \t]*\\\\+$")
               (if (progn
                     (end-of-line)
                     (search-backward-regexp
                      "\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\$" ; ODD number of \s at EOL  :-)
                      bol-pos t))
                   (progn (end-of-line)   ; escaped EOL.
                          (backward-char)
                          (c-backward-syntactic-ws bol-pos))
                 (end-of-line)))          ; The \ at eol is a fake.
           (bolp))))
    nl-prop))

(defun c-awk-calculate-NL-prop-prev-line (&optional do-lim)
  ;; Calculate and set the value of the c-awk-NL-prop on the immediately
  ;; preceding EOL.  This may also involve doing the same for several
  ;; preceding EOLs.
  ;; NOTE that if the property was already set, we don't recalculate it.
  ;;   (This is by accident rather than design.)
  ;; 
  ;; Return the property set on the previous line.
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (let* ((pos (point))
             (nl-prop (c-awk-back-to-contentful-text-or-NL-prop)))
        ;; We are either (1) at a BOL (with nl-prop containing the previous
        ;; line's c-awk-NL-prop) or (2) after contentful text on a line.  At
        ;; the BOB counts as case (1), so we test next for bolp rather than
        ;; non-nil nl-prop.
        (when (not (bolp))
          (setq nl-prop
                (cond
                 ;; Incomplete statement which doesn't require escaped EOL?
                 ((or (c-awk-after-if-do-for-while-condition-p do-lim) ; CASE 2
                      (c-awk-after-function-decl-param-list) ; CASE 4
                      (c-awk-after-continue-token)) ; CASE 3
                  ?\{)
                 ;; Escaped EOL (where there's also something to continue)?
                 ((and (looking-at "[ \t]*\\\\$")
                       (not (c-awk-after-rbrace-or-statement-semicolon)))
                  ?\\)
                 (t ?\;)))            ; A statement was completed on this line
          (end-of-line)
          (put-text-property (point) (1+ (point)) 'c-awk-NL-prop nl-prop)
          (forward-line))

        ;; We are now at the start of a (possibly empty) sequence of
        ;; content-free lines, nl-prop containing the c-awk-NL-prop value from
        ;; the previous line.  Go round the following loop once for each such
        ;; line, setting the appropriate value of the property on each \n,
        ;; until we get back to our starting point.
        (while (< (point) pos)
          ;; possibilities are:   ?\; -> ?\#  ALWAYS (complete stat -> empty)
          ;;                      ?\# -> ?\#  ALWAYS (empty stays empty)
          ;;                      ?\\ -> ?\\ or ?\#  (still got escaped eol?)
          ;;                      ?\{  -> ?\{  ALWAYS (open stat remains open)
          (cond
           ((eq nl-prop ?\;) (setq nl-prop ?\#))
           ((eq nl-prop ?\\)
            (if (not (looking-at "[ \t]*\\\\$")) (setq nl-prop ?\#))))
          (forward-line)
          (put-text-property (1- (point)) (point) 'c-awk-NL-prop nl-prop))
        nl-prop))))

(defun get-c-awk-NL-prop-prev-line (&optional do-lim)
  ;; Get the c-awk-NL-prop text-property from the previous line, calculating
  ;; it if necessary.  Return nil iff we're already at BOB.
  (if (bobp)
      nil
    (or (get-text-property (c-point 'eopl) 'c-awk-NL-prop)
        (c-awk-calculate-NL-prop-prev-line do-lim))))

(defun c-awk-prev-line-incomplete-p (&optional do-lim)
;;  awk-mode: Is there an incomplete statement at the end of the previous line?

  ;; Four cases to consider, with respect to the previous line:
  ;;  1: A '\' continuation marker at EOL (only valid where there's no comment) 
  ;;     We need to check for '\'s on consecutive (otherwise) empty lines
  ;;     Even if all the above hold, we still need to ensure that there's
  ;;     something to continue;
  ;;  2: A line: "if/while/for/do (...)" (without ';') [careful about do-while :-];
  ;;  3: A line ending with one of ,, {, ?, :, &&, ||, do, else
  ;;  4: A line "function foo (a, b)", possibly followed by a comment.  (The next
  ;;     line then must start with '{'.  Should we check this? NO!!
  ;;
  ;; Note: we use the previous line as part of the definition so that the
  ;; various backward-comment functions work.
  (memq (get-c-awk-NL-prop-prev-line do-lim) '(?\\ ?\{)))

(defun c-awk-clear-NL-props (beg end)
  ;; This function is run from before-change-hooks.  It clears the
  ;; c-awk-NL-prop text property from beg to the end of the buffer (The END
  ;; parameter is ignored).  This ensures that the indentation engine will
  ;; never use stale values for this property.
  (save-restriction
    (widen)
    (put-text-property beg (point-max) 'c-awk-NL-prop nil)))

(defun c-awk-completed-stmt-ws-ends-prev-line-p (&optional do-lim)
  ;;  Is there a termination of a statement as the last thing (apart from an
  ;;  optional comment) on the previous line?
  (eq (get-c-awk-NL-prop-prev-line do-lim) ?\;))

(defun c-awk-completed-stmt-ws-ends-line-p (&optional pos do-lim)
  ;;  Same as previous function, but for the line containing position POS (or
  ;;  the current line if POS is omitted).  If this line lacks an EOL, nil
  ;;  will be returned.  (i.e., the line isn't terminated at all ;-)
  (save-excursion
    (if pos (goto-char pos))
    (and (eq (forward-line) 0)  (bolp)  ; check for unterminated line at EOB.
         (c-awk-completed-stmt-ws-ends-prev-line-p do-lim))))

(defun c-awk-after-logical-semicolon (&optional do-lim)
;; Are we at BOL, the preceding EOL being a "logical semicolon"?
  (and (bolp)
       (eq (get-c-awk-NL-prop-prev-line do-lim) ?\;)))

(defun c-awk-backward-syntactic-ws (&optional lim) 
;; Skip backwards over awk-syntactic whitespace.  Awk-syntactic whitespace is
;; defined as whitespace characters, comments, and NEWLINES WHICH AREN'T
;; "VIRTUAL SEMICOLONS".  However if point starts inside a comment or
;; preprocessor directive, the content of it is not treated as whitespace.
;; LIM sets a lower limit of the backward movement, if specified.
  (let ((lim (or lim (point-min))))
    (while
        (and (> (point) lim)
             (progn (c-backward-syntactic-ws (max lim (c-point 'bol)))
                    (bolp))
             (/= (get-c-awk-NL-prop-prev-line) ?\;)))))

(defun c-awk-NL-prop-not-set ()
  ;; Is the NL-prop on the current line either nil or unset?
  (not (get-text-property (c-point 'eol) 'c-awk-NL-prop)))

(defun c-awk-unstick-NL-prop ()
  ;; Ensure that the text property c-awk-NL-prop is "non-sticky".  Without
  ;; this, a new newline inserted after an old newline (e.g. by C-j) would
  ;; inherit any c-awk-NL-prop from the old newline.  This would be a Bad
  ;; Thing.
  (if (not (assoc 'c-awk-NL-prop text-property-default-nonsticky))
      (setq text-property-default-nonsticky
            (cons '(c-awk-NL-prop . t) text-property-default-nonsticky))))
  
;; The following is purely a diagnostic command, to be commented out of the
;; final release.  ACM, 2002/6/1
(defun NL-props ()
  (interactive)
  (let (pl-prop cl-prop)
    (message "Prev-line: %s  Cur-line: %s"
             (if (setq pl-prop (get-text-property (c-point 'eopl) 'c-awk-NL-prop))
                 (char-to-string pl-prop)
               "nil")
             (if (setq cl-prop (get-text-property (c-point 'eol) 'c-awk-NL-prop))
                 (char-to-string cl-prop)
               "nil"))))
(define-key awk-mode-map [?\C-c ?\r] 'NL-props)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-awk-beginning-of-logical-line (&optional pos)
  ;;   awk-mode: Go back to the start of the (apparent) current line (or the
  ;;   start of the line containing POS).
  ;; Return the buffer position of that point.  I.e., go back to the last line
  ;; which doesn't have an escaped EOL before it.  This is guaranteed to be
 ;; "safe" for syntactic analysis, i.e. outwith any comment, string or regexp.
  ;; IT MAY WELL BE that this function should not be executed on a narrowed
  ;; buffer.
  (if pos (goto-char pos))
  (forward-line 0)
  (while (and (> (point) (point-min))
              (eq (char-before (1- (point))) ?\\))
    (forward-line -1))
  (point))

(defun c-awk-end-of-logical-line (&optional pos)
;;   awk-mode: Go forward to the end of the (apparent) current logical line
;; (or the end of the line containing POS).
;; Return the buffer position of that point.  I.e., go to the end of the next
;; line which doesn't have an escaped EOL.  This is guaranteed to be "safe" for
;; syntactic analysis, i.e. outwith any comment, string or regexp.  IT MAY WELL
  ;; BE that this function should not be executed on a narrowed buffer.
  (if pos (goto-char pos))
  (end-of-line)
  (while (and (< (point) (point-max))
              (eq (char-before) ?\\))
    (end-of-line 2))
  (point))

;; ACM 2002/5/25.  When font-locking is invoked by a buffer change, the region
;; specified by the font-lock after-change function must be expanded to
;; include ALL of any string or regexp within the region.  The simplest way to
;; do this in practice is to use the beginning/end-of-logical-line functions.
;; Don't overlook the possibility of the buffer change being the "recapturing"
;; of a previously escaped newline.
(defmacro c-awk-advise-fl-for-awk-region (function)
  `(defadvice ,function (before get-awk-region activate)
  "When font-locking an awk-mode buffer, make sure that any string/regexp is completely font-locked."
  (when (eq major-mode 'awk-mode)
    (save-excursion
      (ad-set-arg 0 (c-awk-beginning-of-logical-line (ad-get-arg 0)))
      (ad-set-arg 1 (progn (goto-char (ad-get-arg 1))
                           (forward-line 1) ; Maybe a "recaptured" NL.
                           (c-awk-end-of-logical-line)))))))

(c-awk-advise-fl-for-awk-region font-lock-after-change-function)
(c-awk-advise-fl-for-awk-region jit-lock-after-change)
(c-awk-advise-fl-for-awk-region lazy-lock-defer-rest-after-change)
(c-awk-advise-fl-for-awk-region lazy-lock-defer-line-after-change)

;;; ACM, 2002/02/02:  Give up the FSM approach, and use regexps as suggested by
;;; Martin S. in his Email of 19th January 2002.

;; N.B. In the following regexps, an EOL is either \n OR \r.  This is because
;; Emacs has in the past used \r to mark hidden lines in some fashion (and
;; maybe still does).

(defconst c-awk-esc-pair-re "\\\\\\(.\\|\n\\|\r\\|\\'\\)")
;;   Matches any escaped (with \) character-pair, including an escaped newline.
(defconst c-awk-comment-without-nl "#.*")
;; Matches an awk comment, not including the terminating NL (if any).  Note
;; that the "enclosing" (elisp) regexp must ensure the # is real.
(defconst c-awk-nl-or-eob "\\(\n\\|\r\\|\\'\\)")
;; Matches a newline, or the end of buffer.
(defconst c-awk-$-or-eob "\\($\\|\\'\\)")
;; Matches the end of line (without consuming it) or the end of buffer.
(defconst c-awk-whitespace-and-nls*-here-re
  (concat "\\=\\(" c-awk-comment-without-nl "\\|"
          "\\\\?" "\\([ \t]\\|" c-awk-nl-or-eob "\\)"
          "\\)*"))
;; Matches any amount of whitespace and newlines (including none at all).  It
;; assumes that the start point of a search is outwith any string or comment.

;; The next three are used in the font-lock settings.
(defconst c-awk-escaped-nl "\\\\[\n\r]") 
;; Matches an escaped newline.
(defconst c-awk-escaped-nls* (concat "\\(" c-awk-escaped-nl "\\)*"))
;; Matches a possibly empty sequence of escaped newlines.
(defconst c-awk-escaped-nls*-with-space*
  (concat "\\(" c-awk-escaped-nl "\\|" "[ \t]" "\\)*"))
;; Matches a possibly empty sequence of escaped newlines with optional
;; interspersed spaces and tabs.

;; REGEXPS FOR "HARMLESS" STRINGS/LINES.
(defconst c-awk-harmless-char-re "[^_#/\"\\\\\n\r]")
;;   Matches any character but a #, /, ", \, or newline.  N.B. _" starts a
;; localisation string in gawk 3.1
(defconst c-awk-harmless-_ "_\\([^\"]\\|\\'\\)")
;;   Matches an underline NOT followed by ".
(defconst c-awk-harmless-string*-re
  (concat "\\(" c-awk-harmless-char-re "\\|" c-awk-esc-pair-re "\\|" c-awk-harmless-_ "\\)*"))
;;   Matches a (possibly empty) sequence of chars without unescaped /, ", \,
;; #, or newlines.
(defconst c-awk-harmless-string*-here-re
  (concat "\\=" c-awk-harmless-string*-re))
;; Matches the (possibly empty) sequence of chars without unescaped /, ", \,
;; at point.
(defconst c-awk-harmless-line-re
;;  (concat c-awk-harmless-string*-re "\\(#.*\\)?\\(\n\\|\r\\|\\'\\)"
  (concat c-awk-harmless-string*-re
          "\\(" c-awk-comment-without-nl "\\)?" c-awk-nl-or-eob))
;;   Matches (the tail of) an awk \"logical\" line not containing an unescaped
;; " or /.  "logical" means "possibly containing escaped newlines".  A comment
;; is matched as part of the line even if it contains a " or a /.  The End of
;; buffer is also an end of line.
(defconst c-awk-harmless-lines+-here-re
  (concat "\\=\\(" c-awk-harmless-line-re "\\)+"))
;; Matches a sequence of (at least one) \"harmless-line\" at point.


;; REGEXPS FOR AWK STRINGS.
(defconst c-awk-string-ch-re "[^\"\\\n\r]")
;; Matches any character which can appear unescaped in a string.
(defconst c-awk-string-innards-re
  (concat "\\(" c-awk-string-ch-re "\\|" c-awk-esc-pair-re "\\)*"))
;;   Matches the inside of an awk string (i.e. without the enclosing quotes).
(defconst c-awk-string-noeol-re
  (concat "_?\"" c-awk-string-innards-re "\\(\"\\|" c-awk-$-or-eob "\\)"))
;;   Matches an awk string, without swallowing EOL if it's missing its terminating ".
;; A gawk 3.1+ string may look like _"localisable string".
(defconst c-awk-string-re
  (concat "_?\"" c-awk-string-innards-re "\\(\"\\|" c-awk-nl-or-eob "\\)"))
;;   Matches an awk string at point, including EOL/EOB if it's missing its
;;   terminating \".
;; A gawk 3.1+ string may look like _"localisable string".
(defconst c-awk-string-here-re
  (concat "\\=" c-awk-string-re))
;;(defconst c-awk-unterminated-string-re
;;  (concat "\\(\"\\)"         ; This gives "subexpression 1" in the match data.
;;          c-awk-string-innards c-awk-nl-or-eob))
;;   Matches an unterminated awk-string, including the final unescaped EOL (or
;; up to EOB).  The opening \" constitutes subexpression 1 in the match data.


;; REGEXPS FOR AWK REGEXPS.
(defconst c-awk-regexp-normal-re "[^[/\\\n\r]")
;;   Matches any awk regexp character which doesn't require special analysis.
(defconst c-awk-escaped-newlines*-re "\\(\\\\[\n\r]\\)*")
;;   Matches a (possibly empty) sequence of escaped newlines.
(defconst c-awk-regexp-char-class-re
  (concat "\\[" c-awk-escaped-newlines*-re "^?" c-awk-escaped-newlines*-re "]?"
          "\\(" c-awk-esc-pair-re "\\|" "[^]\n\r]" "\\)*" "\\(]\\|$\\)"))
;;   Matches a regexp char class, up to (but not including) EOL if the ] is
;;   missing.
(defconst c-awk-regexp-innards-re
  (concat "\\(" c-awk-esc-pair-re "\\|" c-awk-regexp-char-class-re
          "\\|" c-awk-regexp-normal-re "\\)*"))
;;   Matches the inside of an awk regexp (i.e. without the enclosing /s)
(defconst c-awk-regexp-re
  (concat "/" c-awk-regexp-innards-re "\\(/\\|" c-awk-nl-or-eob "\\)"))
;; Matches an awk regexp, including EOL/EOB if it's missing its terminating /.
(defconst c-awk-regexp-noeol-re
  (concat "/" c-awk-regexp-innards-re "\\(/\\|" c-awk-$-or-eob "\\)"))
;; Matches an awk regexp, without swallowing EOL/EOB, should it be missing its
;; terminating /.

;; REGEXPS used for scanning an awk buffer in order to decide IF A '/' IS A
;; REGEXP OPENER OR A DIVISION SIGN.  By "state" in the following is meant
;; whether a '/' at the current position would by a regexp opener or a
;; division sign.
(defconst c-awk-neutral-re
  "\\([{}@` \t]\\|\\+\\+\\|--\\|\\\\.\\)+")
;;   A "neutral" char(pair).  Doesn't change the "state" of a subsequent /.
;; This is space/tab, any shape of bracket, an auto-increment/decrement
;; operator or an escaped character.  Or one of the (illegal) characters @ or
;; `.  But NOT an end of line.
(defconst c-awk-neutrals*-re
  (concat "\\(" c-awk-neutral-re "\\)*"))
;;   A (possibly empty) string of neutral characters (or character pairs).
(defconst c-awk-var-num-ket-re "[]\)0-9a-zA-Z_$.\x80-\xff]+")
;;   Matches a char which is a constituent of a variable or number, or a ket
;; (i.e. closing braKET), round or square.  Assume that all characters \x80 to
;; \xff are "letters".
(defconst c-awk-div-sign-re
  (concat c-awk-var-num-ket-re c-awk-neutrals*-re "/"))
;;   Will match a string ending in / which is a division sign, in a context
;; where an immediate / would be a regexp bracket.  It follows a variable or
;; number (with optional intervening "neutral" characters).  This will only
;; work when there won't be a preceding " or / to foul things up.
(defconst c-awk-non-arith-op-bra-re
  "[[\(&=:!><,?;'~|]")
;;   Matches any operator character apart from +,-,/,*,%.  For the purpose at
;; hand (detecting a / which is a regexp bracket) these arith ops are
;; unnecessary and a pain, because of "++" and "--".
(defconst c-awk-regexp-sign-re
  (concat c-awk-non-arith-op-bra-re c-awk-neutrals*-re "/"))
;;   Will match a string ending in / which is an opening regexp bracket, in a
;; context where an immediate / would be a division sign.  This will only work
;; when there won't be a preceding " or / to foul things up.

;; ACM, 2002/02/15: The idea of the next function is to put the "Error font"
;; on strings/regexps which are missing their closing delimiter.
;; 2002/4/28.  The default syntax for / has been changed from "string" to
;; "punctuation", to reduce hassle when this character appears within a string
;; or comment.

(defun c-awk-set-string-regexp-syntax-table-properties (beg end)
;;   BEG and END bracket a string or regexp.  "String" here can also mean a
;; gawk 3.1 "localizable" string which starts with _".  In this case, we step
;; over the _ and ignore it; It will get it's font from an entry in
;; awk-font-lock-keywords.
;;
;; If the closing delimiter is missing (i.e., there is an EOL there) set the
;; STRING-FENCE property on the opening " or / and closing EOL.
  (if (eq (char-after beg) ?_) (setq beg (1+ beg)))
  (cond ((< (- (point-max) beg) 2)   ; BEG is at end of buffer
         (put-text-property beg (1+ beg) 'syntax-table '(15))) ; (15) = "string fence"
        ((/= (char-after beg) (char-before end)) ; mismatched "delimiters"
         (put-text-property beg (1+ beg) 'syntax-table '(15))
         (if (< end (point-max)) ; If END is at EOB, don't mark the last char.
             (put-text-property (1- end) end 'syntax-table '(15))))
        ((eq (char-after beg) ?/)       ; Properly bracketed regexp
         (put-text-property beg (1+ beg) 'syntax-table '(7)) ; (7) = "string"
         (put-text-property (1- end) end 'syntax-table '(7)))
        (t)))                       ; Properly bracketed string. Nothing to do.
         
(defun c-awk-set-syntax-table-properties (lim)
;;     Scan the buffer text between point and LIM, setting (and clearing) the
;; syntax-table property where necessary.

;; This function is designed to be called as the FUNCTION in a MATCHER in
;; font-lock-syntactic keywords, and it always returns NIL (to inhibit
;; repeated calls from font-lock).  It can also, with a bit of glue, be called
;; from after-change-functions.  Point is left "undefined" after this function
;; exits.  THE BUFFER SHOULD HAVE BEEN WIDENED, AND ANY PRECIOUS MATCH-DATA
;; SAVED BEFORE CALLING THIS ROUTINE.

;; We need to set/clear the syntax-table property on:
;; (i) / - It is set to "string" on a / which is the opening or closing
;;     delimiter of the properly terminated regexp.
;; (ii) On an unterminated string/regexp opener, we set the property "generic
;; string delimiter" on both the opening " or / and the end of the line where
;; the closing delimiter is missing.
;; (iii) Inside a comment, all syntax-table properties are cleared.
    (let (anchor
          (anchor-state-/div nil)) ; t means a following / would be a div sign.
      (c-awk-beginning-of-logical-line)
      (put-text-property (point) lim 'syntax-table nil)
      (search-forward-regexp c-awk-harmless-lines+-here-re nil t) ; skip harmless lines.

      ;; Once round the next loop for each string, regexp, or div sign
      (while (< (point) lim)
        (setq anchor (point))
        (search-forward-regexp c-awk-harmless-string*-here-re nil t)
        ;; We are now looking at either a '"' or a '/'.
        (if (looking-at c-awk-string-re) ; a string
            (progn (setq anchor-state-/div t) ; ("15" / 5) gives 3 in awk ;-)
                   (c-awk-set-string-regexp-syntax-table-properties
                    (match-beginning 0) (match-end 0))
                   (search-forward-regexp c-awk-string-noeol-re nil t)) ; go to end of string but not over an EOL.
                   
          ;; We've got a '/'.  Is it a division sign or the start of a regexp?
          ;; We need to go back to anchor, where the state is known, then scan
          ;; forward.  Between anchor and point, there is exactly one /.
          (setq /point (point))
          (goto-char anchor)
          (if (or
               (and (not anchor-state-/div) (search-forward-regexp c-awk-div-sign-re (1+ /point) t))
               (and anchor-state-/div
                    (not (search-forward-regexp c-awk-regexp-sign-re (1+ /point) t))))
              ;; We've got a '/' which is a division sign.
              (progn
                (setq anchor-state-/div nil)
                (goto-char (1+ /point)))
            ;; We've got a '/' which opens a regexp.  Currently, Point may be
            ;; at either ANCHOR or (1+ /POINT).  The latter's no good, so....
            (goto-char /point)
            (looking-at c-awk-regexp-re) ; Set the match data. We KNOW we've got a regexp.
            (setq anchor-state-/div t)
            (c-awk-set-string-regexp-syntax-table-properties
             (match-beginning 0) (match-end 0))
            (search-forward-regexp c-awk-regexp-noeol-re))) ; jump over the regexp, but not past an EOL

        ;; Skip any further "harmless" lines before the next tricky line. 
        (if (search-forward-regexp c-awk-harmless-lines+-here-re nil t)
            (setq anchor-state-/div nil)))
      nil))


;; ACM, 2002/02/15: The next function hasn't been fully thought out yet.
;; Don't use it, it won't work!
(defun c-awk-/syntax-after-change (beg end &optional old-len)
  "Text in the buffer between BEG and END has been changed.  The previous
length of text there was OLD-LEN, but we don't use this.  Set the syntax-table
property as necessary on any '/' characters whose meaning may have
changed. This function is designed to be called as a hook from
after-change-functions."
  (save-restriction
    (widen)
    (let* ((cur-state /bol)
           (inhibit-point-motion-hooks t) ; copied from font-lock.el
           (buf-pos (save-excursion ((c-awk-back-to-safe) (point)))))
      ;; Calculate what CUR-STATE is at BEG.
      (while (< buf-pos beg)
        (setq cur-state (c-awk-new-state (char-after bufpos) cur-state)))
      ;; Run through the FSM, setting the syntax-table text property on each
      ;; '/' until the first (uncontinued) \n after END.
      (while (and (< buf-pos end) (not (eq cur-state '/bol)))
        (if (eq (char-after bufpos) ?/)
            (put-text-property bufpos (1+ bufpos) 'syntax-table
               (if (memq cur-state (list /bol /ws-re /re /misc-re /misc-- /misc-+))
                    nil '(1)))            ; (1) means "punctuation"
          (setq cur-state (c-awk-new-state (char-after bufpos) cur-state))
          (setq bufpos (1+ bufpos)))))))

;; Regexps written with help from Peter Galbraith <galbraith@mixing.qc.dfo.ca>.
(defconst awk-font-lock-keywords
  (eval-when-compile
    (list
     ;; Function names.
     '("^[ \t]*\\(func\\(tion\\)?\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (c-regexp-opt
	    '("ARGC" "ARGIND" "ARGV" "BINMODE" "CONVFMT" "ENVIRON" "ERRNO"
	      "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "LINT" "NF"
              "NR" "OFMT" "OFS" "ORS" "PROCINFO" "RLENGTH" "RS" "RSTART" "RT"
              "SUBSEP" "TEXTDOMAIN" "dev/stdin" "/dev/stdout" "dev/stderr") 'words)
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (c-regexp-opt
      '("BEGIN" "END" "break" "continue" "delete" "do" "exit" "else" "for"
	"getline" "if" "in" "next" "print" "printf" "return" "while") 'words)
     ;;
     ;; Builtins.
     (list (c-regexp-opt
	    '("and" "asort" "atan2" "bindtextdomain" "close" "compl" "cos"
              "ctime" "dcgettext" "exp" "extension" "fflush" "gensub" "gsub"
              "index" "int" "length" "log" "lshift" "match" "mktime" "nextfile"
              "or" "rand" "rshift" "sin" "split" "sprintf" "sqrt" "srand"
              "strftime" "strtonum" "sub" "substr" "system" "systime" "time"
              "tolower" "toupper" "xor") 'words)
	   1 'font-lock-builtin-face)

     ;; User defined functions with a space before the opening parenthesis.
     ;; ACM, 2002/5/30.
     `(,(concat "\\(\\w\\|_\\)" c-awk-escaped-nls* "[ \t]"
             c-awk-escaped-nls*-with-space* "(") (0 'font-lock-warning-face))

     ;; Space after \ in what looks like an escaped newline.  ACM, 2002/5/31
     '("\\\\[ \t]+$" 0 font-lock-warning-face t)

     ;; Operators.  Is this too much?
     ;; ACM, 2002/5/26.  Yes, this is _far_ too much!  Take it out
;; (cons (c-regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~"))
;; 	   'font-lock-constant-face)

     ;; Unbalanced string (") or regexp (/) delimiters.  ACM 2002/02/16.
     '("\\s|" 0 font-lock-warning-face t nil)
;; Patterns for gawk 3.1 localizable strings ( _"translate me!").  ACM, 2002/5/21
     '("\\(_\\)\\s|" 1 font-lock-warning-face)
     '("\\(_\\)\\s\"" 1 font-lock-string-face)
     ))
 "Default expressions to highlight in AWK mode.")


(provide 'cc-awk)                       ; Changed from 'awk-mode, ACM 2002/5/21

;;; awk-mode.el ends here

