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
  (modify-syntax-entry ?\r ">   " st)   ; ACM, 2002/3/24
  (modify-syntax-entry ?\f ">   " st)
  (modify-syntax-entry ?\# "<   " st)
  ;; / can delimit regexes or be a division operator.  We assume that it is
  ;; more commonly used for regexes and fix the remaining cases with
  ;; `font-lock-syntactic-keywords'.
  ;; NO! Do it the other way round.  ACM 2002/4/27.
;  (modify-syntax-entry ?/ "\"" st)
  (modify-syntax-entry ?/ "." st)       ; ACM 2002/4/27.  
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
  (modify-syntax-entry ?\' "\"" st)
  st)
  "Syntax table in use in `awk-mode' buffers.")


;; (require 'syntax)
(defun c-awk-back-to-contentful-text ()
;;  awk-mode: move point back to just after the last non-ws text, if any,
;;  keeping track of whether there is a continuous chain of (real) escaped
;;  newlines from that point to the line we started in.
;;  Return value: whether we have this chain of escaped newlines.
;;
;;  Point should be at the start of a line when calling this function.  This
;;  is to ensure that the various backward-comment functions will work properly.
  (let ((bksl-active t)
        (bol-pos (point)))
    (while ;; We are at the beginning of a line here, and go back one line
	   ;; each iteration.
        (and
         (progn (c-backward-syntactic-ws (c-point 'bopl))
                (< (point) bol-pos))    ; Stop if we reach point-min
         (progn
           (setq bol-pos (c-point 'bol))
           ;; If we had a backslash at EOL, c-backward-syntactic-ws will
           ;; have gone backwards over it.  Check the backslash was "real".
           (if (looking-at "[ \t]*\\\\+$")
               (if (progn
                     (end-of-line)
                     (search-backward-regexp
                      "\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\$" ; odd number of \s at EOL  :-)
                      bol-pos t))
                   (progn (end-of-line) ; escaped EOL.
                          (backward-char)
                          (c-backward-syntactic-ws bol-pos))
                 (end-of-line) ; The \ at eol is a fake.
                 (setq bksl-active nil))
             (setq bksl-active nil))    ; no \ at EOL
           (bolp)))) ; If we moved back to BOL, there was nothing on the line.
    bksl-active))

(defun c-awk-after-if-do-for-while-condition-p (&optional lim)
;; Are we just after the ) in "if/do/for/while (<condition>)"?
;; Note that at the end of the ) in a do .... while (<condition>) doesn't count.
  (and
   (eq (char-before) ?\))
   (save-excursion
     (goto-char (c-safe (scan-lists (point) -1 0))) ; back over "(...)"
     (c-backward-token-1)
     (or (looking-at "if\\>\\|for\\>\\|do\\>")
         (and (looking-at "while\\>") ; make sure the while isn't from a do-while.
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
                     (looking-at "function\\>"))))))

(defun c-awk-after-continue-token ()
;; Are we just after a token which can be continued onto the next line without
;; a backslash?
  (save-excursion
    (c-backward-token-1)
    (if (looking-at "[&|]") (backward-char)) ; c-backward-token-1 doesn't do this :-(
    (looking-at "[,{?:]\\|&&\\|||\\|do\\>\\|else\\>")))

(defun c-awk-after-statement-semicolon ()
;; Are we just after a ; which closes a statement?
;; Be careful about ;s in for loop control bits.  They don't count!
  (and
   (eq (char-before) ?\;)
   (not (save-excursion
          (goto-char (c-safe (scan-lists (point) -1 1))) ; go back to containing (
          (and (looking-at "(")
               (c-backward-token-1)
               (looking-at "for\\>"))))))

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
  (save-excursion
    (save-match-data
      (let (bksl-active) ; "Chain of BacKSLashed newlines to the starting line."
            
        ;; Go back over any content-free lines, so that point is just after
        ;; the last significant character.
        (beginning-of-line)
        (setq bksl-active (c-awk-back-to-contentful-text))

        (and (> (point) (point-min))  ; start of buffer ??
             (or (c-awk-after-if-do-for-while-condition-p do-lim) ; CASE 2
                 (c-awk-after-function-decl-param-list) ; CASE 4
                 (c-awk-after-continue-token) ; CASE 3
                 (and bksl-active       ; CASE 1 (active escaped eol).
                      (not (c-awk-after-statement-semicolon)))))))))

(defun c-awk-complete-stmt-on-prev-line-p (&optional do-lim)
;;  awk-mode: Is there a complete statement on the previous line?  This
;;  divides into two parts: i) Is there a statement on the previous line?
;;  ii) Is it complete?
  (and
   (save-excursion
     ;; Go back over whitespace on the previous line;
     (beginning-of-line)
     (c-backward-syntactic-ws (c-point 'bopl))
     (> (point) (c-point 'bol))) ;; Do we have any meat on this previous line?
   (not (c-awk-prev-line-incomplete-p))))

(defun c-awk-beginning-of-logical-line ()
;;   awk-mode: Go back to the start of the (apparent) current logical line.
;; Return the buffer position of that point.  I.e., go back to the last line
;; which doesn't have an escaped EOL before it.  This is guaranteed to be
;; "safe" for syntactic analysis, i.e. outwith any comment, string or regexp.
;; IT MAY WELL BE that this function should not be executed on a narrowed
;; buffer.
  (forward-line 0)
  (while (and (> (point) (point-min))
              (eq (char-before (1- (point))) ?\\))
    (forward-line -1))
  (point))

(defun c-awk-end-of-logical-line ()
;;   awk-mode: Go forward to the end of the (apparent) current logical line.
;; Return the buffer position of that point.  I.e., go to the end of the next
;; line which doesn't have an escaped EOL.  This is guaranteed to be "safe" for
;; syntactic analysis, i.e. outwith any comment, string or regexp.  IT MAY WELL
;; BE that this function should not be executed on a narrowed buffer.
  (end-of-line)
  (while (and (< (point) (point-max))
              (eq (char-before) ?\\))
    (end-of-line 2))
  (point))

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

;; REGEXPS FOR "HARMLESS" STRINGS/LINES.
(defconst c-awk-harmless-char-re "[^#/\"\\\\\n\r]")
;;   Matches any character but a #, /, ", \, or newline
(defconst c-awk-harmless-string*-re
  (concat "\\(" c-awk-harmless-char-re "\\|" c-awk-esc-pair-re "\\)*"))
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
  (concat "\"" c-awk-string-innards-re "\\(\"\\|" c-awk-$-or-eob "\\)"))
;;   Matches an awk string, without swallowing EOL if it's missing its terminating ".
(defconst c-awk-string-re
  (concat "\"" c-awk-string-innards-re "\\(\"\\|" c-awk-nl-or-eob "\\)"))
;;   Matches an awk string at point, including EOL/EOB if it's missing its
;;   terminating \".
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

;; (defun c-awk-set-string-regexp-syntax-table-properties (beg end)
;; ;;   BEG and END bracket a string or regexp.  If the closing delimiter is
;; ;; missing (i.e., there is an EOL there) set the STRING-FENCE property on the
;; ;; opening " or / and closing EOL.
;;   (when (or (< (- end beg) 2)   ; Only a single delimiter - can happen at EOB.
;;             (/= (char-after beg) (char-before end)))
;;     (put-text-property beg (1+ beg) 'syntax-table '(15)) ; (1) = "string fence".
;;     (put-text-property (1- end) end 'syntax-table '(15))))

(defun c-awk-set-string-regexp-syntax-table-properties (beg end)
;;   BEG and END bracket a string or regexp.  If the closing delimiter is
;; missing (i.e., there is an EOL there) set the STRING-FENCE property on the
;; opening " or / and closing EOL.
  (cond ((< (- (point-max) beg) 2)   ; At end of buffer
         (put-text-property beg (1+ beg) 'syntax-table '(15))) ; (15) = "string fence"
        ((/= (char-after beg) (char-before end)) ; mismatched "delimiters"
         (put-text-property beg (1+ beg) 'syntax-table '(15))
         (put-text-property (1- end) end 'syntax-table '(15)))
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
     '("^[ \t]*\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (regexp-opt
	    '("ARGC" "ARGIND" "ARGV" "CONVFMT" "ENVIRON" "ERRNO"
	      "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "NF" "NR"
	      "OFMT" "OFS" "ORS" "RLENGTH" "RS" "RSTART" "SUBSEP") 'words)
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (regexp-opt
      '("BEGIN" "END" "break" "continue" "delete" "exit" "else" "for"
	"getline" "if" "next" "print" "printf" "return" "while") 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '("atan2" "close" "cos" "ctime" "exp" "gsub" "index" "int"
	      "length" "log" "match" "rand" "sin" "split" "sprintf"
	      "sqrt" "srand" "sub" "substr" "system" "time"
	      "tolower" "toupper") 'words)
	   1 'font-lock-builtin-face)
     ;;
     ;; Operators.  Is this too much?
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~"))
	   'font-lock-constant-face)
     ;; Unbalanced string (") or regexp (/) delimiters.  ACM 2002/02/16.
     '("\\s|" 0 font-lock-warning-face t nil)
     ))
 "Default expressions to highlight in AWK mode.")


(provide 'cc-awk)                       ; Changed from 'awk-mode, ACM 2002/4/21

;;; awk-mode.el ends here
