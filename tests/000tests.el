;; Regression test suite harness

;; You can run the regression test either within your current X/Emacs
;; session, or via batch mode.  I usually run the latter first, to
;; make sure my changes haven't messed anything up.  If this passes,
;; everything is fine.  If it fails, I run the test in the current
;; session until I fix the breakage.

;; To run in the current session, make sure you have all the latests
;; definitions of any function you've changed.  Then load this file
;; and type:
;;
;;    M-x do-all-tests RET
;;
;; This regression tests all the files listed in the variable
;; list-of-tests.  Note that this form of testing takes over X/Emacs
;; until the test completes, and the tests can take a while!

;; To run in batch mode, make sure all the changed .el files are byte
;; compiled and make sure they will be found first on your load-path,
;; then at the shell prompt do this:
;;
;;    % cd .../cc-mode/tests
;;    % xemacs -batch -l 000tests.el -f do-all-tests
;;
;; Obviously replacing `xemacs' with `emacs' if necessary.

;; The tests stop if any regression is found.  With in-session
;; testing, you can restart the tests from where it left off by just
;; doing a M-x do-all-tests again.  Batch testing starts over from the
;; beginning.

;; Whenever I add a new syntactic symbol, or add a new case to the big
;; c-guess-basic-syntax cond, or find and fix a new breakage, I create
;; a new regression test by:
;;
;; 1) creating a small, but complete test case in a file with the
;;    proper extension.  This file must have a unique name within the
;;    tests directory (sans the extension).
;; 2) add the file to the variable list-of-tests
;; 3) Create a `results' (.res) file with the base name of the file,
;;    appended with .res.  Split the current window so that you have
;;    two buffers visible, the new test file buffer on top, and the
;;    .res file on bottom.  Put point at the top of the new test file
;;    and type `M-x resfile'.  This will populate the .res file.
;;    Verify that the results are what you expect, then save both
;;    files, and check then into CVS.

;; Some times the tests will fail without an actual regression being
;; introduced.  This might happen if, e.g. the default Java style
;; changes.  In this case, you can modify the corresponding .res file
;; instead of fixing the regression, but be VERY careful when doing
;; this.  Make sure you know this is what you want to do!
;;
;; To do this, you want to make sure the test source is indented
;; properly, then clear the .res file and regenerate it using step #3
;; above.

;; The eval depth can be rather deep when CC Mode isn't compiled.
(setq max-lisp-eval-depth 1000)

(let ((srcdir (expand-file-name (concat default-directory ".."))))
  (setq load-path (cons srcdir load-path)))
(require 'cc-mode)

;; This is bogus and should eventually go away
(c-initialize-cc-mode)

(defconst TESTSTYLE
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-hanging-braces-alist        . ((block-open after)
				      (brace-list-open)
				      (substatement-open after)
				      ))
    (c-hanging-colons-alist        . ((member-init-intro before)
				      (inher-intro)
				      (case-label after)
				      (label after)
				      (access-key after)))
    (c-cleanup-list                . (scope-operator
				      empty-defun-braces
				      defun-close-semi))
    (c-offsets-alist
     . ((string                . -1000)
	(c                     . c-lineup-C-comments)
	(defun-open            . 0)
	(defun-close           . 0)
	(defun-block-intro     . +)
	(class-open            . 0)
	(class-close           . 0)
	(inline-open           . 0)
	(inline-close          . 0)
	(func-decl-cont        . +)
	(knr-argdecl-intro     . +)
	(knr-argdecl           . 0)
	(topmost-intro         . 0)
	(topmost-intro-cont    . 0)
	(member-init-intro     . +)
	(member-init-cont      . 0)
	(inher-intro           . +)
	(inher-cont            . c-lineup-multi-inher)
	(block-open            . 0)
	(block-close           . 0)
	(brace-list-open       . 0)
	(brace-list-close      . 0)
	(brace-list-intro      . +)
	(brace-list-entry      . 0)
	(statement             . 0)
	;; some people might prefer
	;;(statement             . c-lineup-runin-statements)
	(statement-cont        . +)
	;; some people might prefer
	;;(statement-cont        . c-lineup-math)
	(statement-block-intro . +)
	(statement-case-intro  . +)
	(statement-case-open   . 0)
	(substatement          . +)
	(substatement-open     . +)
	(case-label            . 0)
	(access-label          . -)
	(label                 . *)
	(do-while-closure      . 0)
	(else-clause           . 0)
	(comment-intro         . c-lineup-comment)
	(arglist-intro         . +)
	(arglist-cont          . 0)
	(arglist-cont-nonempty . c-lineup-arglist)
	(arglist-close         . +)
	(stream-op             . c-lineup-streamop)
	(inclass               . +)
	(cpp-macro             . -1000)
	(cpp-macro-cont        . c-lineup-dont-change)
	(friend                . 0)
	(objc-method-intro     . -1000)
	(objc-method-args-cont . c-lineup-ObjC-method-args)
	(objc-method-call-cont . c-lineup-ObjC-method-call)
	(extern-lang-open      . 0)
	(extern-lang-close     . 0)
	(inextern-lang         . +)
	(namespace-open        . 0)
	(namespace-close       . 0)
	(innamespace           . +)
	(template-args-cont    . +)
	(inlambda              . c-lineup-inexpr-stat)
	(lambda-intro-cont     . +)
	(inexpr-statement      . 0)
	(inexpr-block-open     . 0)
	(inexpr-block-intro    . +)
	(inexpr-block-close    . 0)
	))
    (c-echo-syntactic-information-p . t)
    )
  "Style for testing.")

(c-add-style "TESTSTYLE" TESTSTYLE)

(defconst list-of-tests
  '("arglist-1.cc"
    "arglist-2.cc"
    "arglist-3.cc"
    "arglist-4.cc"
    "arglist-5.cc"
    "arglist-6.cc"
    "arglist-7.java"
    "arglist-8.java"
    "bl-1.cc"
    "bl-2.cc"
    "bod-1.cc"
    "bos-1.cc"
    "bos-2.cc"
    "bos-3.cc"
    "bracelist-1.java"
    "bracelist-2.pike"
    "class-1.cc"
    "class-2.cc"
    "class-3.cc"
    "class-4.cc"
    "class-5.cc"
    "class-6.c"
    "class-7.cc"
    "class-8.cc"
    "class-9.cc"
    "class-10.java"
    "class-11.cc"
    "comments.c"
    "comments-1.java"
    "comments-2.c"
    "cond-1.c"
    "decls-1.java"
    "decls-2.java"
    "decls-3.java"
    "enum-1.cc"
    "enum-2.c"
    "enum-3.c"
    "except-1.cc"
    "except-2.cc"
    "except-3.cc"
    "except-4.java"
    "externs-1.cc"
    "externs-2.cc"
    "forloop.cc"
    "funcs-1.cc"
    "if-1.cc"
    "if-2.cc"
    "if-3.cc"
    "if-4.cc"
    "if-5.cc"
    "inexprstat-1.pike"
    "inher-1.cc"
    "inher-2.cc"
    "inher-3.cc"
    "interface-1.m"
    "ivar.java"
    "label-1.c"
    "lambda-1.pike"
    "macro-1.c"
    "member-1.cc"
    "member-2.cc"
    "member-3.cc"
    "methods-1.java"
    "namespace-1.cc"
    "namespace-2.cc"
    "nested-1.cc"
    "nested-2.c"
    "nometh.m"
    "statement-1.cc"
    "statement-2.cc"
    "statement-3.cc"
    "statement-4.cc"
    "statement-5.cc"
    "statement-6.cc"
    "statement-7.c"
    "statement-8.c"
    "statement-9.c"
    "statement-10.c"
    "statement-11.java"
    "stream-1.cc"
    "stream-2.cc"
    "struct-1.c"
    "struct-2.cc"
    "switch-1.cc"
    "switch-2.cc"
    "switch-3.cc"
    "switch-4.cc"
    "switch-5.cc"
    "switch-6.c"
    "synch.java"
    "templates-1.cc"
    "templates-2.cc"
    "templates-3.cc"
    "templates-4.cc"
    "templates-5.cc"
    "top-1.cc"
    "top-2.cc"
    "top-3.cc"
    "typedef-1.c"
    "unbal.c"
    "union.cc"
    ))

(defvar finished-tests nil)

(defun make-test-buffers (filename)
  (let ((testbuf (get-buffer-create "*cc-test*"))
	(resultsbuf (get-buffer-create "*cc-results*"))
	(expectedbuf (get-buffer-create "*cc-expected*"))
	(resfile (concat (file-name-sans-extension filename) ".res")))
    (set-buffer testbuf)
    (erase-buffer)
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((style "TESTSTYLE")
	  c-mode-hook c++-mode-hook objc-mode-hook c-mode-common-hook)
      (cond
       ((string-match "\\.cc$" filename) (c++-mode))
       ((string-match "\\.m$" filename) (objc-mode))
       ((string-match "\\.java$" filename)
	(java-mode)
	(setq style "java"))
       ((string-match "\\.pike$" filename) (pike-mode))
       (t (c-mode)))
      (c-set-style style))
    (set-buffer expectedbuf)
    (erase-buffer)
    (insert-file-contents resfile)
    (text-mode)
    (goto-char (point-min))
    (set-buffer resultsbuf)
    (erase-buffer)
    (list testbuf resultsbuf expectedbuf)))

(defun shutup-parsing-error ())

(defun do-one-test (filename)
  (interactive "fFile to test: ")
  (if (member filename finished-tests)
      nil
    (message "Testing %s..." filename)
    (let* ((baw:c-testing-p t)
	   (buflist (make-test-buffers filename))
	   (testbuf (car buflist))
	   (resultsbuf (nth 1 buflist))
	   (expectedbuf (nth 2 buflist))
	   (pop-up-windows t)
	   (linenum 1)
	   (style "TESTSTYLE")
	   error-found-p
	   )
      (set-buffer testbuf)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((syntax (c-guess-basic-syntax)))
	  (set-buffer resultsbuf)
	  (insert (format "%s" syntax) "\n")
	  (set-buffer testbuf))
	(forward-line 1))
      (set-buffer resultsbuf)
      (goto-char (point-min))
      (set-buffer expectedbuf)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((results (prog2
			   (set-buffer resultsbuf)
			   (buffer-substring (c-point 'bol) (c-point 'eol))
			 (forward-line 1)))
	      (expected (prog2
			    (set-buffer expectedbuf)
			    (buffer-substring (c-point 'bol) (c-point 'eol))
			  (forward-line 1))))
	  (if (string= results expected)
	      nil
	    (set-buffer testbuf)
	    (goto-line linenum)
	    (message "error found on line: %d" linenum)
	    (message "expected: %s, results: %s" expected results)
	    (indent-for-comment)
	    (insert "!TBD: Regression!")
	    (setq error-found-p t)))
	(setq linenum (1+ linenum)))
      (find-file filename)
      (cond
       ((string-match "\\.cc$" filename) (c++-mode))
       ((string-match "\\.m$" filename) (objc-mode))
       ((string-match "\\.java$" filename)
	(java-mode)
	(setq style "java"))
       ((string-match "\\.pike$" filename) (pike-mode))
       (t (c-mode)))
      (c-set-style style)
      (let ((c-progress-interval nil))
	(indent-region (point-min) (point-max) nil))
      (and (buffer-modified-p)
	   (setq error-found-p t))
      (if error-found-p
	  (progn
	    (pop-to-buffer testbuf)
	    (error "Indentation regression found in file: %s!" filename))))
    (setq finished-tests (cons filename finished-tests))
;    (message "Testing %s... done." filename)
    ))

(defun do-all-tests (&optional resetp)
  (interactive "P")
  (c-version)
  (if (consp resetp)
      (setq finished-tests nil))
  ;; TBD: HACK HACK HACK
  (let ((old-c-echo-parsing-error (symbol-function 'c-echo-parsing-error)))
    (fset 'c-echo-parsing-error 'shutup-parsing-error)
    (unwind-protect
	(mapcar 'do-one-test list-of-tests)
      (fset 'c-echo-parsing-error old-c-echo-parsing-error)))
  (message "All tests passed!")
  (setq finished-tests nil))

(defun resfile ()
  (interactive)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (other-window 1)
  (while (not (eobp))
    (let ((syntax (c-guess-basic-syntax)))
      (other-window 1)
      (insert (format "%s\n" syntax)))
    (other-window 1)
    (forward-line 1))
  (delete-backward-char 1))
