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
;; This regression tests all the files in the current directory.  Note
;; that this form of testing takes over X/Emacs until the test
;; completes, and the tests can take a while!

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
;; 2) Create a `results' (.res) file with the base name of the file,
;;    appended with .res.  Split the current window so that you have
;;    two buffers visible, the new test file buffer on top, and the
;;    .res file on bottom.  Put point at the top of the new test file
;;    and type `M-x resfile'.  This will populate the .res file.
;;    Verify that the results are what you expect, then save both
;;    files, and check them into CVS.

;; Some times the tests will fail without an actual regression being
;; introduced.  This might happen if, e.g. the default Java style
;; changes.  In this case, you can modify the corresponding .res file
;; instead of fixing the regression, but be VERY careful when doing
;; this.  Make sure you know this is what you want to do!
;;
;; To do this, you want to make sure the test source is indented
;; properly, then clear the .res file and regenerate it using step #3
;; above.

(require 'compile)
(require 'cl)

(defvar cc-test-dir nil)
(setq cc-test-dir
      (if load-file-name
	  (file-name-directory load-file-name)
	default-directory))

(let ((srcdir (expand-file-name (concat cc-test-dir ".."))))
  (setq load-path (cons srcdir load-path)))
(require 'cc-mode)

(defconst cc-test-teststyle
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-comment-prefix-regexp       . "//+\\|\\**")
    (c-hanging-braces-alist        . ((block-open after)
				      (brace-list-open)
				      (substatement-open after)
				      (inexpr-class-open after)
				      (inexpr-class-close before)
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
	(member-init-cont      . c-lineup-multi-inher)
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
	(catch-clause          . 0)
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
	(template-args-cont    . (c-lineup-template-args +))
	(inlambda              . c-lineup-inexpr-block)
	(lambda-intro-cont     . +)
	(inexpr-statement      . +)
	(inexpr-class          . +)
	))
    (c-echo-syntactic-information-p . t)
    )
  "Style for testing.")

(defconst cc-test-javateststyle
  '("teststyle"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist . ((inline-open . 0)
			(topmost-intro-cont    . +)
			(statement-block-intro . +)
			(knr-argdecl-intro     . 5)
			(substatement-open     . +)
			(label                 . 0)
			(statement-case-open   . +)
			(statement-cont        . +)
			(arglist-intro  . c-lineup-arglist-intro-after-paren)
			(arglist-close  . c-lineup-arglist)
			(access-label   . 0)
			(inher-cont     . c-lineup-java-inher)
			(func-decl-cont . c-lineup-java-throws)
			))
    )
  "Style for testing Java code.")

(c-add-style "teststyle" cc-test-teststyle)
(c-add-style "javateststyle" cc-test-javateststyle)

(defvar cc-test-finished-tests nil)
(defvar cc-test-comp-buf nil)
(defvar cc-test-comp-win nil)

(defun cc-test-message (msg &rest args)
  (if noninteractive
      (send-string-to-terminal (concat (apply 'format msg args) "\n"))
    (apply 'message msg args)))

(defun cc-test-log (msg &rest args)
  (if cc-test-comp-buf
      (save-excursion
	(save-selected-window
	  (select-window cc-test-comp-win)
	  (insert (apply 'format msg args))
	  (recenter -1)
	  (sit-for 0)
	  (insert "\n")))
    (apply 'cc-test-message msg args)))

(defun make-test-buffers (filename)
  (let ((testbuf (get-buffer-create "*cc-test*"))
	(resultsbuf (get-buffer-create "*cc-results*"))
	(expectedbuf (get-buffer-create "*cc-expected*"))
	(resfile (concat (file-name-sans-extension filename) ".res")))
    ;; Setup the test file buffer.
    (set-buffer testbuf)
    (buffer-disable-undo testbuf)
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
	(setq style "JAVATESTSTYLE"))
       ((string-match "\\.pike$" filename) (pike-mode))
       ((string-match "\\.idl$" filename) (idl-mode))
       (t (c-mode)))
      (c-set-style style))
    (hack-local-variables)
    ;; Setup the expected analysis buffer.
    (set-buffer expectedbuf)
    (buffer-disable-undo expectedbuf)
    (erase-buffer)
    (insert-file-contents resfile)
    (text-mode)
    (goto-char (point-min))
    ;; Setup the resulting analysis buffer.
    (set-buffer resultsbuf)
    (buffer-disable-undo resultsbuf)
    (erase-buffer)
    (list testbuf resultsbuf expectedbuf)))

(defun kill-test-buffers ()
  (let (buf)
    (if (setq buf (get-buffer "*cc-test*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-results*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-expected*"))
	(kill-buffer buf))))

(defun do-one-test (filename &optional no-error)
  (interactive "fFile to test: ")
  (let ((default-directory cc-test-dir))
    (save-excursion
      (if (or (when (member filename cc-test-finished-tests)
		(cc-test-message "Skipping %s - already tested" filename)
		t)
	      (when (not (file-exists-p
			  (concat (file-name-sans-extension filename) ".res")))
		(cc-test-log "Skipping %s - no .res file" filename)
		t))
	  t
	(if noninteractive
	    (send-string-to-terminal (format "Testing %s        \r" filename))
	  (message "Testing %s" filename))
	(let* ((baw:c-testing-p t)
	       (buflist (make-test-buffers filename))
	       (testbuf (car buflist))
	       (resultsbuf (nth 1 buflist))
	       (expectedbuf (nth 2 buflist))
	       (pop-up-windows t)
	       (linenum 1)
	       (style "TESTSTYLE")
	       error-found-p
	       expectedindent
	       c-echo-syntactic-information-p)
	  (set-buffer testbuf)
	  (goto-char (point-min))
	  ;; Collect the analysis of all lines.
	  (while (not (eobp))
	    (let ((syntax
		   (condition-case err
		       (c-guess-basic-syntax)
		     (error
		      (unless error-found-p
			(setq error-found-p t)
			(cc-test-log "%s:%d: c-guess-basic-syntax error: %s"
				     filename
				     (1+ (count-lines (point-min) (point)))
				     (error-message-string err)))
		      ""))
		   ))
	      (set-buffer resultsbuf)
	      (insert (format "%s" syntax) "\n")
	      (set-buffer testbuf))
	    (forward-line 1))
	  ;; Record the expected indentation and reindent.  This is done
	  ;; in backward direction to avoid cascading errors.
	  (while (= (forward-line -1) 0)
	    (back-to-indentation)
	    (setq expectedindent (cons (current-column) expectedindent))
	    (unless (eolp)
	      ;; Do not reindent empty lines; the test cases might have
	      ;; whitespace at eol trimmed away, so that could produce
	      ;; false alarms.
	      (condition-case err
		  (c-indent-line)
		(error
		 (unless error-found-p
		   (setq error-found-p t)
		   (cc-test-log "%s:%d: c-indent-line error: %s"
				filename linenum
				(error-message-string err)))))))
	  (unless error-found-p
	    ;; Compare and report.
	    (set-buffer resultsbuf)
	    (goto-char (point-min))
	    (set-buffer expectedbuf)
	    (goto-char (point-min))
	    (set-buffer testbuf)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let* ((currentindent (progn
				      (back-to-indentation)
				      (current-column)))
		     (results (prog2
				  (set-buffer resultsbuf)
				  (buffer-substring (c-point 'bol)
						    (c-point 'eol))
				(forward-line 1)))
		     (expected (prog2
				   (set-buffer expectedbuf)
				   (buffer-substring (c-point 'bol)
						     (c-point 'eol))
				 (forward-line 1)))
		     regression-comment)
		(set-buffer testbuf)
		(unless (or (= (length results) 0)
			    (string= results expected))
		  (let ((msg (format "Expected analysis %s, got %s"
				     expected results)))
		    (cc-test-log "%s:%d: %s" filename linenum msg)
		    (indent-for-comment)
		    (unless (eolp) (end-of-line) (insert "  "))
		    (insert "!!! " msg ".")
		    (setq error-found-p t
			  regression-comment t)))
		(unless (= (car expectedindent) currentindent)
		  (let ((msg (format "Expected indentation %d, got %d"
				     (car expectedindent) currentindent)))
		    (cc-test-log "%s:%d: %s" filename linenum msg)
		    (if regression-comment
			(insert "  ")
		      (indent-for-comment)
		      (unless (eolp) (end-of-line) (insert "  "))
		      (insert "!!! "))
		    (insert msg ".")
		    (setq error-found-p t))))
	      (forward-line 1)
	      (setq expectedindent (cdr expectedindent)
		    linenum (1+ linenum))))
	  (unless error-found-p
	    (setq cc-test-finished-tests
		  (cons filename cc-test-finished-tests)))
	  (when (and error-found-p (not no-error))
	    (set-buffer testbuf)
	    (buffer-enable-undo testbuf)
	    (set-buffer-modified-p nil)
	    (set-buffer resultsbuf)
	    (buffer-enable-undo resultsbuf)
	    (set-buffer-modified-p nil)
	    (set-buffer expectedbuf)
	    (buffer-enable-undo expectedbuf)
	    (set-buffer-modified-p nil)
	    (pop-to-buffer testbuf)
	    (error "Indentation regression found in file %s" filename))
	  (not error-found-p)
	  )))))

(defun do-all-tests (&optional resetp)
  (interactive "P")
  (let ((old-c-echo-parsing-error (symbol-function 'c-echo-parsing-error))
	broken-files cc-test-comp-buf cc-test-comp-win)
    (unwind-protect
	(progn
	  (unless noninteractive
	    ;; Log to a buffer like M-x compile does.
	    (save-some-buffers)
	    (setq cc-test-comp-buf (get-buffer-create "*cc-test-log*"))
	    (save-excursion
	      (set-buffer cc-test-comp-buf)
	      (buffer-disable-undo cc-test-comp-buf)
	      (erase-buffer)
	      (buffer-enable-undo cc-test-comp-buf)
	      (set-buffer-modified-p nil)
	      (compilation-mode)
	      (setq cc-test-comp-win (display-buffer cc-test-comp-buf))
	      (set-window-start cc-test-comp-win (point-min))
	      (compilation-set-window-height cc-test-comp-win)))
	  (when (consp resetp)
	    (setq cc-test-finished-tests nil))
	  (cc-test-log "Using CC Mode version %s" c-version)
	  (fset 'c-echo-parsing-error (lambda (&optional quiet)))
	  (mapcar (lambda (test)
		    (condition-case err
			(unless (do-one-test test t)
			  (setq broken-files (cons test broken-files)))
		      (error
		       (cc-test-log "%s: Eval error: %s"
				    test (error-message-string err))
		       (setq broken-files (cons test broken-files)))
		      ))
		  (directory-files
		   cc-test-dir nil
		   "\\.\\(c\\|cc\\|java\\|pike\\|idl\\|m\\)\\'")))
      (fset 'c-echo-parsing-error old-c-echo-parsing-error))
    (if noninteractive
	(send-string-to-terminal "                              \r"))
    (kill-test-buffers)
    (when broken-files
      (cc-test-message "Broken file(s): %s"
		       (mapconcat 'identity broken-files ", ")))
    (unless noninteractive
      (if broken-files
	  (first-error)
	(message "All tests successful")
	(delete-window cc-test-comp-win)
	(kill-buffer cc-test-comp-buf)
	(setq cc-test-finished-tests nil)))))

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
    (forward-line 1)))
