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
;;    appended with .res by doing `M-x resfile'.  The created or
;;    changed results file will be shown in another window.  Verify
;;    that the results are what you expect, then save both files, and
;;    check them into CVS.

;; The test suite can also check the fontification done by the
;; font-lock settings.  If there is a file with the extension .face,
;; the fontification will be tested against it.  Such files are
;; generated with `M-x facefile'.

;; Some times the tests will fail without an actual regression being
;; introduced.  This might happen if, e.g. the default Java style
;; changes.  In this case, you can modify the corresponding .res file
;; instead of fixing the regression, but be VERY careful when doing
;; this.  Make sure you know this is what you want to do!
;;
;; To do this, you want to make sure the test source is indented
;; properly, then regenerate the .res and/or .face files using
;; `resfile' or `facefile', respectively.  There's also a convenience
;; function `shift-res-offsets' that shifts the positions in a .res
;; file after a certain point, which is useful if the length of a line
;; inside a test changes for some reason.

(defvar cc-test-dir nil)
(eval-and-compile
  (setq cc-test-dir
	(let ((file (if (and (boundp 'byte-compile-dest-file)
			     (stringp byte-compile-dest-file))
			byte-compile-dest-file
		      load-file-name)))
	  (if file
	      (file-name-directory file)
	    default-directory)))
  (let ((srcdir (expand-file-name (concat cc-test-dir ".."))))
    (setq load-path (cons srcdir load-path))))

(require 'compile)
(require 'cl)
(require 'font-lock)
(require 'cc-defs)

;; Make sure all used faces are unique before loading cc-fonts.  We
;; might be screwed if it's already loaded - the check for ambiguous
;; faces below will complain in that case.
(unless (c-face-name-p 'font-lock-doc-face)
  (if (c-face-name-p 'font-lock-string-face)
      (copy-face 'font-lock-string-face 'font-lock-doc-face)
    (make-face 'font-lock-doc-face)))
(unless (c-face-name-p 'font-lock-preprocessor-face)
  (cond ((c-face-name-p 'font-lock-builtin-face)
	 (copy-face 'font-lock-builtin-face 'font-lock-preprocessor-face))
	((c-face-name-p 'font-lock-reference-face)
	 (copy-face 'font-lock-reference-face 'font-lock-preprocessor-face))
	(t (make-face 'font-lock-preprocessor-face)))
  (defvar font-lock-preprocessor-face nil)
  (setq font-lock-preprocessor-face 'font-lock-preprocessor-face))
(unless (c-face-name-p 'font-lock-constant-face)
  (if (c-face-name-p 'font-lock-reference-face)
      (copy-face 'font-lock-reference-face 'font-lock-constant-face)
    (make-face 'font-lock-constant-face))
  (defvar font-lock-constant-face nil)
  (setq font-lock-constant-face 'font-lock-constant-face))

;; Define this to operate correctly with earlier versions of
;; font-lock.  The reason is that they can contain a bug that's
;; triggered when font-lock-mode is turned off by
;; `cc-test-force-font-lock-buffer' before being turned on.
(defvar font-lock-syntactic-face-function nil)

(require 'cc-mode)

;; Alist that maps the symbols used for faces in the .face files to
;; their actual names known by font-lock.
(defconst cc-test-face-alist
  `((reg . nil)
    (cmt . font-lock-comment-face)
    (str . font-lock-string-face)
    (key . font-lock-keyword-face)
    (fun . font-lock-function-name-face)
    (var . font-lock-variable-name-face)
    (typ . font-lock-type-face)
    (ref . font-lock-reference-face)
    (doc . ,c-doc-face)
    (lbl . ,c-label-face)
    (cpp . ,c-preprocessor-face)
    (err . ,c-invalid-face)))

(let ((alist cc-test-face-alist) elem face facename)
  (while alist
    (setq elem (car alist)
	  alist (cdr alist))
    (unless (eq (car elem) 'reg)
      ;; Check that we don't have a duplicate.
      (when (and (setq facename (get (setq face (cdr elem))
				     'cc-test-face-name))
		 (not (eq facename (car elem))))
	(error (concat "Ambiguous face %s - can be both %s and %s"
		       " (cc-fonts loaded too early?)")
	       (cdr elem) facename (car elem)))

      ;; In Emacs face names are resolved as variables which can point
      ;; to another face.  Make sure we don't have such indirections
      ;; when we create or check against .face files.
      (when (and (boundp face)
		 (not (eq (symbol-value face) face)))
	(copy-face (symbol-value face) face)
	(set face face))

      (put (cdr elem) 'cc-test-face-name (car elem)))))

(defvar cc-test-font-lock-init-failed nil)

(defun cc-test-force-font-lock-buffer ()
  ;; Try to forcibly font lock the current buffer, even in batch mode.
  ;; We're doing really dirty things to trick font-lock into action in
  ;; batch mode in the different emacsen.
  (let ((orig-font-lock-make-faces
	 (and (fboundp 'font-lock-make-faces)
	      (symbol-function 'font-lock-make-faces)))
	(orig-noninteractive-function
	 (and (fboundp 'noninteractive)
	      (symbol-function 'noninteractive)))
	(orig-noninteractive-variable
	 (and (boundp 'noninteractive)
	      (symbol-value 'noninteractive)))
	;; font-lock in XEmacs 19 looks at a variable named `noninteractive'.
	(noninteractive nil))
    (unwind-protect
	(progn
	  (when (and orig-noninteractive-variable orig-font-lock-make-faces)
	    ;; `font-lock-make-faces' is used in Emacs 19.34 and
	    ;; requires a window system.  Since we never actually
	    ;; display the faces we can skip it.
	    (fset 'font-lock-make-faces (lambda (&rest args))))
	  (when orig-noninteractive-function
	    ;; XEmacs (at least 21.4) calls `noninteractive' to check
	    ;; for batch mode, so we let it lie.
	    (fset 'noninteractive (lambda () nil)))
	  (font-lock-mode 1)
	  (let (;; Avoid getting some lazy fontification package that
		;; might decide that nothing should be done.
		(font-lock-fontify-buffer-function
		 'font-lock-default-fontify-buffer))
	    (font-lock-fontify-buffer)))
      (when (and noninteractive orig-font-lock-make-faces)
	(message "uncovered font-lock-make-faces")
	(fset 'font-lock-make-faces orig-font-lock-make-faces))
      (when orig-noninteractive-function
	(fset 'noninteractive orig-noninteractive-function)))))

(defconst cc-test-teststyle
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-comment-prefix-regexp       . "\\(//+\\|\\**\\)[.!|]?")
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
     . ((string                . c-lineup-dont-change)
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
	(topmost-intro-cont    . c-lineup-topmost-intro-cont)
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
	(substatement-label    . *)
	(case-label            . 0)
	(access-label          . -)
	(label                 . *)
	(do-while-closure      . 0)
	(else-clause           . 0)
	(catch-clause          . 0)
	(comment-intro         . c-lineup-comment)
	(arglist-intro         . +)
	(arglist-cont          . (c-lineup-gcc-asm-reg 0))
	(arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist))
	(arglist-close         . +)
	(stream-op             . c-lineup-streamop)
	(inclass               . +)
	(cpp-macro             . [0])
	(cpp-macro-cont        . +)
	(cpp-define-intro      . (c-lineup-cpp-define +))
	(friend                . 0)
	(objc-method-intro     . [0])
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
    (c-indent-comment-alist . nil)
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

(defun cc-test-record-faces (testbuf facebuf check-unknown-faces)
  (set-buffer testbuf)
  (let (face prev-face (pos (point)) facename lines col preceding-entry
	(emacs-strings (not (featurep 'xemacs)))
	in-string)

    (while (progn
	     (unless (eq (setq face (get-text-property pos 'face)) prev-face)
	       (setq prev-face face)

	       ;; Translate the face to the short names used in the
	       ;; .face files and check that we expect it.
	       (unless (setq facename (if face
					  (get face 'cc-test-face-name)
					'reg))
		 (if check-unknown-faces
		     (error "Unknown face %s" face)
		   (setq facename face)))

	       (catch 'record-face
		 ;; XEmacs does not highlight the quotes surrounding
		 ;; string literals as strings, while Emacs does.  The
		 ;; .face files follow the XEmacs variety since the
		 ;; Emacs behavior can be more easily converted for
		 ;; empty strings than the other way around.
		 (when emacs-strings
		   (if in-string
		       (progn
			 (backward-char)
			 (setq in-string nil))
		     (when (and (eq facename 'str)
				(looking-at "\\s\""))
		       (when (looking-at "\"\"\\|''")
			 ;; Ignore empty strings altogether.
			 (while (progn (goto-char (match-end 0))
				       (looking-at "\"\"\\|''")))
			 (throw 'record-face nil))
		       (forward-char)
		       (setq in-string t))))

		 (when (prog1 (and col (>= col (current-column)))
			 (setq col (current-column))
			 (set-buffer facebuf))
		   ;; Since we might modify positions above we need
		   ;; to deal with new face change entries that
		   ;; override earlier ones.
		   (let (pos)

		     (while (and (not (bolp))
				 (progn
				   (c-backward-sexp)
				   (setq pos (point)
					 preceding-entry (read facebuf))
				   (>= (car entry) col)))
		       (goto-char pos)
		       (setq preceding-entry nil))
		     (delete-region (point) (c-point 'eol))

		     (unless preceding-entry
		       (save-excursion
			 (c-safe
			   (c-backward-sexp)
			   (setq preceding-entry (read facebuf)))))))

		 ;; Don't add a new entry if the preceding one has the
		 ;; same face.
		 (when (and preceding-entry
			    (eq (car (cdr preceding-entry)) facename))
		   (set-buffer testbuf)
		   (throw 'record-face nil))

		 (insert (format "%s" (setq preceding-entry
					    (list col facename))))
		 (set-buffer testbuf)))

	     (setq pos (next-single-property-change (point) 'face))

	     ;; Insert the same amount of line breaks in facebuf as
	     ;; we've passed in testbuf (count-lines is clumsy here).
	     (setq lines 0)
	     (while (re-search-forward "[\n\r]" pos 'move)
	       (setq lines (1+ lines)))
	     (when (> lines 0)
	       (set-buffer facebuf)
	       (insert-char ?\n lines)
	       (set-buffer testbuf)
	       (setq col nil))

	     pos))))

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
	(resfile (concat (file-name-sans-extension filename) ".res"))
	(facefile (concat (file-name-sans-extension filename) ".face"))
	exp-syntax-buf res-syntax-buf exp-faces-buf res-faces-buf)

    ;; Setup the test file buffer.
    (set-buffer testbuf)
    (buffer-disable-undo testbuf)
    (erase-buffer)
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((c-default-style "TESTSTYLE")
	  c-mode-hook c++-mode-hook objc-mode-hook c-mode-common-hook)
      (cond
       ((string-match "\\.cc$" filename) (c++-mode))
       ((string-match "\\.m$" filename) (objc-mode))
       ((string-match "\\.java$" filename)
	(setq c-default-style "JAVATESTSTYLE")
	(java-mode))
       ((string-match "\\.pike$" filename) (pike-mode))
       ((string-match "\\.idl$" filename) (idl-mode))
       (t (c-mode))))
    (hack-local-variables)

    ;; Setup the expected and resulting analysis buffers.
    (when (file-exists-p resfile)
      (setq exp-syntax-buf (get-buffer-create "*cc-expected-syntax*"))
      (set-buffer exp-syntax-buf)
      (buffer-disable-undo exp-syntax-buf)
      (erase-buffer)
      (insert-file-contents resfile)
      (goto-char (point-min))

      (setq res-syntax-buf (get-buffer-create "*cc-resulting-syntax*"))
      (set-buffer res-syntax-buf)
      (buffer-disable-undo res-syntax-buf)
      (erase-buffer))

    ;; Setup the expected and resulting faces buffers.
    (when (file-exists-p facefile)
      (setq exp-faces-buf (get-buffer-create "*cc-expected-faces*"))
      (set-buffer exp-faces-buf)
      (buffer-disable-undo exp-faces-buf)
      (erase-buffer)
      (insert-file-contents facefile)
      (goto-char (point-min))

      (setq res-faces-buf (get-buffer-create "*cc-resulting-faces*"))
      (set-buffer res-faces-buf)
      (buffer-disable-undo res-faces-buf)
      (erase-buffer))

    (list testbuf res-syntax-buf exp-syntax-buf res-faces-buf exp-faces-buf)))

(defun kill-test-buffers ()
  (let (buf)
    (if (setq buf (get-buffer "*cc-test*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-expected-syntax*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-resulting-syntax*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-expected-faces*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-resulting-faces*"))
	(kill-buffer buf))))

(defun do-one-test (filename &optional no-error collect-tests)
  (interactive "fFile to test: ")

  (let ((default-directory cc-test-dir)
	(save-buf (current-buffer))
	(save-point (point))
	(font-lock-maximum-decoration t))

    (if (and collect-tests
	     (member filename cc-test-finished-tests))
	(progn
	  (cc-test-message "Skipping %s - already tested" filename)
	  t)

      (let* ((buflist (make-test-buffers filename))
	     (testbuf (car buflist))
	     (res-syntax-buf (nth 1 buflist))
	     (exp-syntax-buf (nth 2 buflist))
	     (res-faces-buf (nth 3 buflist))
	     (exp-faces-buf (nth 4 buflist))
	     (check-syntax exp-syntax-buf)
	     (check-faces exp-faces-buf)
	     (pop-up-windows t)
	     (linenum 1)
	     error-found-p
	     expectedindent
	     c-echo-syntactic-information-p
	     font-lock-verbose)

	(if (and (not check-syntax) (not check-faces))
	    (progn
	      (cc-test-log "Skipping %s - no .res or .face file" filename)
	      t)

	  (if noninteractive
	      (send-string-to-terminal
	       (format "Testing %s        \r" filename))
	    (message "Testing %s" filename))

	  (set-buffer testbuf)

	  (when check-syntax
	    ;; Collect the syntactic analysis of all lines.
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let ((syntax
		     (condition-case err
			 (c-guess-basic-syntax)
		       (error
			(if no-error
			    (unless error-found-p
			      (setq error-found-p t)
			      (cc-test-log
			       "%s:%d: c-guess-basic-syntax error: %s"
			       filename (1+ (count-lines (point-min) (point)))
			       (error-message-string err)))
			  (switch-to-buffer testbuf)
			  (signal (car err) (cdr err)))
			""))
		     ))
		(set-buffer res-syntax-buf)
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
		   (if no-error
		       (unless error-found-p
			 (setq error-found-p t)
			 (cc-test-log
			  "%s:%d: c-indent-line error: %s"
			  filename (1+ (count-lines (point-min)
						    (c-point 'bol)))
			  (error-message-string err)))
		     (switch-to-buffer testbuf)
		     (signal (car err) (cdr err))))))))

	  ;; Collect the face changes.
	  (when check-faces
	    (cc-test-force-font-lock-buffer)
	    (goto-char (point-min))
	    (cc-test-record-faces testbuf res-faces-buf nil))

	  (unless error-found-p
	    ;; Compare and report.
	    (when check-syntax
	      (set-buffer res-syntax-buf)
	      (goto-char (point-min))
	      (set-buffer exp-syntax-buf)
	      (goto-char (point-min)))
	    (when check-faces
	      (set-buffer res-faces-buf)
	      (goto-char (point-min))
	      (set-buffer exp-faces-buf)
	      (goto-char (point-min)))
	    (set-buffer testbuf)
	    (goto-char (point-min))

	    (catch 'break-loop
	      (while (not (eobp))
		(let* (result expected regression-comment indent-err)

		  (flet ((regression-msg (msg &rest args)
			  (setq msg (apply 'format msg args))
			  (cc-test-log "%s:%d: %s" filename linenum msg)
			  (set-buffer testbuf)
			  (unless regression-comment
			    (setq regression-comment t)
			    (indent-for-comment)
			    (when (re-search-forward
				   "\\*/" (c-point 'eol) 'move)
			      (goto-char (match-beginning 0)))
			    (delete-horizontal-space)
			    (insert " !!! "))
			  (insert msg ". ")
			  (setq error-found-p t)))

		    (when check-syntax
		      (set-buffer exp-syntax-buf)
		      (if (eobp)
			  ;; Check for premature end of the .res file here
			  ;; to avoid noise from the errors below.
			  (progn
			    (cc-test-log
			     "%s:%d: Unexpected end of .res file"
			     filename linenum)
			    (setq error-found-p t
				  check-syntax nil))

			;; Compare the syntax analysis.
			(setq result (progn
				       (set-buffer res-syntax-buf)
				       (buffer-substring (c-point 'bol)
							 (c-point 'eol)))
			      expected (progn
					 (set-buffer exp-syntax-buf)
					 (buffer-substring (c-point 'bol)
							   (c-point 'eol))))
			(unless (string= result expected)
			  (regression-msg "Expected analysis %s, got %s"
					  expected result))

			;; Compare indentation.
			(set-buffer testbuf)
			(unless (= (car expectedindent)
				   (progn (back-to-indentation)
					  (current-column)))
			  (regression-msg "Expected indentation %d, got %d"
					  (car expectedindent)
					  (current-column))
			  (setq indent-err t))

			(set-buffer res-syntax-buf)
			(forward-line 1)
			(set-buffer exp-syntax-buf)
			(forward-line 1)))

		    ;; Compare faces, but don't bother if the
		    ;; indentation is different.  Only report the
		    ;; first inconsistency on the line.
		    (when (and check-faces (not indent-err))
		      (set-buffer exp-faces-buf)
		      (if (eobp)
			  ;; Check for premature end of the .face file here
			  ;; to avoid noise from the errors below.
			  (progn
			    (cc-test-log
			     "%s:%d: Unexpected end of .face file"
			     filename linenum)
			    (setq error-found-p t
				  check-faces nil))

			(while (progn
				 (set-buffer res-faces-buf)
				 (skip-chars-forward " \t")
				 (setq result (and (not (eolp))
						   (read res-faces-buf)))
				 (set-buffer exp-faces-buf)
				 (skip-chars-forward " \t")
				 (setq expected (and (not (eolp))
						     (read exp-faces-buf)))
				 (and (or result expected)
				      (equal result expected))))

			(cond
			 ((not (or result expected)))
			 ((not result)
			  (regression-msg
			   "Expected %s face at column %d"
			   (cadr expected) (car expected)))
			 ((not expected)
			  (regression-msg
			   "Got unexpected %s face at column %d"
			   (cadr result) (car result)))
			 ((eq (car result) (car expected))
			  (if (and (featurep 'xemacs)
				   (<= emacs-major-version 20)
				   (eq (cadr result) 'doc)
				   (eq (cadr expected) 'str))
			      ;; `font-lock-fontify-syntactically-region'
			      ;; in XEmacs <= 20 contains Lisp
			      ;; specific crud that affects all modes:
			      ;; Any string at nesting level 1 is
			      ;; fontified with the doc face.  Argh!  Yuck!
			      nil
			    (regression-msg
			     "Expected %s face at column %d, got %s face"
			     (cadr expected) (car expected)
			     (cadr result))))
			 ((eq (cadr result) (cadr expected))
			  (regression-msg
			   "Expected %s face at column %d, got it at %d"
			   (cadr expected) (car expected)
			   (car result)))
			 (t
			  (regression-msg
			   (concat "Expected %s face at column %d, "
				   "got %s face at %d")
			   (cadr expected) (car expected)
			   (cadr result) (car result))))

			(set-buffer res-faces-buf)
			(forward-line 1)
			(set-buffer exp-faces-buf)
			(forward-line 1)))

		    (set-buffer testbuf)
		    (forward-line 1)
		    (setq expectedindent (cdr-safe expectedindent)
			  linenum (1+ linenum)))))

	      (when check-syntax
		(set-buffer exp-syntax-buf)
		(unless (eobp)
		  (setq error-found-p t)
		  (cc-test-log "%s:%d: Expected end of .res file"
			       filename linenum)))
	      (when check-faces
		(set-buffer exp-faces-buf)
		(unless (eobp)
		  (setq error-found-p t)
		  (cc-test-log "%s:%d: Expected end of .face file"
			       filename linenum)))))

	  (unless (or error-found-p (not collect-tests))
	    (setq cc-test-finished-tests
		  (cons filename cc-test-finished-tests)))

	  (when (and error-found-p (not no-error))
	    (set-buffer testbuf)
	    (buffer-enable-undo testbuf)
	    (set-buffer-modified-p nil)

	    (when exp-syntax-buf
	      (set-buffer res-syntax-buf)
	      (buffer-enable-undo res-syntax-buf)
	      (set-buffer-modified-p nil)
	      (set-buffer exp-syntax-buf)
	      (buffer-enable-undo exp-syntax-buf)
	      (set-buffer-modified-p nil))

	    (when exp-faces-buf
	      (set-buffer exp-faces-buf)
	      (buffer-enable-undo exp-faces-buf)
	      (set-buffer-modified-p nil))

	    (switch-to-buffer testbuf)
	    (error "Indentation regression found in file %s" filename))

	  (unless noninteractive
	    (message nil))

	  (set-buffer save-buf)
	  (goto-char save-point)
	  (not error-found-p))))))

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
	  (cc-test-log "Testing CC Mode %s in %s" c-version (emacs-version))
	  (fset 'c-echo-parsing-error (lambda (&optional quiet)))
	  (mapcar (lambda (test)
		    (condition-case err
			(unless (do-one-test test t t)
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
		       (mapconcat 'identity (reverse broken-files) ", ")))
    (unless noninteractive
      (if broken-files
	  (first-error)
	(message "All tests successful")
	(delete-window cc-test-comp-win)
	(kill-buffer cc-test-comp-buf)
	(setq cc-test-finished-tests nil)))))

(defun facefile ()
  "Creates a .face file from the test file in the current buffer.
It records the faces put into the buffer by font-lock in the test file."

  ;; Note: fast-lock cache files could perhaps be used for this, but
  ;; it's better to have a file format that we control.  It's also
  ;; more edit and cvs friendly, and it avoids some noise that we
  ;; don't want to record, e.g. fontification of whitespace.

  (interactive)

  (when (and (featurep 'xemacs)
	     (<= emacs-major-version 20))
    ;; See special case in `do-one-test'.
    (error "Won't make .face files in XEmacs <= 20 since those versions "
	   "have broken syntactic fontification"))

  (let* ((testbuf (current-buffer))
	 (facefile (concat (file-name-sans-extension buffer-file-name)
			   ".face"))
	 (facebuf (find-file-noselect facefile))
	 error errpos)

    (save-excursion
      (save-selected-window
	(condition-case err
	    (progn
	      (switch-to-buffer-other-window facebuf)
	      (set-buffer facebuf)
	      (erase-buffer)
	      (set-buffer testbuf)

	      (cc-test-force-font-lock-buffer)
	      (goto-char (point-min))
	      (cc-test-record-faces testbuf facebuf t)

	      ;; Beautify the face file a little.
	      (set-buffer facebuf)
	      (goto-char (point-min))
	      (while (search-forward ")(" nil 'move)
		(replace-match ") (" t t))
	      (unless (bolp) (insert "\n")))

	  (error
	   (setq error err)
	   (set-buffer testbuf)
	   (setq errpos (point))))))
    (when error
      (when errpos (goto-char errpos))
      (signal (car error) (cdr error)))))

(defun resfile ()
  "Creates a .res file from the test file in the current buffer.
It records the syntactic analysis of each line in the test file."

  (interactive)
  (save-excursion
    (save-selected-window
      (let* ((testbuf (current-buffer))
	     (resfile (concat (file-name-sans-extension buffer-file-name)
			      ".res"))
	     (resbuf (find-file-noselect resfile)))
	(switch-to-buffer-other-window resbuf)
	(set-buffer resbuf)
	(erase-buffer)
	(set-buffer testbuf)

	(goto-char (point-min))
	(while (not (eobp))
	  (let ((syntax (c-guess-basic-syntax)))
	    (set-buffer resbuf)
	    (insert (format "%s\n" syntax)))
	  (set-buffer testbuf)
	  (forward-line 1))))))

(defun shift-res-offsets (offset)
  ;; Shifts the offsets in the corresponding .res files by the
  ;; specified amount from the current point forward.
  (interactive "nShift offsets at or after point with: ")
  (let ((save-point (point))
	(resfile (concat (file-name-sans-extension buffer-file-name) ".res"))
	(count 0))
    (unless (file-exists-p resfile)
      (error "Cannot open result file %s" resfile))
    (find-file resfile)
    (goto-char (point-min))
    (while (re-search-forward "\\<[0-9]+\\>" nil t)
      (let ((pos (string-to-number (match-string-no-properties 0))))
	(when (>= pos save-point)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert-and-inherit (format "%d" (+ pos offset)))
	  (setq count (1+ count)))))
    (message "Shifted %d offsets" count)))
