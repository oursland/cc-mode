(require 'cc-mode)

(defconst test-dir "~/src/elisp/cc-mode/tests/")

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
    (c-recognize-knr-p         . nil)
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
	(friend                . 0)
	(objc-method-intro     . -1000)
	(objc-method-args-cont . c-lineup-ObjC-method-args)
	(objc-method-call-cont . c-lineup-ObjC-method-call)
	(extern-lang-open      . 0)
	(extern-lang-close     . 0)
	(inextern-lang         . +)
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
    "comments.c"
    "comments-1.java"
    "comments-2.c"
    "decls-1.java"
    "decls-2.java"
    "decls-3.java"
    "enum-1.cc"
    "except-1.cc"
    "except-2.cc"
    "except-3.cc"
    "externs-1.cc"
    "externs-2.cc"
    "funcs-1.cc"
    "label-1.c"
    "if-1.cc"
    "if-2.cc"
    "if-3.cc"
    "if-4.cc"
    "if-5.cc"
    "inher-1.cc"
    "inher-2.cc"
    "inher-3.cc"
    "interface-1.m"
    "member-1.cc"
    "member-2.cc"
    "nested-1.cc"
    "nested-2.c"
    "statement-1.cc"
    "statement-2.cc"
    "statement-3.cc"
    "statement-4.cc"
    "statement-5.cc"
    "statement-6.cc"
    "statement-7.c"
    "statement-8.c"
    "statement-9.c"
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
    "top-1.cc"
    "top-2.cc"
    "top-3.cc"
    "typedef-1.c"
    ))

(defvar finished-tests nil)

(defsubst file-name-with-extension (filename extension)
  (save-match-data
    (if (string-match "\\([^.]+\\)\\.[^.]*$" filename)
	(concat (substring filename (match-beginning 1) (match-end 1))
		extension))))

(defun make-test-buffers (filename)
  (let ((testbuf (get-buffer-create "*cc-test*"))
	(resultsbuf (get-buffer-create "*cc-results*"))
	(expectedbuf (get-buffer-create "*cc-expected*"))
	(resfile (file-name-with-extension filename ".res")))
    (set-buffer testbuf)
    (erase-buffer)
    (insert-file filename)
    (beginning-of-buffer)
    (let ((style "TESTSTYLE")
	  c-mode-hook c++-mode-hook objc-mode-hook c-mode-common-hook)
      (cond
       ((string-match "\\.cc$" filename) (c++-mode))
       ((string-match "\\.m$" filename) (objc-mode))
       ((string-match "\\.java$" filename)
	(java-mode)
	(setq style "java"))
       (t (c-mode)))
      (c-set-style style))
    (set-buffer expectedbuf)
    (erase-buffer)
    (insert-file resfile)
    (text-mode)
    (beginning-of-buffer)
    (set-buffer resultsbuf)
    (erase-buffer)
    (list testbuf resultsbuf expectedbuf)))

(defun do-one-test (filename)
  (interactive "fFile to test: ")
  (if (member filename finished-tests)
      nil
    (message "Testing %s..." filename)
    (let* ((filename (concat test-dir filename))
	   (baw:c-testing-p t)
	   (buflist (make-test-buffers filename))
	   (testbuf (car buflist))
	   (resultsbuf (nth 1 buflist))
	   (expectedbuf (nth 2 buflist))
	   (pop-up-windows t)
	   (linenum 1)
	   (style "TESTSTYLE")
	   error-found-p)
      (set-buffer testbuf)
      (beginning-of-buffer)
      (while (not (eobp))
	(let ((syntax (c-guess-basic-syntax)))
	  (set-buffer resultsbuf)
	  (insert (format "%s" syntax) "\n")
	  (set-buffer testbuf))
	(forward-line 1))
      (set-buffer resultsbuf)
      (beginning-of-buffer)
      (set-buffer expectedbuf)
      (beginning-of-buffer)
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
       (t (c-mode)))
      (c-set-style style)
      (indent-region (point-min) (point-max) nil)
      (and (buffer-modified-p)
	   (setq error-found-p t))
      (if error-found-p
	  (progn
	    (pop-to-buffer testbuf)
	    (error "Regression found in file: %s!" filename))))
    (setq finished-tests (cons filename finished-tests))
    (message "Testing %s... done." filename)))

(defun do-all-tests (resetp)
  (interactive "P")
  (if (consp resetp)
      (setq finished-tests nil))
  (mapcar 'do-one-test list-of-tests)
  (message "All tests passed!")
  (setq finished-tests nil))

(defun resfile ()
  (interactive)
  (beginning-of-buffer)
  (other-window 1)
  (beginning-of-buffer)
  (other-window 1)
  (while (not (eobp))
    (let ((syntax (c-guess-basic-syntax)))
      (other-window 1)
      (insert (format "%s\n" syntax)))
    (other-window 1)
    (forward-line 1))
  (delete-backward-char 1))
