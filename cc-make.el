;; Version:    See cc-mode.el

(setq path-to-the-custom-library
      ;; In Emacs 19.34, change the following line to the directory
      ;; that contains Per Abrahamsen's new Custom library, which you
      ;; must download first.  You can get Custom from:
      ;;
      ;; <http://www.dina.kvl.dk/~abraham/custom/>
      ;;
      ;; See CC Mode's README file for details (also available at
      ;; <ftp://ftp.python.org/pub/emacs/cc-mode/README>)
      nil
      )

(setq load-path (cons "./" load-path))
(if path-to-the-custom-library
    (setq load-path (cons path-to-the-custom-library load-path)))
(if (and (condition-case nil
	     (require 'custom)
	   (error nil))
	 ;; Stock Emacs 19.34 doesn't have this
	 (fboundp 'defcustom))
    (batch-byte-compile)
  (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required to use CC Mode 5.  You MUST fix cc-make.el.  See
that file or the CC Mode README for details."))
