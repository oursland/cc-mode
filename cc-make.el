;; Version:    5.18

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
(or (require 'custom)
    (error "STOP!  Custom library not found.  You MUST fix cc-make.el"))
(batch-byte-compile)
