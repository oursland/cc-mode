(setq path-to-the-custom-library
      ;; Change the following line to the directory that contains
      ;; Per's new custom library.  Leave this as nil if you are using
      ;; XEmacs 20.3, Emacs 20.0 or beyond.
      nil
      )

(setq load-path (cons "./" load-path))
(if path-to-the-custom-library
    (setq load-path (cons path-to-the-custom-library load-path)))
(batch-byte-compile)
