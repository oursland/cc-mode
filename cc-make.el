;; Version:    5.17

(setq path-to-the-custom-library
      ;; In Emacs 19.34, change the following line to the directory
      ;; that contains Per's new custom library, which you should have
      ;; already downloaded.
      nil
      )

(setq load-path (cons "./" load-path))
(if path-to-the-custom-library
    (setq load-path (cons path-to-the-custom-library load-path)))
(batch-byte-compile)
