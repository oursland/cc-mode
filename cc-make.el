;;; cc-make.el --- Makes sure globally needed packages are loaded.

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Authors:    1997 Barry A. Warsaw
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(setq cc-path-to-the-custom-library
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


(or (member default-directory load-path)
    (setq load-path (cons default-directory load-path)))

(let ((load-path (if cc-path-to-the-custom-library
		     (cons cc-path-to-the-custom-library load-path)
		   load-path)))
  (if (not (and (condition-case nil
		    (require 'custom)
		  (error nil))
		;; Stock Emacs 19.34 doesn't have this
		(fboundp 'defcustom)))
      (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required to use CC Mode 5.  You MUST fix cc-make.el.  See
that file or the CC Mode README for details.")))

;; Always get the compile time definitions
(require 'cc-defs)
(require 'cc-menus)

;; cc-mode-19.el contains compatibility macros that should be compiled
;; in if needed.
(if (or (not (fboundp 'functionp))
	(not (fboundp 'char-before))
	(not (c-safe (char-after) t))
	(not (fboundp 'when))
	(not (fboundp 'unless)))
    (require 'cc-mode-19))

(provide 'cc-make)
;;; cc-make.el ends here
