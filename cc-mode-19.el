;;; cc-mode-19.el --- compatibility library for Emacs and XEmacs 19

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Authors:    1997 Barry A. Warsaw
;; Maintainer: cc-mode-help@python.org
;; Created:    03-Jul-1997
;; Version:    5.15
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; This file is necessary in order to run CC Mode 5 under Emacs 19.34
;; Do *not* load this file if you are using XEmacs 19.15, Emacs 20 or
;; XEmacs 20!

;; To conditionally load this only in Emacs 19.34, add the following
;; to your .emacs file, *before* you load CC Mode.
;;
;; (or (fboundp 'functionp)
;;     (require 'cc-mode-19))

;;; Code:


(require 'advice)

;; Emacs 19.34 requires the POS argument to char-after.  Emacs 20
;; makes it optional, as it has long been in XEmacs.
(defadvice char-after (before c-char-after-advice (&optional pos) activate)
  (if (not pos)
      (setq pos (point))))

;; Emacs 19.34 doesn't have a char-before function.  Here's it's Emacs
;; 20 definition.
(defsubst char-before (&optional pos)
  (if (not pos)
      (setq pos (point)))
  (char-after (1- pos)))

;; Emacs 19.34 doesn't have a functionp function.  Here's it's Emacs
;; 20 definition.
(defun functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond
   ((symbolp obj) (fboundp obj))
   ((subrp obj))
   ((compiled-function-p obj))
   ((consp obj)
    (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
   (t nil)))


(provide 'cc-mode-19)
;;; cc-mode-19.el ends here
