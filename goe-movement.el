;;; goe-movement.el --- movement -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; (defvar goe--identfier-regex "\\b[a-zA-Z_][a-zA-Z_0-9]*?\\b")

(defun goe--exit-string ()
  "Goto string's beginning, if in string"
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun goe--in-string ()
  "Return nil if not in string"
  (nth 3 (syntax-ppss))
  )

(defun goe--goto-current-func ()
  "Move to current function name's point,
return its point if success,
return nil when not in func call place."
  (let ((p (save-excursion
	     (goe--exit-string)
	     (let ((bnd (bounds-of-thing-at-point 'symbol)))
	       (if (and bnd
			(progn
			  (goto-char (cdr bnd))
			  (looking-at "[\t ]*?(")))
		   (car bnd)
		 (when (looking-back ")")
		   (backward-char))
		 (up-list -1)
		 (if (and (looking-at "(")
			  (looking-back "[a-zA-Z_0-9]\s*?"))
		     (progn
		       (backward-word-strictly)
		       (point))
		   nil))))))
    (when p (goto-char p))
    p))

(defun goe-different-side ()
  "Move to another side of (, [, {, \", `"
  (cond
   ((goe--in-string)
    ;; in string, do nothing
    )
   ((looking-at "[([{\"`]")
    (forward-sexp)
    )
   ((looking-back "[\])}\"`]")
    (backward-sexp))
   ))






(provide 'goe-movement)
