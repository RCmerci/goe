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

(require 'hydra)

(defun goe--exit-string (&optional arg)
  "Goto string's beginning, if in string.
If ARG non nil, goto its ending."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s))
      (when arg (forward-sexp)))))

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
		 (let ((old (point)))
		   (loop
		    (up-list -1)
		    (when (or (looking-at "(") (= (point) old)) (return))
		    (setq old (point))))
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


(defun goe--at-special-point-p ()
  "Return non nil when at special points.
- (, ), {, }, [, ]
- golang keywords
- begining of line
Except all above conditions, also need to be not in string"
  (let ((thing (thing-at-point 'symbol)))
    (and
     (not (goe--in-string))
     (or
      (= 0 (current-column))
      (and thing (member thing go-mode-keywords))
      (looking-at "[([{]")
      (looking-back "[\]})]")))))

(defmacro goe--defun-special (name arglist &optional docstring &rest body)
  "Define function NAME, when current at special point,
run BODY, else just run `self-insert-command'."
  `(defun ,name ,arglist ,docstring
	  (interactive)
	  (if (goe--at-special-point-p)
	      (progn
		,@body)
	    (self-insert-command 1))))

(goe--defun-special goe-special-d ()
		    "When at special point, goto different side of current point.
Otherwise, just insert char 'd'"
		    (goe-different-side))

(goe--defun-special goe-special-n ()
		    "When at special point, move forward list.
Otherwise, just insert char 'n'"
		    (forward-list))

(goe--defun-special goe-special-p ()
		    "When at special point, move backward list.
Otherwise, just insert char 'p'"
		    (backward-list))

(goe--defun-special goe-special-f ()
		    "When at special point, move forward sexp.
Otherwise, just insert char 'f'"
		    (forward-sexp))

(goe--defun-special goe-special-b ()
		    "When at special point, move backward sexp.
Otherwise, just insert char 'f'"
		    (backward-sexp))

(defun goe-backward ()
  "Backward list.
When couldn't move backward list anymore,
move up list backward."
  (interactive)
  (if (goe--in-string)
      (goe--exit-string)
    (let ((origin (point)))
      (ignore-errors (forward-list -1))
      (when (= origin (point))
	(up-list -1)))))

(defun goe-forward ()
  "Forward list.
When couldn't move forward list anymore,
move up list forward."
  (interactive)
  (if (goe--in-string)
      (goe--exit-string t)
    (let ((origin (point)))
      (ignore-errors (forward-list))
      (when (= origin (point))
	(up-list)))))

(defun goe-lbrace ()
  "Insert brace pair."
  (interactive)
  (let ((beg (point)))
    (insert (with-temp-buffer
	      (delay-mode-hooks (go-mode))
	      (insert "{\n\t\n}")
	      (buffer-string)))
    (indent-region-line-by-line beg (point))
    (search-backward "\n")))

(defun goe-rbrace ()
  "Insert square brackets."
  (interactive)
  (insert "[]")
  (backward-char))


(defhydra goe--leader-map (:hint nil :exit t)
  "
_f_: goto function name
_r_: goto return values
_a_: goto arguments
_d_: goto docstring
_i_: goto imports
_m_: goto method receiver
"
  ("f" go-goto-function-name)
  ("r" go-goto-return-values)
  ("a" go-goto-arguments)
  ("d" go-goto-docstring)
  ("i" go-goto-imports)
  ("m" go-goto-method-receiver)
  )

(provide 'goe-movement)
