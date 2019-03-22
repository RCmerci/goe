;;; goe-imports.el --- manage golang imports -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; Author: rcmerci <rcmerci@gmail.com>

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

;; (defcustom goe-imports-order 'sto
;;   "Order of import declarations."
;;   :type '(choice
;; 	  (const :tag "stdlib, third-party lib, own lib" sto)
;; 	  (const :tag "stdlib, own lib, third-party lib" sot)
;; 	  (const :tag "stdlib, others" so)
;; 	  (const :tag "none" none))
;;   :group 'goe)

(defvar-local goe-ownlib-prefix nil "Own lib path prefix")

(defvar goe--stdlib-cache nil)

(defun goe--stdlib-list ()
  (unless goe--stdlib-cache
    (let ((output (shell-command-to-string "go list -f '{{.Standard}} {{.ImportPath}}' all")))
      (setq goe--stdlib-cache (make-hash-table :test 'equal))
      (dolist (pkg (split-string output "\n"))
	(let ((e (split-string pkg " ")))
	  (when (equal (car e) "true")
	    (puthash (cadr e) t goe--stdlib-cache))))))
  goe--stdlib-cache)

(defun goe--unused-imports-lines ()
  (reverse (remove nil
                   (mapcar
                    (lambda (line)
                      (when (string-match "^\\(.+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): imported and not used: \".+\".*$" line)
                        (let ((error-file-name (match-string 1 line))
                              (error-line-num (match-string 2 line)))
                          (if (string= (file-truename error-file-name) (file-truename buffer-file-name))
                              (string-to-number error-line-num)))))
                    (split-string (shell-command-to-string
                                   (concat go-command
                                           (if (string-match "_test\\.go$" buffer-file-truename)
                                               " test -c"
                                             (concat " build -o " null-device))
                                           " -gcflags=-e"
                                           " "
                                           (shell-quote-argument (file-truename buffer-file-name)))) "\n")))))


(defun goe--imports ()
  "Extract imports as list.
e.g.
((\"as \"pkg1\"\" . \"pkg1\") (\"as2 \"pkg2\"\" . \"pkg2\"))
"
  (save-excursion
    (goto-char 0)
    (mapcar
     (lambda (s) (propertize (string-trim (or (cadr (split-string s)) s) "\"" "\"") 'content s))
     (cond ((search-forward-regexp "^\\<import\\>[[:space:]]*(\\(\\(.\\|\n\\)*?\\))" nil t)
	    (remove "" (mapcar 'string-trim
			       (split-string (match-string-no-properties 1) "\n" t))))
	   ((search-forward-regexp "^\\<import\\>[[:space:]]*\"\\(.+\\)\"" nil t)
	    `(,(match-string-no-properties 1)))))))

(defun goe--delete-import-decl ()
  "Delete import declarations."
  (goto-char 0)
  (let ((beg-end (cond ((search-forward-regexp "^\\<import\\>[[:space:]]*(\\(\\(.\\|\n\\)*?\\))" nil t)
			`(,(match-beginning 0) . ,(match-end 0)))
		       ((search-forward-regexp "^\\<import\\>[[:space:]]*\"\\(.+\\)\"" nil t)
			`(,(match-beginning 0) . ,(match-end 0))))))
    (delete-region (car beg-end) (cdr beg-end))))


(defun goe--classify-imports (order imports)
  "Classify imports according to ORDER."
  (let* ((stdlib (seq-filter (lambda (e) (gethash e (goe--stdlib-list))) imports))
	 (not-stdlib (seq-difference imports stdlib))
	 (third-party (if goe-ownlib-prefix (seq-filter
					     (lambda (e)
					       (not (string-prefix-p goe-ownlib-prefix e)))
					     not-stdlib)
			not-stdlib))
	 (own-lib (when goe-ownlib-prefix (seq-filter (lambda (e) (string-prefix-p goe-ownlib-prefix e)) not-stdlib))))

    (cond
     ((eq 'none order)  `(,imports))
     ((eq 'sto order) `(,stdlib ,third-party ,own-lib))
     ((eq 'sot order) `(,stdlib ,own-lib ,third-party))
     ((eq 'so order) `(,stdlib ,not-stdlib)))))

(defun goe-sort-and-remove-unused-imports (order)
  "Sort all import declarations. And remove unused ones.
If ARG is non-nil, unused imports will be commented, otherwise, will be deleted."
  (interactive (list
		(let ((order 'none))
		  (ivy-read "order of imports: " `(,(propertize "none" 'order 'none)
						   ,(propertize "stdlib, third-party, own-lib" 'order 'sto)
						   ,(propertize "stdlib, own-lib, third-party" 'order 'sot)
						   ,(propertize "stdlib, others" 'order 'so))
			    :require-match t
			    :action (lambda (e) (setq order (get-text-property 0 'order e))))
		  order)))
  (or (eq t (gofmt)) (error "gofmt failed"))
  (save-buffer)
  (save-excursion
    (dolist (line (goe--unused-imports-lines))
      (goto-line line)
      (kill-whole-line 0))


    (let ((sorted-imports (goe--classify-imports order (goe--imports)))
	  result-imports-str)
      (dolist (part sorted-imports)
	(push (string-join (mapcar (lambda (s) (get-text-property 0 'content s)) part) "\n") result-imports-str))
      (setq result-imports-str (reverse result-imports-str))
      (setq result-imports-str (concat "import (" (string-join result-imports-str "\n\n") ")"))
      (message "XXXX:%S, %S" sorted-imports result-imports-str)
      (goe--delete-import-decl)
      (insert result-imports-str))
    (gofmt)))

(provide 'goe-imports)
;;; goe-imports.el ends here
