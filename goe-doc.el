;;; goe-doc.el --- show doc -*- lexical-binding: t; -*-

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

(require 'lsp-mode)

(defvar goe--doc-overlay nil
  "Overlay to display documents.")

(defvar goe--last-doc-overlay-point nil
  "Last point of doc overlay.")

(defun goe--lsp-request (type point cb)
  (let ((doc-id (lsp--text-document-identifier))
	(position (lsp--point-to-position point)))
    (funcall cb (lsp-send-request
	      (lsp--make-request
	       type
	       (list :textDocument doc-id :position position))))))

(defun goe--lsp-signature (point cb)
  (goe--lsp-request "textDocument/signatureHelp" point cb))

(defun goe--lsp-hover (point cb)
  (goe--lsp-request "textDocument/hover" point cb))

(defun goe--lsp-mix-request (point cb &optional prefer-hover)
  "Make textDocument/hover and textDocument/signatureHelp request.
hover result have higher priority."
  (if prefer-hover
      (or (goe--lsp-hover point cb)
	  (goe--lsp-signature point cb))
    (or (goe--lsp-signature point cb)
      (goe--lsp-hover point cb))))

(defun goe--extract-lsp-contents (info)
  "Extract infomation from lsp request's response.
request: textDocument/signatureHelp and textDocument/hover
return a plist: (:main \"maininfo\" :other \"something like documents\")"
  (let ((contents (or (gethash "contents" info)
		      (gethash "signatures" info))))
    (when contents
      (cond
       ((stringp contents) `(:main ,contents))
       ((sequencep contents)
	(let ((r '()))
	  (seq-doseq (i contents)
	    (cond
	     ((stringp i) (plist-put r :other i))
	     ((and (hash-table-p i)
		   (gethash "language" i))
	      (setq r (plist-put r :main (gethash "value" i))))
	     ((and (hash-table-p i)
		   (gethash "label" i))
	      (setq r (plist-put r :main (gethash "label" i))))))
	  r))
       ((gethash "kind" contents) `(:main ,(gethash "value" contents)))
       ((gethash "language" contents) `(:main ,(gethash "value" contents)))
       ((gethash "label" contents) `(:main ,(gethash "label" contents)))))
    ))

(defun goe--format-lsp-content (plist &optional omit-other-info)
  "Convert PLIST to string list.
when OMIT-OTHER-INFO not nil, ignore :other part of plist"
  (let ((other (plist-get plist :other))
	(main (plist-get plist :main))
	strlist)
    (when (and other (not omit-other-info))
      (dolist (i (split-string other "[\r\n]"))
	(push i strlist))
      (setq strlist (reverse strlist))
      (setq strlist
	    (reverse
	     (split-string
	      (string-trim
	       (with-temp-buffer
		 (insert (string-join strlist "\n"))
		 (let ((fill-column 70))
		   (fill-region 0 (buffer-end 1)))
		 (buffer-string))) "[\r\n]")))
      (push "" strlist))
    (when main
      (dolist (i (split-string (string-trim main) "[\r\n]" t))
	(when (not (string-empty-p i)) (push i strlist))))
    strlist))

(defun goe--clear-doc-overlay (point)
  "Clear doc overlay. Return t if need to redisplay overlay."
  (let ((last-point goe--last-doc-overlay-point))
    (when (overlayp goe--doc-overlay)
      (delete-overlay goe--doc-overlay)
      (setq goe--last-doc-overlay-point nil))
    (and point (null last-point))))

(defun goe--display-doc-overlay (point strlist)
  "Display document overlay at POINT."
  (let ((column (save-excursion
		  (goto-char point)
		  (current-column)))
	(max-length 0)
	(docpoint (save-excursion
		    (forward-line -1)
		    (end-of-line)
		    (point))))
    (dolist (i strlist)
      (when (> (length i) max-length)
	(setq max-length (length i))))
    (setq goe--doc-overlay (make-overlay docpoint (1+ docpoint)))
    (setq goe--last-doc-overlay-point point)
    (setq tmp-strlist '())
    (dolist (i strlist)
      (let ((tmpstr (make-string (- max-length (length i)) ?\s)))
    	(push (concat (make-string column ?\s)
    		      (concat i tmpstr)
    		      "\n")
    	      tmp-strlist)))
    (let ((result-str (with-temp-buffer
			(insert (string-join tmp-strlist))
			(delay-mode-hooks (markdown-mode))
			(font-lock-ensure)
			(concat "\n" (propertize (substring-no-properties (org-copy-visible (point-min) (point-max)))
						 'face '(:background "#fff3bc" :foreground "black"))))))
      (overlay-put goe--doc-overlay 'display result-str)
      (overlay-put goe--doc-overlay 'priority 9999))))

(defun goe--enough-place-p (strlist)
  (<= (length strlist) (* (/ (window-body-height) 3) 2)))

(defun goe--display-doc-other-buffer (plist)
  "Display document PLIST in other buffer."
  (save-selected-window
    (pop-to-buffer (get-buffer-create "*goe-doc*"))
    (let ((inhibit-read-only t)
	  (main (plist-get plist :main))
	  (other (plist-get plist :other)))
      (delete-region (point-min) (point-max))
      (when main
	(insert main)
	(insert "\n"))
      (when other
	(insert other))
      (goto-char (point-min))
      (help-mode))))

(defun goe-describe-current-func (&optional only-func-type)
  "Describe current function."
  (interactive)
  (save-excursion
    (let* ((current-func (goe--goto-current-func-call))
	   (redisplay (goe--clear-doc-overlay current-func)))
      (when redisplay
	(goe--lsp-mix-request current-func
			      (lambda (info)
				(when info
				  (let* ((plist (goe--extract-lsp-contents info))
					 (strlist (goe--format-lsp-content plist only-func-type)))
				    (when plist
				      (if (goe--enough-place-p strlist)
					  (goe--display-doc-overlay current-func strlist)
					(goe--display-doc-other-buffer plist))
				      t))))
			      (not only-func-type))))))



(defun goe-describe-current-symbol ()
  "Describe current symbol"
  (interactive)
  (let ((bnd (bounds-of-thing-at-point 'symbol)))
    (when (goe--clear-doc-overlay (and bnd (car bnd)))
      (goe--lsp-mix-request (car bnd)
			    (lambda (info)
			      (if info
				  (let* ((plist (goe--extract-lsp-contents info))
					 (strlist (goe--format-lsp-content plist)))
				    (when plist
				      (if (goe--enough-place-p strlist)
					  (goe--display-doc-overlay (point) strlist)
					(goe--display-doc-other-buffer plist))
				      t))))
			    t))))


(provide 'goe-doc)
;;; goe-doc.el ends here
