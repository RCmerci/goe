;;; goe.el --- ease golang editing. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; Author: rcmerci <rcmerci@gmail.com>
;; Keywords: languages

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

(defvar goe-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode goe-mode
  "Minor mode for navigating and editing golang programs."
  :keymap goe-mode-map
  :group goe
  :lighter " Goe"
  )



(let ((map goe-mode-map))
  (define-key map (kbd "C-1") 'goe-describe-current-symbol)
  (define-key map (kbd "C-2") (lambda () (interactive) (goe-describe-current-func t)))
  (define-key map (kbd "C-3") 'goe-describe-current-func)
  )






(provide 'goe)
;;; goe.el ends here
