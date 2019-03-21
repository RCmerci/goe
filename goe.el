;;; goe.el --- ease golang editing. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; Author: rcmerci <rcmerci@gmail.com>
;; Keywords: languages
;; Package-Requires: ((lsp-mode "6.0") (go-mode "20180327.830") (hydra "0.14.0"))
;; Version: 0.1

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

(require 'goe-doc)
(require 'goe-movement)

(defvar goe-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode goe-mode
  "Minor mode for navigating and editing golang files."
  :keymap goe-mode-map
  :group goe
  :lighter " Goe"
  )



(let ((map goe-mode-map))
  (define-key map (kbd "C-1") 'goe-describe-current-symbol)
  (define-key map (kbd "C-2") (lambda () (interactive) (goe-describe-current-func t)))
  (define-key map (kbd "C-3") 'goe-describe-current-func)

  ;; movement
  (define-key map (kbd "[") 'goe-backward)
  (define-key map (kbd "]") 'goe-forward)
  (define-key map (kbd "{") 'goe-lbrace)
  (define-key map (kbd "}") 'goe-rbrace)
  (define-key map (kbd "n") 'goe-special-n)
  (define-key map (kbd "p") 'goe-special-p)
  (define-key map (kbd "d") 'goe-special-d)
  (define-key map (kbd "<SPC>") 'goe-space)
  (define-key map (kbd "\"") 'goe-quote)
  ;; goto specific points
  (define-key map (kbd ";") 'goe--leader-map/body)

  )


(provide 'goe)
;;; goe.el ends here
