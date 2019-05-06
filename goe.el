;;; goe.el --- ease golang editing. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; Author: rcmerci <rcmerci@gmail.com>
;; Keywords: languages
;; Package-Requires: ((lsp-mode "6.0") (go-mode "20180327.830") (hydra "0.14.0") (paredit "20171127.205"))
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
(require 'goe-imports)
(require 'paredit)

(defvar goe-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode goe-mode
  "Minor mode for navigating and editing golang files."
  :keymap goe-mode-map
  :group goe
  :lighter " Goe"
  )

(defhydra goe--leader-map (:hint nil :exit t)
  "
_f_: goto function name
_r_: goto return values
_a_: goto arguments
_d_: goto docstring
_i_: goto imports
_m_: goto method receiver
_S_: remove unused imports and sort
_I_: add imports
"
  ("f" go-goto-function-name)
  ("r" go-goto-return-values)
  ("a" go-goto-arguments)
  ("d" go-goto-docstring)
  ("i" go-goto-imports)
  ("m" go-goto-method-receiver)
  ("S" goe-sort-and-remove-unused-imports)
  ("I" goe-add-imports)
  (";" #'self-insert-command)
  )

(defhydra goe--barf-sexp ()
  "barf sexp"
  ("<" paredit-forward-barf-sexp)
  (">" paredit-forward-slurp-sexp)
  ("<RET>" (insert "<")))

(defhydra goe--slurp-sexp ()
  "slurp sexp"
  ("<" paredit-forward-barf-sexp)
  (">" paredit-forward-slurp-sexp)
  ("<RET>" (insert ">")))

(let ((map goe-mode-map))
  (define-key map (kbd "C-1") 'goe-describe-current-symbol)
  (define-key map (kbd "C-2") (lambda () (interactive) (goe-describe-current-func t)))
  (define-key map (kbd "C-3") 'goe-describe-current-func)

  ;; movement
  (define-key map (kbd "[") 'goe-backward)
  (define-key map (kbd "]") 'goe-forward)
  (define-key map (kbd "{") 'goe-lbrace)
  (define-key map (kbd "}") 'goe-rbrace)
  (define-key map (kbd "d") 'goe-special-d)
  (define-key map (kbd "<SPC>") 'goe-space)
  (define-key map (kbd "\"") 'goe-quote)
  (define-key map (kbd "(") 'goe-lparenthesis)
  (define-key map (kbd ")") 'goe-rparenthesis)
  (define-key map (kbd "DEL") 'goe-delete-backward)
  ;; goto specific points
  (define-key map (kbd ";") 'goe--leader-map/body)
  (define-key map (kbd "<") 'goe--barf-sexp/body)
  (define-key map (kbd ">") 'goe--slurp-sexp/body))


(provide 'goe)
;;; goe.el ends here
