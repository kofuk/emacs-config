;;; matching-punct.el --- Operate against text inside matching punctuator  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Koki Fukuda

;; Author: Koki Fukuda
;; Keywords: tools

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

(defmacro matching-punct--apply-inside-matching-punct (open close boundary func)
  `(let ((start) (end) (b (if boundary 0 1)))
    (save-excursion
      (setq start (search-backward ,open  nil t)))
    (save-excursion
      (setq end (search-forward ,close nil t)))
    (unless (and start end)
      (error "Not in matching punct"))
    (,func (+ start b) (- end b))))

(defun kill-inside-paren (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "(" ")" boundary kill-region))

(defun kill-inside-bracket (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "[" "]" boundary kill-region))

(defun kill-inside-brace (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "{" "}" boundary kill-region))

(defun kill-inside-single-quote (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "'" "'" boundary kill-region))

(defun kill-inside-quote (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "\"" "\"" boundary kill-region))

(defun kill-inside-angle-bracket (boundary)
    (interactive "P")
    (matching-punct--apply-inside-matching-punct "<" ">" boundary kill-region))

(defvar matching-punct-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o (") #'kill-inside-paren)
    (define-key map (kbd "C-c C-o )") #'kill-inside-paren)
    (define-key map (kbd "C-c C-o [") #'kill-inside-single-bracket)
    (define-key map (kbd "C-c C-o ]") #'kill-inside-single-bracket)
    (define-key map (kbd "C-c C-o {") #'kill-inside-brace)
    (define-key map (kbd "C-c C-o }") #'kill-inside-brace)
    (define-key map (kbd "C-c C-o '") #'kill-inside-single-quote)
    (define-key map (kbd "C-c C-o \"") #'kill-inside-quote)
    (define-key map (kbd "C-c C-o <") #'kill-inside-angle-bracket)
    (define-key map (kbd "C-c C-o >") #'kill-inside-angle-bracket)
    map)
  "Keymap for matching-punct-mode.")

(define-minor-mode matching-punct-mode
  "Operate against matching punctuators."
  :group 'tools
  :lighter " {|}")

(define-global-minor-mode global-matching-punct-mode matching-punct-mode
  (lambda () (matching-punct-mode +1)))

(provide 'matching-punct)
;;; matching-punct.el ends here
