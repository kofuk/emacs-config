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

;; This is a minor mode to operate region which surrounded by matching punctuators,
;; inspired by Vim's concept of text objects.

;; To activate this mode, type M-x matching-punct-mode.
;; If you want to activate it globally, put this in your init.el:
;;   (global-matching-punct-mode 1)

;; Usage (vertical line indicates cursor):
;; 1. Delete inside parenthesis
;;   text: foo (|bar) baz
;;   C-c C-o (
;;   text: foo (|) baz
;; 2. Delete inside parenthesis including boundary
;;   text: foo (|bar) baz
;;   C-c C-o (
;;   text: foo | baz

;; This package supports the following punctuators:
;; - ()
;; - []
;; - {}
;; - ''
;; - ""
;; - <>

;; If you want to define original punctuators, you can easily do this using
;; `matching-punct--apply-inside-matching-punct' macro.
;; For example, if you apply `kill-region' to text surrounded by "a" and "b",
;; you can use the function as follows:
;;   (matching-punct--apply-inside-matching-punct "a" "b" boundary kill-region)

;;; Code:

(defmacro matching-punct--apply-inside-matching-punct (open close boundary func)
  `(let ((orig-pt (point)) (level 0) (required-level) (start) (end) (b (if boundary 0 1)) (end-found))
     (save-excursion
       (goto-char (point-min))
       (while (and (< (point) (point-max)) (not end-found))
         ,@(if (= open close)
               `((when (and (= ,open (char-after)))
                   (cond
                    ((not start)
                     (setq start (point)))
                    ((not end)
                     (setq end (1+ (point))))
                    (t (setq start end
                             end (1+ (point)))))
                   (when (and start end (or (< orig-pt start)
                                            (and (<= start orig-pt) (< orig-pt end))))
                     (setq end-found t))))
             `((cond
                ((= ,open (char-after))
                 (setq level (1+ level))
                 (unless end
                   (setq start (point))
                   (when (and required-level (<= required-level 0))
                     (setq required-level level
                           end -1))))
                ((= ,close (char-after))
                 (when end
                   (setq end (1+ (point)))
                   (when (= level required-level)
                     (setq end-found t)))
                 (unless end-found (setq level (1- level)))))
               (when (= (point) orig-pt)
                 (setq required-level level)
                 (when start
                   (setq end -1)))))
         (unless end-found (forward-char))))
     (when (and end-found ,(if (= open close) t '(= level required-level)))
       (,func (+ start b) (- end b))
       (goto-char (+ start b)))))

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
