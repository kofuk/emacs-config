;;; postfix.el --- Postfix completion                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Koki Fukuda

;; Author: Koki Fukuda <ko.fu.dev@gmail.com>
;; Keywords: abbrev

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

;; Postfix completion inspired by IntelliJ IDEA, for Emacs.

;;; Code:
(defun postfix-completion ()
  "Expand template that corresponds to postfix and move cursor to position
specified in the template."
       (interactive)
       (let (old-point new-point postfix-type expr expr-end-point)
	 (setq old-point (point))
	 (search-backward "." -1 t 1)
	 (setq new-point (point))
	 (setq postfix-type
	       (buffer-substring-no-properties (+ new-point 1) old-point))
	 (setq expr-end-point new-point)
	 (back-to-indentation)
	 (setq expr (buffer-substring-no-properties (point) expr-end-point))
	 (unless (do-postfix-completion postfix-type expr)
	   (message "Cannot expand postfix.")
	   (go-to-point old-point))))

(defun go-to-point (to-point)
       (forward-char (- to-point (point))))

(defun set-comp-path ()
       (if (buffer-file-name)
	 (let (ext-start-pos extension)
	   (setq ext-start-pos (string-match "\\.[a-zA-Z0-9]*$" (buffer-file-name)))
	   (if ext-start-pos
	       (progn
		 (setq extension (substring (buffer-file-name) (+ ext-start-pos 1) nil))
		 (setq-local postfix-comp-path (cdr (assoc extension postfix-snippets-alist))))
	     (setq postfix-comp-path nil)))
	 (setq postfix-comp-path nil)))

(defun do-postfix-completion (postfix-type expr)
       (unless (boundp 'postfix-comp-path)
	 (set-comp-path))
       (if postfix-comp-path
	   (if (file-exists-p (concat postfix-comp-path "/" postfix-type))
	       (let (snippet-content snippet-line-count last-line comp-path)
		 (setq comp-path (concat postfix-comp-path "/" postfix-type))
		 (with-temp-buffer
		   (insert-file-contents comp-path)
		   (while (search-forward "%expr%" nil t)
		     (replace-match expr nil t))
		   (setq snippet-content (buffer-substring-no-properties (point-min) (point-max)))
		   (setq snippet-line-count (line-number-at-pos)))
		 (insert snippet-content)
		 (kill-line 1)
		 (setq last-line (line-number-at-pos))
		 (forward-line (- 0 snippet-line-count))
		 (while (< (line-number-at-pos) last-line)
		   (indent-for-tab-command)
		   (forward-line))
		 (search-backward "%end%" nil t)
		 (replace-match "" nil t)
		 (indent-for-tab-command)
		 t)
	     nil)
	 nil))

(setq postfix-snippets-alist '())

(provide 'postfix)
;;; postfix.el ends here
