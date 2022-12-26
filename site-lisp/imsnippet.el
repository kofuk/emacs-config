;;; imsnippet.el --- Immediate snippet               -*- lexical-binding: t; -*-

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

(defun imsnippet-region (count)
  "Evaluate current region as imsnippet."
  (interactive "p")
  (unless (use-region-p)
    (error "Region inactive"))
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (deactivate-mark)
    (dotimes (i count)
      (let ((region-name) (new-text)
            (variables (make-hash-table :test 'equal))
            (overlay (make-overlay (point) (point))))
        (unwind-protect
            (insert
             (with-temp-buffer
               (insert text)
               (goto-char (point-min))
               (while (search-forward-regexp "{{[ \n\r\t\v]*\\(?1:\\w*\\)[ \n\r\t\v]*}}" nil t nil)
                 (setq region-name (match-string 1))
                 (replace-match "")
                 (overlay-put overlay 'after-string (buffer-string))
                 (when (= (length region-name) 0)
                   (setq region-name (int-to-string i)))
                 (setq new-text (gethash region-name variables))
                 (unless new-text
                   (setq new-text (read-string (format "%s: " region-name)))
                   (puthash region-name new-text variables))
                 (insert new-text))
               (buffer-string)))
          (delete-overlay overlay))))))

(provide 'imsnippet)
;;; imsnippet.el ends here
