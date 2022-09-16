;;; buffile.el --- Utilities for handling buffer and file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Koki Fukuda

;; Author: Koki Fukuda
;; Keywords: convenience

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

(defun rename-this-file (new-name)
  "Rename visiting file."
  (interactive "FNew name: ")
  (if buffer-file-name
      (let ((modified (buffer-modified-p)))
        (rename-file buffer-file-name new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p modified))
    (error "Cannot rename non-file buffer")))

(defun delete-this-file ()
  "Delete visiting file along with the buffer."
  (interactive)
  (if (yes-or-no-p "Are you sure to delete this file? ")
      (if buffer-file-name
          (progn
            (delete-file buffer-file-name)
            (kill-buffer))
        (error "Cannot delete non-file buffer"))
    (message "Abort")))

(provide 'buffile)
;;; buffile.el ends here
