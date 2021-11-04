;;; source-line.el --- Put line number info for selected region to kilring  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Koki Fukuda

;; Author: Koki Fukuda <kofu@carbon>
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

;; `source-line' puts line number to kill ring along with editing filename for useing in
;; (for example) markdown docs.
;;
;; Expression put in kill ring depends on states:
;; - If there's region, range of selected area will be put.
;;   For example, "init.el#L1-L3"
;; - If there's no region, line number that cursor located will be put.
;;   For exmple, "init.el#L1"

;;; Code:

;;;###autoload
(defun source-line ()
  "Put line number string for current point or region to kill-ring in
filename#L1-L2 form."
  (interactive)
  (let ((file (file-name-nondirectory (if (buffer-file-name) (buffer-file-name) ""))))
    (kill-new
     (if (use-region-p)
         (let ((first (line-number-at-pos (region-beginning)))
               (last (line-number-at-pos (region-end))))
           (format "%s#L%d-L%d" file first last))
       (format "%s#L%d" file (line-number-at-pos (point))))))
  (deactivate-mark))

(provide 'source-line)
;;; source-line.el ends here
