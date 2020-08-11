;;; hugo-utils.el --- Utilities for Hugo             -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Koki Fukuda

;; Author: Koki Fukuda <ko.fu.dev@gmail.com>
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

(defun hugo-utils--hugo-project-p (project-path)
  (and (file-exists-p (concat project-path "config.toml"))
       (file-exists-p (concat project-path "archetypes"))
       (file-exists-p (concat project-path "content"))))

(defun hugo-utils--get-post-name (post-path)
  (file-name-sans-extension (file-name-nondirectory post-path)))

(defun hugo-insert-image ()
  "Ask for image path, and copy the image to hugo static file directory
for editing markdown file and insert markdown image representation."
  (interactive)
  (if (not (and (project-current)
                (hugo-utils--hugo-project-p (project-root (project-current)))))
      (message "Current editing file is not part of Hugo based project.")
    (letrec ((image-file (read-file-name "Image file path: "))
             (new-name (read-string "Insert as: " (file-name-nondirectory image-file)))
             (post-name (hugo-utils--get-post-name buffer-file-name))
             (project-root-dir (project-root (project-current)))
             (post-image-dir (concat project-root-dir "static/images/" post-name)))
      (make-directory post-image-dir t)
      (copy-file image-file (concat post-image-dir "/" new-name) t)
      (save-excursion
          (insert "!["
                  (read-string "Image alternative string: ")
                  "](/images/" post-name "/" new-name ")")))))

(defun hugo-utils--current-timestamp ()
  (concat (format-time-string "%FT%T")
          (with-temp-buffer
            (insert(format-time-string "%z"))
            (goto-char (point-min))
            (forward-char 3)
            (insert ":")
            (buffer-string))))

(defun hugo-update-lastmod ()
  "Update page variable \"lastmod\" to current datetime."
  (interactive)
  (if (not (and (project-current)
                (hugo-utils--hugo-project-p (project-root (project-current)))
                (string= (file-name-extension buffer-file-name) "md")))
      (message "Current buffer file is not Hugo post file.")
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (next-line)
        (search-forward "---")
        ;; Avoid editing non-property line.
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (if (re-search-forward "^lastmod:" nil t)
            (progn
              (kill-line)
              (insert " " (hugo-utils--current-timestamp)))
          (goto-char (point-min))
          (if (re-search-forward "^date:" nil t)
              (progn
                (end-of-line)
                (open-line 1)
                (next-line)
                (insert "lastmod: " (hugo-utils--current-timestamp)))
            (message "Unable to update lastmod; Is it Hugo post file?")))))))

(provide 'hugo-utils)
;;; hugo-utils.el ends here
