;;; hugo-utils.el --- Utilities for Hugo             -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Koki Fukuda

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

(defun hugo-utils--make-sure-post-is-dir (post-path)
  (unless (string= (file-name-nondirectory post-path) "index.md")
    (let ((new-dir (file-name-sans-extension  post-path))
          (cur-point (point)))
      (make-directory new-dir)
      (save-buffer)
      (rename-file post-path (concat new-dir "/index.md"))
      (find-alternate-file (concat new-dir "/index.md"))
      (goto-char cur-point))))

(defun hugo-insert-image (image-file new-name alt-string move)
  "Ask for image path, and copy the image to hugo static file directory
for editing markdown file and insert markdown image representation."
  (interactive "fImage file path: \nsInsert as: \nsAlternative string: \nP")
  (if (not (and (project-current)
                (hugo-utils--hugo-project-p (project-root (project-current)))))
      (message "Current editing file is not part of Hugo based project.")
    (hugo-utils--make-sure-post-is-dir buffer-file-name)
    (let ((post-image-dir (file-name-directory buffer-file-name)))
      (if move
          (rename-file image-file (concat post-image-dir "/" new-name))
        (copy-file image-file (concat post-image-dir "/" new-name) t))
      (insert (format "![%s](%s)" alt-string new-name)))))

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
        (forward-line)
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
                (forward-line)
                (insert "lastmod: " (hugo-utils--current-timestamp)))
            (message "Unable to update lastmod; Is it Hugo post file?")))))))

(defun hugo-utils--insert-with-id-at-point (shortcode id)
  "Insert specified SHORTCODE with argument ID"
  (if (not (and (project-current)
                (hugo-utils--hugo-project-p (project-root (project-current)))
                (string= (file-name-extension buffer-file-name) "md")))
      (message "Current buffer file is not Hugo post.")
    (insert (format "{{< %s %s >}}" shortcode id))))

(defun hugo-embed-tweet (tweet-id)
  "Insert Tweet to the article."
  (interactive "sTweed ID: ")
  (hugo-utils--insert-with-id-at-point "tweet" tweet-id))

(defun hugo-embed-youtube (video-id)
  "Insert YouTube video to the article."
  (interactive "sVideo ID: ")
  (hugo-utils--insert-with-id-at-point "youtube" video-id))

(defun hugo-new (post-name)
  (interactive "sPost name: ")
  (if (not (string-match "^[-a-zA-Z0-9]+/[-a-zA-Z0-9]+\\.md$" post-name))
      (message "Malformed post-name.")
    (let ((project-root
           (if (and
                (project-current)
                (hugo-utils--hugo-project-p (project-root (project-current))))
               (project-root (project-current))
             (read-directory-name "Hugo project path: ")))
          (post-cat-name (split-string post-name "/")))
      (if (file-exists-p (concat project-root "/content/" post-name))
          (message "Post already exists.")
        (make-directory (concat project-root "/content/" (nth 0 post-cat-name)) t)
        ;; Setup temp hugo project
        (make-directory "/tmp/hugo-utils-tmp/archetypes" t)
        (make-directory "/tmp/hugo-utils-tmp/content" t)
        (with-temp-buffer
          (write-file "/tmp/hugo-utils-tmp/config.toml"))
        (copy-file
         (concat project-root "/archetypes/default.md")
         "/tmp/hugo-utils-tmp/archetypes/" t)
        (let ((default-directory "/tmp/hugo-utils-tmp/"))
          (call-process "hugo" nil nil nil "new" post-name))
        (copy-file
         (concat "/tmp/hugo-utils-tmp/content/" post-name)
         (concat project-root "/content/" post-name) t)
        (delete-directory "/tmp/hugo-utils-tmp" t)
        (find-file (concat project-root "/content/" post-name))))))

(provide 'hugo-utils)
;;; hugo-utils.el ends here
