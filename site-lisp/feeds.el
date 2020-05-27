;; Clear out default feeds
(setq newsticker-url-list-defaults '())

(setq newsticker-url-list '())
(defun add-feed (name url)
  "Add URL as NAME to newsticker's list."
  (add-to-list 'newsticker-url-list (list name url)))

(add-feed "ArchLinux News" "https://www.archlinux.jp/feeds/news.xml")
(add-feed "NJ-TK" "https://njtk.blogspot.com/feeds/posts/default?alt=rss")
(add-feed "GIGAZINE" "https://gigazine.net/news/rss_atom/")

(makunbound 'add-feed)

(provide 'feeds)
