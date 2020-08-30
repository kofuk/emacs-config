;; -*- lexical-binding: t -*-

(let ((index-refreshed-p nil))
  (cl-flet ((install-package
             (lambda (pkg)
               (unless (package-installed-p pkg)
                 (unless index-refreshed-p
                   (package-refresh-contents)
                   (setq index-refreshed-p t))
                 (package-install pkg)))))

    (install-package 'clang-format)
    (install-package 'cmake-mode)
    (install-package 'company-go)
    (install-package 'csv-mode)
    (install-package 'eglot)
    (install-package 'git-gutter)
    (install-package 'go-mode)
    (install-package 'highlight-indent-guides)
    (install-package 'htmlize)
    (install-package 'keyfreq)
    (if (executable-find "mozc_emacs_helper")
        (install-package 'mozc))
    (install-package 'undo-tree)
    (install-package 'use-package)
    (install-package 'web-mode)
    (install-package 'yasnippet)))
