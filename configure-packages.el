;; -*- lexical-binding: t -*-

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'hugo-utils)

;; C/C++ comment style
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-comment-style 1)))

(use-package clang-format
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-deabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer))

(use-package company-go
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eglot
  :ensure t
  :config
  (dolist (mode '(c-mode c++-mode))
    (add-to-list 'eglot-server-programs
                 `(,mode . ("clangd"))))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls"))))

(use-package git-gutter
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (concat init-path "keyfreq")))

(use-package satysfi
  :config
  (add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
  (add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
  (if (equal system-type 'gnu/linux)
      (setq satysfi-pdf-viewer-command "evince")))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode t))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(if (executable-find "mozc_emacs_helper")
    (use-package mozc
      :ensure t
      :config
      (setq default-input-method "japanese-mozc")
      (setq mozc-candidate-style 'echo-area)))
