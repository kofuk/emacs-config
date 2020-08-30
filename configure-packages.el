;; -*- lexical-binding: t -*-

(require 'hugo-utils)

;; C/C++ comment style
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-comment-style 1)))

;; Company
(use-package company
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

(use-package eglot
  :config
  (dolist (mode '(c-mode c++-mode))
    (add-to-list 'eglot-server-programs
                 `(,mode . ("clangd"))))
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook 'eglot-ensure)))

(use-package keyfreq
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
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(if (package-installed-p 'mozc)
    (use-package mozc
      :config
      (setq default-input-method "japanese-mozc")
      (setq mozc-candidate-style 'echo-area)))
