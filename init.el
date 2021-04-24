;;; -*- lexical-binding: t -*-

;; For Emacs 27.1+, we don't have to call package-initialize implicitly,
;; so call it only if running on older Emacs.
(if (version< emacs-version "27.1")
    (progn
      (package-initialize)
      (load (locate-user-emacs-file "early-init.el"))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(dolist (path '("site-lisp" "site-lisp-local" "third_party"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;;; Package configurations

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") #'company-show-doc-buffer))

(use-package company-go
  :ensure t)

(use-package csharp-mode
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
  (if (executable-find "rust-analyzer")
      (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))))

(use-package git-gutter
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t))

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . (lambda () (require 'hugo-utils))))

(use-package sass-mode
  :ensure t)

(use-package satysfi
  :config
  (add-to-list 'auto-mode-alist '("\\.saty\\'" . satysfi-mode))
  (add-to-list 'auto-mode-alist '("\\.satyh\\'" . satysfi-mode))
  (if (equal system-type 'gnu/linux)
      (setq satysfi-pdf-viewer-command "evince")))

(use-package rust-mode
  :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (defface my-telephone-line-accent-active
    '((t (:foreground "white" :background "gray30"))) "")
  (defface my-telephone-line-buf-name-active
    '((t (:foreground "white" :background "dark cyan" :weight bold))) "")
  (setq telephone-line-height 20)
  (setq telephone-line-faces
        '((accent . (my-telephone-line-accent-active . telephone-line-accent-inactive))
          (nil . (mode-line . mode-line-inactive))
          (buf-name . (my-telephone-line-buf-name-active . telephone-line-accent-inactive))))
  (telephone-line-defsegment my-coding-system-segment ()
    (if buffer-file-coding-system
        (prin1-to-string buffer-file-coding-system)
      ""))
  (telephone-line-defsegment my-input-method-segment ()
    (if current-input-method "あ" "A"))
  (setq telephone-line-lhs
        '((buf-name . (telephone-line-buffer-name-segment
                       telephone-line-buffer-modified-segment))
          (nil . (telephone-line-simple-major-mode-segment))
          (accent . (my-coding-system-segment))))
  (setq telephone-line-rhs
        '((accent . (telephone-line-vc-segment))
          (nil . (telephone-line-misc-info-segment))
          (accent . (telephone-line-airline-position-segment))
          (nil . (my-input-method-segment))))
  (setq telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-secondary-right-separator 'telephone-line-nil)
  (telephone-line-mode t))

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

(if (executable-find "mozc_emacs_helper")
    (use-package mozc
      :ensure t
      :config
      (setq default-input-method "japanese-mozc")
      (setq mozc-candidate-style 'echo-area)))

;;; Other configurations

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)
(setq font-lock-maximum-decornation t)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Mode line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(display-battery-mode 1)

(setq transient-mark-mode t)
(set-cursor-color "white")

(setq-default indicate-empty-lines t)

;; GDB
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gud-tooltip-echo-area t)

;; Rectangle region
;;  I don't have to enable cua-mode only to ues cus's rectangle region functionality,
;;  but it's required to edit keymaps for rectangle region
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-x SPC") #'cua-set-rectangle-mark)
(define-key cua--rectangle-keymap (kbd "C-x C-x") #'cua-rotate-rectangle)

;; Show line number in the left
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; Start up screen
(setq inhibit-startup-screen t)

;; Save cursor position
(if (fboundp 'save-place-mode) (save-place-mode 1) (setq-default save-place t))

;; Follow symbolic links to versioned files
(setq vc-follow-symlinks t)

;; Backup file
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups"))))
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Show whitespaces
(setq whitespace-style '(face trailing tabs tab-mark))
(setq-default indent-tabs-mode nil)
(global-whitespace-mode 1)

(setq read-file-name-completion-ignore-case t)

;; Disable annoying key
(global-set-key "\C-v" nil)

(defun revert-buffer-noconfirm ()
  "Reverts buffer data from assciated file, without any prompt"
  (interactive)
  (revert-buffer 1 1 1))

(global-set-key (kbd "<f5>") #'revert-buffer-noconfirm)

;; Make Emacs to put '\n' at the end of file
(setq require-final-newline t)

;; Auto insert
(auto-insert-mode 1)
(setq auto-insert-directory (locate-user-emacs-file "inserts"))
(add-to-list 'auto-insert-alist
             '(("CMakeLists\\.txt\\'" . "CMake build configuration")
               nil "cmake_minimum_required(VERSION 3.15)\n"
               "project(" (read-string "Project: ") & ")\n" | -45
               _))

;; Custom location for Customize
(setq custom-file (locate-user-emacs-file "custom.el"))
(if (file-exists-p (locate-user-emacs-file "custom.el"))
    (load-file (locate-user-emacs-file "custom.el")))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

;; Disable auto insertion of new line after `;'.
(add-hook 'verilog-mode-hook
          (lambda ()
            (define-key verilog-mode-map ";" nil)))

;; Use Noto font
(set-fontset-font t 'unicode "Noto Sans CJK JP")
(set-fontset-font t 'symbol "Noto Color Emoji")

(setq frame-title-format "%b - Emacs")

;; Overwrite selected area
(delete-selection-mode 1)

(windmove-default-keybindings 'meta)

(load "feeds")
(defun news ()
  (interactive)
  (newsticker-show-news))

(defmacro :? (obj &rest body)
  "Evaluate BODY if OBJ is nil."
  `(let ((arg ,obj))
     (if arg
         arg
       ,@body)))

(defun source-line ()
  "Put line number string for current point or region to kill-ring in
filename#L1-L2 form."
  (interactive)
  (let ((file (file-name-nondirectory (:? (buffer-file-name) ""))))
    (kill-new
     (if (use-region-p)
         (let ((first (line-number-at-pos (region-beginning)))
               (last (line-number-at-pos (region-end))))
           (format "%s#L%d-L%d" file first last))
       (format "%s#L%d" file (line-number-at-pos (point))))))
  (deactivate-mark))

;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (locate-user-emacs-file "init-local.el"))
    (load-file (locate-user-emacs-file "init-local.el")))

(setq file-name-handler-alist my-saved-file-name-handler-alist)
(setq gc-cons-threshold 16777216)
