;;; -*- lexical-binding: t -*-

(package-initialize)

(if (version< emacs-version "27.1")
    (progn
      (load (locate-user-emacs-file "early-init.el"))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(dolist (path '("site-lisp" "site-lisp-local" "third_party"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;;; Package configurations

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure theme first (to avoid flicker while startup)
(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-fringes 'intense)
  (modus-themes-paren-match '(bold))
  (modus-themes-region '(bg-only))
  (modus-themes-mode-line '(accented borderless))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(use-package autoinsert
  :config
  (auto-insert-mode 1)
  (add-to-list 'auto-insert-alist
               '(("CMakeLists\\.txt\\'" . "CMake build configuration")
                 nil "cmake_minimum_required(VERSION 3.15)\n"
                 "project(" (read-string "Project: ") & ")\n" | -45
                 _))
  :custom
  (auto-insert-directory (locate-user-emacs-file "inserts")))

(use-package cc-vars
  :defer t
  :hook
  ;; comment style
  (c-mode-common . (lambda () (c-toggle-comment-style -1)))
  :custom
  (c-basic-offset 4)
  (c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "bsd"))))

(use-package clang-format+
  :defer t
  :ensure t)

(use-package cmake-mode
  :defer t
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") #'company-show-doc-buffer)
  :bind
  ("C-M-i" . 'company-complete)
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-deabbrev-downcase nil))

(use-package company-go
  :ensure t
  :defer t)

(use-package csharp-mode
  :ensure t
  :defer t)

(use-package cua-base
  :defer t
  :bind
  ("C-x SPC" . #'cua-rectangle-mark-mode))

(use-package cus-edit
  :config
  ;; Custom location for Customize
  (if (file-exists-p (locate-user-emacs-file "custom.el"))
      (load-file (locate-user-emacs-file "custom.el")))
  :custom
  (custom-file (locate-user-emacs-file "custom.el")))

(use-package delsel
  :config
  ;; Overwrite selected area
  (delete-selection-mode 1))

(use-package dired
  :defer t
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode))))

(use-package display-line-numbers
  :if (version<= "26.0.50" emacs-version)
  :config
  (global-display-line-numbers-mode))

(use-package dockerfile-mode
  :defer t
  :ensure t)

(use-package editorconfig
  :ensure t
  :defer t
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

(use-package electric
  :config
  (electric-indent-mode -1))

(use-package emacs
  :custom
  (indicate-empty-lines t)
  (create-lockfiles nil)
  ;; Start up screen
  (inhibit-startup-screen t)
  (indent-tabs-mode nil)
  (history-delete-duplicates t)
  (initial-scratch-message nil)
  :config
  ;; Completion
  (setq completion-ignore-case t)
  ;; Use Noto font
  (set-fontset-font t 'unicode "Noto Sans CJK JP")
  (set-fontset-font t 'symbol "Noto Color Emoji")
  ;; Simplify window title
  (setq frame-title-format '("%b - Emacs"))
  ;; Keep scratch buffer empty
  ;; Scroll the buffer
  :bind
  ;; Disable annoying key
  ("C-v" . nil)
  ("M-n" . #'scroll-up-line)
  ("M-p" . #'scroll-down-line)
  ("C-x -" . #'split-window-vertically)
  ("C-x |" . #'split-window-horizontally))

(use-package font-lock
  :custom
  (font-lock-support-mode 'jit-lock-mode))

(use-package files
  :custom
  ;; Backup file
  (backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups"))))
  (auto-save-default nil)
  ;; Don't warn file size up to 100MB.
  (large-file-warning-threshold  100000000)
  ;; Make Emacs to put '\n' at the end of file
  (require-final-newline t)
  :mode
  ("PKGBUILD\\'" . shell-script-mode))

(use-package frame
  :config
  (set-cursor-color "white"))

(use-package gdb-mi
  :defer t
  :hook
  (gdb-mode . (lambda () (gud-tooltip-mode t)))
  :config
  (setq gdb-many-windows t)
  (setq gdb-use-separate-io-buffer t)
  (setq gud-tooltip-echo-area t))

(use-package git-gutter
  :defer t
  :ensure t
  :custom
  (global-git-gutter-mode t))

(use-package go-mode
  :defer t
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook
  (prog-mode . (lambda () (highlight-indent-guides-mode t))))

(use-package hl-line-mode
  :defer t
  :custom
  (global-hl-line-mode t))

;; Backward compatibility
(use-package linum
  :if (version< emacs-version "26.0.50")
  :config
  (global-linum-mode t))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :hook
  (markdown-mode . (lambda () (require 'hugo-utils))))

(use-package meson-mode
  :ensure t
  :defer t)

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t))

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mozc
  :if (executable-find "mozc_emacs_helper")
  :ensure t
  :defer t
  :custom
  (default-input-method "japanese-mozc")
  (mozc-candidate-style 'echo-area))

(use-package newst-reader
  :config
  (require 'feeds)
  (defun news ()
    (interactive)
    (newsticker-show-news)))

(use-package paren
  :config
  (show-paren-mode t)
  :custom
  (show-paren-style 'mixed))

(use-package sass-mode
  :ensure t
  :defer t)

(use-package satysfi
  :config
  (if (equal system-type 'gnu/linux)
      (setq satysfi-pdf-viewer-command "evince"))
  :mode
  ("\\.saty\\'" . satysfi-mode)
  ("\\.satyh\\'" . satysfi-mode))

(use-package saveplace
  :config
  ;; Save cursor position
  (if (fboundp 'save-place-mode)
      (save-place-mode 1)
    (setq-default save-place t)))

(use-package simple
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1))

(use-package so-long
  :config
  (global-so-long-mode t))

(use-package source-line)

(use-package subword
  :config
  ;; Recognize words in camelCaseString.
  (global-subword-mode t))

(use-package rust-mode
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  :custom
  (undo-tree-mode-lighter ""))

(use-package vc-hooks
  :custom
  ;; Follow symbolic links to versioned files
  (vc-follow-symlinks t))

(use-package verilog-mode
  :defer t
  :hook
  ;; Disable auto insertion of new line after `;'.
  (verilog-mode . (lambda () (define-key verilog-mode-map ";" nil))))

(use-package vterm
  :if (equal system-type 'gnu/linux)
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-v") nil)
  (define-key vterm-mode-map (kbd "C-v C-v") #'vterm-send-C-v)
  (define-key vterm-mode-map (kbd "C-v ESC") #'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
  (define-key vterm-copy-mode-map (kbd "ESC") #'vterm-copy-mode))

(use-package web-mode
  :ensure t
  :defer t
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-auto-close-style 2)
  (web-mode-enable-current-element-highlight t))

(use-package which-func
  :config
  (which-function-mode t))

(use-package whitespace
  :config
  (global-whitespace-mode 1)
  :custom
  ;; Show whitespaces
  (whitespace-style '(face trailing tabs tab-mark)))

(use-package windmove
  :config
  (windmove-default-keybindings 'meta))

(defun revert-buffer-noconfirm (force-utf-8)
  "Reverts buffer data from assciated file, without any prompt"
  (interactive "P")
  (let ((coding-system-for-read (if force-utf-8 'utf-8 nil)))
    (revert-buffer 1 1 1)))

(global-set-key (kbd "<f5>") #'revert-buffer-noconfirm)

;; Display battery status only on laptop.
(if (file-exists-p "/sys/class/dmi/id/chassis_type")
    (with-temp-buffer
      (insert-file-contents "/sys/class/dmi/id/chassis_type")
      (if (not (string= (buffer-string) "3\n"))
          (display-battery-mode 1))))

(when (equal system-type 'gnu/linux)
  (require 'dbus)
  (defun open-buffer (path)
    (message (concat "Open file requested from remote: " (file-name-nondirectory path)))
    (find-file-other-window path)
    t)
  (let ((opener-interface (format "org.kofuk.EmacsOpener%d" (emacs-pid))))
    (dbus-register-method
     :session
     opener-interface
     "/org/kofuk/EmacsOpener"
     opener-interface
     "OpenBuffer"
     #'open-buffer)))

;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (locate-user-emacs-file "init-local.el"))
    (load-file (locate-user-emacs-file "init-local.el")))

(setq file-name-handler-alist my-saved-file-name-handler-alist)
(setq gc-cons-threshold 16777216)
