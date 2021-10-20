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

(use-package modus-themes
  :ensure t
  :init
  (setq
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-fringes 'intense
   modus-themes-paren-match '(bold)
   modus-themes-region '(bg-only)
   modus-themes-mode-line '(accented borderless))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

;; C/C++ comment style
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-comment-style 1)))

(use-package clang-format+
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

(use-package dockerfile-mode
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

(use-package lua-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . (lambda () (require 'hugo-utils))))

(use-package meson-mode
  :ensure t)

(use-package moody
  :ensure t
  :config
 (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

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
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
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

(setq font-lock-support-mode 'jit-lock-mode)
(setq font-lock-maximum-decornation t)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Mode line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
;; Display battery status only on laptop.
(if (file-exists-p "/sys/class/dmi/id/chassis_type")
    (with-temp-buffer
      (insert-file-contents "/sys/class/dmi/id/chassis_type")
      (if (not (string= (buffer-string) "3\n"))
          (display-battery-mode 1))))

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

;; Recognize words in camelCaseString.
(global-subword-mode t)

(global-so-long-mode t)

(which-function-mode t)

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
(setq completion-ignore-case t)
(setq history-delete-duplicates t)

(setq initial-scratch-message nil)

;; Don't warn file size up to 100MB.
(setq large-file-warning-threshold  100000000)

;; Disable annoying key
(global-set-key "\C-v" nil)

;; Scroll the buffer line-by-line
(global-set-key "\M-n" #'scroll-up-line)
(global-set-key "\M-p" #'scroll-down-line)

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

;; Major mode
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . shell-script-mode))

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

;; "Clear the clutter."
(setq minor-mode-alist nil)

(setq file-name-handler-alist my-saved-file-name-handler-alist)
(setq gc-cons-threshold 16777216)
