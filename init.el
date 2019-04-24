;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Add subdirs of ~/.emacs.d/site-lisp to load-path if it exists
(if (file-exists-p (expand-file-name "~/.emacs.d/site-lisp"))
    (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
      (normal-top-level-add-subdirs-to-load-path)))

;; Add subdirs of ~/.emacs.d/site-lisp-local to load-path if it exists
(if (file-exists-p (expand-file-name "~/.emacs.d/site-lisp-local"))
    (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp-local")))
      (normal-top-level-add-subdirs-to-load-path)))

(load-theme 'manoj-dark t)

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)
(setq font-lock-maximum-decornation t)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Display line and column number in mode line.
(line-number-mode 1)
(column-number-mode 1)
(setq transient-mark-mode t)
(setq hl-line-face 'underline)
(global-hl-line-mode t)
(global-linum-mode t)

;; Display datetime.
(setq display-time-interval 1)
(setq display-time-string-forms
      '((format "%s/%s/%s(%s) %s:%s:%s" year month day dayname 24-hours minutes seconds)))
(display-time)
(display-time-mode 1)

;; Start up screen
(setq inhibit-startup-screen t)

;; Disable menu bar since it is not needed.
(menu-bar-mode -1)

;; Use C-h as <Delback>.
(global-set-key "\C-h" 'delete-backward-char)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; Alternate bell
(defun modeline-bell ()
  (set-face-background 'mode-line "yellow")
  (run-at-time "100 millisec" nil #'set-face-background #'mode-line "white"))
(setq ring-bell-function 'modeline-bell)

;; Save cursor position
(if (fboundp 'save-place-mode) (save-place-mode 1) (setq-default save-place t))

;; Follow symbolic links to versioned files
(setq vc-follow-symlinks t)

;; Don't make backup file
(setq make-backup-files t)

;; Show whitespaces
(setq whitespace-style '(face trailing tabs tab-mark))
(setq-default indent-tabs-mode nil)
(global-whitespace-mode 1)

;; Make inserting line easy
(defun open-next-line ()
  "Opens new line in next line of the cursor and move cursor to the line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(global-set-key "\C-j" 'open-next-line)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)

;; Make Emacs to put '\n' at the end of file
(setq require-final-newline t)

;; Spell check
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  ;; Enable flyspell mode automatically only if I edit plain text file.
  ;; flyspell-prog-mode disturb completion from working properly thus I don't
  ;; enable it if I edit source code.
  (mapc (lambda (hook)
          (add-hook hook '(lambda () (flyspell-mode 1))))
        '(text-mode-hook)))

;; Set key so that I can switch buffer easily.
(global-set-key (kbd "C-c <C-right>") 'next-buffer)
(global-set-key (kbd "C-c <C-left>") 'previous-buffer)

;; Abbrev
(setq abbrev-mode t)
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(global-set-key "\C-x'" 'just-one-space)
(global-set-key "\M- " 'dabbrev-expand)
(global-set-key "\M-/" 'expand-abbrev)
(eval-after-load "abbrev" '(global-set-key "\M-/" 'expand-abbrev))

;; Custom location for Customize
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(if (file-exists-p (expand-file-name "~/.emacs.d/custom.el"))
    (load-file (expand-file-name "~/.emacs.d/custom.el")))

(add-hook 'dired-mode-hook
          (lambda()
            (dired-hide-details-mode)))

(setq skk-jisyo-code 'utf-8)

(use-package auto-complete
  :config
  (ac-config-default)
  (setq ac-use-menu-map t)
  (global-auto-complete-mode 1))

(use-package eglot
  :config
  (when (executable-find "clangd")
    (add-to-list 'eglot-server-programs '((c-mode c++-mode ) "clangd"))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)))

(use-package undo-tree
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-character-face-perc 30)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Add code here.


;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (expand-file-name "~/.emacs.d/init-local.el"))
    (load-file (expand-file-name "~/.emacs.d/init-local.el")))
