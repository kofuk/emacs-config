;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Add ~/.emacs.d/site-lisp and subdirs of ~/.emacs.d/site-lisp to load-path if it exists
(if (file-exists-p (expand-file-name "~/.emacs.d/site-lisp"))
    (progn
      (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
        (normal-top-level-add-subdirs-to-load-path))
      (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))))

;; Add subdirs of ~/.emacs.d/site-lisp-local to load-path if it exists
(if (file-exists-p (expand-file-name "~/.emacs.d/site-lisp-local"))
    (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp-local")))
      (normal-top-level-add-subdirs-to-load-path)))

(require 'init-install-packages)

;; Dracula Theme
(load-theme 'dracula t)

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)
(setq font-lock-maximum-decornation t)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Display line and column number in mode line.
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(setq transient-mark-mode t)
(setq hl-line-face 'underline)
(global-hl-line-mode t)

;; Show line number in the left
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; Display datetime.
(setq display-time-interval 1)
(setq display-time-string-forms
      '((format "%s/%s/%s(%s) %s:%s:%s" year month day dayname 24-hours minutes seconds)))
(display-time)
(display-time-mode 1)

;; Start up screen
(setq inhibit-startup-screen t)

;; Disable menu bar since it is not needed.
(menu-bar-mode 0)

;; Save cursor position
(if (fboundp 'save-place-mode) (save-place-mode 1) (setq-default save-place t))

;; Follow symbolic links to versioned files
(setq vc-follow-symlinks t)

;; Backup file
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Show whitespaces
(setq whitespace-style '(face trailing tabs tab-mark))
(setq-default indent-tabs-mode nil)
(global-whitespace-mode 1)

;; Make easy to split window
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

;; Abbrev
(setq abbrev-mode t)
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(global-set-key "\C-x'" 'just-one-space)
(global-set-key "\M- " 'dabbrev-expand)
(global-set-key "\M-/" 'expand-abbrev)
(eval-after-load "abbrev" '(global-set-key "\M-/" 'expand-abbrev))

;; Auto insert
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/inserts")

;; Custom location for Customize
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(if (file-exists-p (expand-file-name "~/.emacs.d/custom.el"))
    (load-file (expand-file-name "~/.emacs.d/custom.el")))

(add-hook 'dired-mode-hook
          (lambda()
            (dired-hide-details-mode)))

(if window-system
    (require 'init-gui))

(require 'init-installed-packages)

;; Add code here.


;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (expand-file-name "~/.emacs.d/init-local.el"))
    (load-file (expand-file-name "~/.emacs.d/init-local.el")))
