;; For Emacs 27.1+, we don't have to call package-initialize implicitly,
;; so call it only if running on older Emacs.
(if (version< emacs-version "27.1")
    (package-initialize))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq init-path (file-name-directory load-file-name))

(dolist (path '("site-lisp" "site-lisp-local" "contrib"))
  (add-to-list 'load-path (concat init-path path)))

(load (concat init-path "ensure-packages.el"))

(load-theme 'wombat t)

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
(setq hl-line-face nil)
(set-cursor-color "white")

(setq-default indicate-empty-lines t)

;; Show line number in the left
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; Start up screen
(setq inhibit-startup-screen t)

;; Disable menu bar since it is not needed.
(menu-bar-mode 0)

;; Save cursor position
(if (fboundp 'save-place-mode) (save-place-mode 1) (setq-default save-place t))

;; Follow symbolic links to versioned files
(setq vc-follow-symlinks t)

;; Backup file
(setq backup-directory-alist `((".*" . ,(concat init-path "backups"))))
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Show whitespaces
(setq whitespace-style '(face trailing tabs tab-mark))
(setq-default indent-tabs-mode nil)
(global-whitespace-mode 1)

(setq read-file-name-completion-ignore-case t)

;; Make easy to split window
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)

;; Disable annoying key
(global-set-key "\C-v" nil)

(defun revert-buffer-noconfirm ()
  "Reverts buffer data from assciated file, without any prompt"
  (interactive)
  (revert-buffer 1 1 1))

(global-set-key (kbd "<f5>") 'revert-buffer-noconfirm)

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

;; Avoid messing up gnome-terminal when chars with ambiguous width are displayed.
(set-language-environment "English")

;; Auto insert
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat init-path "inserts"))

;; Custom location for Customize
(setq custom-file (concat init-path "custom.el"))
(if (file-exists-p (concat init-path "custom.el"))
    (load-file (concat init-path "custom.el")))

(add-hook 'dired-mode-hook
          (lambda()
            (dired-hide-details-mode)))

;; Settings to use GUI comfortably.
(tool-bar-mode 0)
(setq default-frame-alist '((width . 100)
                            (height . 35)
                            (cursor-type . bar)))

;; Use Noto font for kanji and half-width katakana
(when window-system
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t 'unicode "-GOOG-Noto Sans CJK JP-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")))
  (set-fontset-font t 'unicode "-GOOG-Noto Sans CJK JP-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"))

(setq frame-title-format "%b - Emacs")

;; Overwrite selected area
(delete-selection-mode 1)

(load "feeds")
(defun news ()
  (interactive)
  (newsticker-show-news))

(load (concat init-path "configure-packages.el"))

;; Add code here.


;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (concat init-path "init-local.el"))
    (load-file (concat init-path "init-local.el")))
