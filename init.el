;;; -*- lexical-binding: t -*-

(if (version< emacs-version "27.1")
    (progn
      (load (locate-user-emacs-file "early-init.el"))))

(customize-set-variable 'package-archives
                        '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; Install leaf.el if it's not installed.
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

;; Initialize leaf-keywords to use leaf's convenient functions later.
(leaf leaf-keywords
  :ensure t
  :config
  (leaf-keywords-init))

(dolist (path '("site-lisp" "site-lisp-local" "third_party"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; Configure theme first (to avoid flicker while startup)
(leaf modus-themes
  :doc "Set Emacs's theme to modus vivendi theme."
  :ensure t
  :custom ((modus-themes-italic-constructs . t)
           (modus-themes-bold-constructs . t)
           (modus-themes-fringes . 'intense)
           (modus-themes-paren-match . '(bold))
           (modus-themes-region . '(bg-only))
           (modus-themes-mode-line . '(accented borderless))
           (modus-themes-hl-line . '(intense)))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(leaf all-the-icons
  :ensure t
  :config
  (unless (or (equal system-type 'windows-nt) (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts)))

(leaf autoinsert
  :doc "Enable auto-insert-mode and add specific templates."
  :config
  (auto-insert-mode 1)
  (add-to-list 'auto-insert-alist
               '(("CMakeLists\\.txt\\'" . "CMake build configuration")
                 nil "cmake_minimum_required(VERSION 3.15)\n"
                 "project(" (read-string "Project: ") & ")\n" | -45
                 _)))

(leaf battery
  :if (and (equal system-type 'gnu/linux) (file-exists-p "/sys/class/dmi/id/chassis_type"))
  :require t
  :config
  (with-temp-buffer
      (insert-file-contents "/sys/class/dmi/id/chassis_type")
      (goto-char (point-min))
      (when (not (= (thing-at-point 'number) 3))
          (display-battery-mode 1))))

(leaf buffer
  :setq-default ((bidi-display-reordering 'left-to-right)
                 (bidi-paragraph-direction 'left-to-right)
                 (fill-column . 120))
  :custom ((indicate-empty-lines . t)
           (tab-width . 4)
           (cursor-in-non-selected-windows . nil)))

(leaf cc-vars
  :hook (c-mode-common-hook . (lambda ()
                                ;; comment style
                                (c-toggle-comment-style -1)))
  :custom ((c-basic-offset . 4)
           (c-default-style . '((java-mode . "java") (awk-mode . "awk") (other . "bsd")))))

(leaf clang-format+
  :ensure t)

(leaf cmake-mode
  :ensure t)

(leaf company
  :ensure t
  :require t
  :config
  (global-company-mode 1)
  (setq company-backends (remove 'company-clang company-backends))
  :bind
  (("C-M-i" . company-complete)
   (:company-active-map
    ("C-n" . #'company-select-next)
    ("C-p" . #'company-select-previous)
    ("C-S-h" . #'company-show-doc-buffer)
    ("C-h")))
  :custom ((company-transformers . '(company-sort-by-backend-importance))
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-deabbrev-downcase . nil)))

(leaf csharp-mode
  :ensure t)

(leaf cus-edit
  :if (equal system-type 'gnu/linux)
  :custom ((custom-file . null-device)))

(leaf delsel
  :config
  ;; Overwrite selected area
  (delete-selection-mode 1))

(leaf dired
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode))))

(leaf display-line-numbers
  :emacs>= "26.0.50"
  :config
  (global-display-line-numbers-mode)
  :custom ((display-line-numbers-widen . t)
           (display-line-numbers-width-start . 3)))

(leaf dockerfile-mode
  :ensure t)

(leaf doom-modeline
  :ensure t
  :setq ((doom-modeline-percent-position . "	")) ;; Dirty hack to align misc info to the right.
  :init
  (doom-modeline-mode t)
  :custom ((doom-modeline-hud . t)
           (doom-modeline-buffer-file-name-style . 'file-name)
           (doom-modeline-minor-modes . t)
           (doom-modeline-buffer-encoding . 'nondefault)
           (doom-modeline-env-version . nil)))

(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(leaf eglot
  :ensure (t eldoc-box)
  :require t
  :config
  (if (executable-find "rust-analyzer")
      (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))
  ;; Remove unneeded "\r" because it appears in doc buffer as "^M" and is annoying.
  (advice-add 'eglot--format-markup :around
              (lambda (orig-func &rest args)
                (when (stringp (car args))
                  (setf (car args) (string-replace "\r" "" (car args))))
                (apply orig-func args)))
  :custom ((eldoc-box-max-pixel-width . 600)
           (eldoc-box-max-pixel-height . 800)
           (eldoc-box-offset . '(16 24 24))
           (eldoc-box-fringe-use-same-bg . nil)))

(leaf eldoc
  :hook
  (eldoc-mode-hook . eldoc-box-hover-mode))

(leaf emmet-mode
  :ensure t
  :hook
  (web-mode-hook . emmet-mode))

(leaf filelock
  :custom ((create-lockfiles . nil)))

(leaf files
  :require t
  :custom ((auto-mode-case-fold . nil)
           ;; Backup file
           (backup-directory-alist . `((".*" . ,(locate-user-emacs-file "backups"))))
           (make-backup-files . nil)
           (auto-save-default . nil)
           ;; Don't warn file size up to 100MB.
           (large-file-warning-threshold .  100000000)
           ;; Make Emacs to put '\n' at the end of file
           (require-final-newline . t))
  :bind (("<f5>" . (lambda (force-utf-8)
                     "Reverts buffer data from assciated file, without any prompt"
                     (interactive "P")
                     (let ((coding-system-for-read (if force-utf-8 'utf-8 nil)))
                       (revert-buffer 1 1 1))))))

(leaf frame
  :config
  (set-cursor-color "white"))

(leaf font
  :setq ((inhibit-compacting-font-caches . t)))

(leaf fontset
  :config
  ;; Use Noto font
  (set-fontset-font t 'unicode
                    (if (equal system-type 'windows-nt)
                        "Noto Sans JP"
                      "Noto Sans CJK JP")))

(leaf fontset
  :emacs< "28.0"
  :config
  (set-fontset-font t 'symbol "Noto Color Emoji"))

(leaf fontset
  :emacs>= "28.0"
  :config
  ;; Set Emoji font to "Noto Color Emoji" implicitly.
  ;; Although it's Emacs' default but it doesn't used as-is because
  ;; I specified different font above.
  (set-fontset-font t 'emoji
                    '("Noto Color Emoji" . "iso10646-1") nil 'prepend)
  :custom ((mode-line-compact . t)))

(leaf fontset
  :if (equal system-type 'windows-nt)
  :config
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-10")))

(leaf gdb-mi
  :hook
  (gdb-mode . (lambda () (gud-tooltip-mode t)))
  :setq ((gdb-use-separate-io-buffer . t))
  :custom ((gdb-many-windows . t)
           (gud-tooltip-echo-area . t)))

(leaf git-gutter
  :ensure t
  :custom ((global-git-gutter-mode . t)))

(leaf go-mode
  :ensure t)

(leaf hl-line-mode
  :custom ((global-hl-line-mode . t)))

(leaf hugo-utils
  :require t)

(leaf indent
  :custom ((indent-tabs-mode . nil)))

(leaf jsonian
  :ensure t)

(leaf js
  :mode ("tsconfig\\.json\\'")
  :hook ((js-mode-hook . (lambda ()
                           (when (string-match-p "tsconfig\\.json\\'" buffer-file-name)
                             (make-local-variable 'js-indent-level)
                             (setq js-indent-level 2))))))

(leaf linum
  :emacs< "26.0.50"
  :config
  (global-linum-mode t))

(leaf lua-mode
  :ensure t)

(leaf markdown-mode
  :ensure t
  :bind ((:markdown-mode-map
          ("M-n")
          ("M-p"))))

(leaf menu-bar
  :config
  (menu-bar-mode -1))

(leaf meson-mode
  :ensure t)

(leaf minibuf
  :custom ((history-delete-duplicates . t))
  :setq
  ;; Completion
  (completion-ignore-case . t))

(leaf minibuffer
  :custom ((read-file-name-completion-ignore-case . t)))

(leaf minions
  :ensure t
  :init
  (minions-mode t))

(leaf mozc
  :if (or (executable-find "mozc_emacs_helper")
          (and (equal system-type 'windows-nt)
               (executable-find "wsl")
               ;; Use "Ubuntu" installation. Should I use variable to set Distro name?
               (call-process "wsl" nil nil nil "-d" "Ubuntu" "-e" "command" "-v" "mozc_emacs_helper")))
  :ensure t
  :config
  (if (equal system-type 'windows-nt)
      (setq mozc-helper-program-name "wsl"
            mozc-helper-program-args '("-d" "Ubuntu" "-e" "mozc_emacs_helper" "--supress_stderr")))
  :custom ((default-input-method . "japanese-mozc")
           (mozc-candidate-style . 'echo-area)))

(leaf mule-cmds
  :config
  (prefer-coding-system 'utf-8-unix))

(leaf package
  :custom ((package-native-compile . t)))

(leaf paragraphs
  :bind (("M-[" . backward-paragraph) ("M-]" . forward-paragraph)))

(leaf paren
  :config
  (show-paren-mode t)
  :custom ((show-paren-when-point-inside-paren . t)
           (show-paren-context-when-offscreen . 'child-frame)))

(leaf project
  :require (cl-generic)
  :config
  (cl-defmethod project-root ((project (head golang)))
    (car (cdr project)))
  :hook ((project-find-functions . (lambda (directory)
                                     (while (and directory
                                                 (not (string= (file-name-directory directory)
                                                               (directory-file-name directory)))
                                                 (not (file-exists-p (concat directory "go.mod"))))
                                       (setq directory (file-name-directory (directory-file-name directory))))
                                     (if (file-exists-p (concat directory "go.mod"))
                                         `(golang ,directory)
                                       nil)))))

(leaf remocon
  :require t
  :config
  (remocon-mode t))

(leaf ruler-mode
  :hook
  (prog-mode-hook . (lambda () (ruler-mode 1))))

(leaf rust-mode
  :ensure t)

(leaf sass-mode
  :ensure t)

(leaf satysfi
  :config
  (if (equal system-type 'gnu/linux)
      (setq satysfi-pdf-viewer-command "evince"))
  :mode
  ("\\.saty\\'" "\\.satyh\\'"))

(leaf saveplace
  :config
  (save-place-mode t))

(leaf scroll-bar
  :init
  (scroll-bar-mode -1))

(leaf sh-script
  :mode
  (("PKGBUILD\\'" . sh-mode)))

(leaf simple
  :init
  (line-number-mode -1)
  :bind (("C-h" . #'delete-backward-char))
  :custom ((idle-update-delay . 1.0)))

(leaf so-long
  :config
  (global-so-long-mode t))

(leaf source-line
  :require t)

(leaf startup
  :custom ((inhibit-startup-echo-area-message . t)
           ;; Start up screen
           (inhibit-startup-screen . t)
           (initial-scratch-message . nil)
           (auto-save-list-file-prefix . nil)
           (inhibit-default-init . t)))

(leaf subword
  :config
  ;; Recognize words in camelCaseString.
  (global-subword-mode t))

(leaf tooltip
  :config
  (tooltip-mode -1))

(leaf tool-bar-mode
  :config
  (tool-bar-mode -1))

(leaf tramp
  :require t
  :config
  (add-to-list 'tramp-remote-path 'tramp-default-remote-path)
  :custom ((tramp-histfile-override . t)))

(leaf tree-sitter
  :ensure (t tree-sitter-langs)
  :require t
  :config
  (global-tree-sitter-mode t)
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  :mode (("\\.tsx\\'" . typescript-tsx-mode)))

(leaf undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  :custom ((undo-tree-mode-lighter . "")
           (undo-tree-visualizer-diff . t)
           (undo-tree-auto-save-history . nil)))

(leaf vc-hooks
  :custom (;; Follow symbolic links to versioned files
           (vc-follow-symlinks . t)))

(leaf verilog-mode
  :bind ((:verilog-mode-map
          (";"))))

(leaf vterm
  :if (equal system-type 'gnu/linux)
  :ensure t
  :bind-keymap
  (:vterm-mode-map
   ("C-v")
   ("C-v C-v" . #'vterm-send-C-v)
   ("C-v ESC" . #'vterm-copy-mode)
   ("C-y" . #'vterm-yank)
   ("ESC" . #'vterm-copy-mode)))

(leaf web-mode
  :ensure t
  :mode
  ("\\.php\\'" "\\.html?\\'" "\\.jsx\\'")
  :custom ((web-mode-markup-indent-offset . 2)
           (web-mode-auto-close-style . 0)
           (web-mode-enable-current-element-highlight . t)))

(leaf whitespace
  :config
  (global-whitespace-mode 1)
  :custom (;; Show whitespaces
           (whitespace-style . '(face trailing tabs tab-mark))))

(leaf windmove
  :config
  (windmove-default-keybindings 'meta))

(leaf window
  :bind (;; Disable annoying key
         ("C-v")
         ("M-n" . #'scroll-up-line)
         ("M-p" . #'scroll-down-line)
         ("C-x -" . #'split-window-vertically)
         ("C-x |" . #'split-window-horizontally))
  :custom ((fast-but-imprecise-scrolling . t)))

(leaf xdisp
  :setq ((bidi-inhibit-bpa . t)
         (redisplay-skip-fontification-on-input . t)
         ;; Simplify window title
         (frame-title-format
          . '(:eval
              (let ((bn (buffer-name)))
                (cond
                 ((string= bn "*scratch*") "scratch")
                 ((string= bn "*vterm*") "Terminal")
                 ((string= bn "*Messages*") "Messages")
                 (t "%b")))))))

(leaf yaml-mode
  :ensure t
  :mode
  ("\\.clang-tidy\\'"))

(leaf zig-mode
  :ensure t
  :custom ((zig-format-on-save . nil)))

;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (locate-user-emacs-file "init-local.el"))
    (load-file (locate-user-emacs-file "init-local.el")))

(setq file-name-handler-alist my-saved-file-name-handler-alist)
(makunbound 'my-saved-file-name-handler-alist)
(setq gc-cons-threshold 134217728)
