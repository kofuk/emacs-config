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
           (modus-themes-mode-line . '(accented borderless)))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

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
  :if (file-exists-p "/sys/class/dmi/id/chassis_type")
  :require t
  :config
  (with-temp-buffer
      (insert-file-contents "/sys/class/dmi/id/chassis_type")
      (if (not (string= (buffer-string) "3\n"))
          (display-battery-mode 1))))

(leaf buffer
  :custom ((indicate-empty-lines . t)
           (tab-width . 4)))

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
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") #'company-show-doc-buffer)
  :bind
  (("C-M-i" . company-complete))
  :custom ((company-transformers . '(company-sort-by-backend-importance))
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-deabbrev-downcase . nil)))

(leaf company-go
  :ensure t)

(leaf csharp-mode
  :ensure t)

(leaf cua-base
  :bind
  (("C-x SPC" . cua-rectangle-mark-mode)))

(leaf cus-edit
  :if (equal system-type 'gnu/linux)
  :custom ((custom-file . "/dev/null")))

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
  :custom ((display-line-numbers-type . 'relative)
           (display-line-numbers-width-start . 3)))

(leaf dockerfile-mode
  :ensure t)

(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(leaf eglot
  :ensure t
  :require t
  :config
  (if (executable-find "rust-analyzer")
      (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))))

(leaf filelock
  :custom ((create-lockfiles . nil)))

(leaf files
  :require t
  :config
  (defun revert-buffer-noconfirm (force-utf-8)
    "Reverts buffer data from assciated file, without any prompt"
    (interactive "P")
    (let ((coding-system-for-read (if force-utf-8 'utf-8 nil)))
      (revert-buffer 1 1 1)))
  :custom (;; Backup file
           (backup-directory-alist . `((".*" . ,(locate-user-emacs-file "backups"))))
           (make-backup-files . nil)
           (auto-save-default . nil)
           ;; Don't warn file size up to 100MB.
           (large-file-warning-threshold .  100000000)
           ;; Make Emacs to put '\n' at the end of file
           (require-final-newline . t))
  :bind
  (("<f5>" . revert-buffer-noconfirm)))

(leaf frame
  :config
  (set-cursor-color "white"))

(leaf fontset
  :config
  ;; Use Noto font
  (set-fontset-font t 'unicode "Noto Sans CJK JP"))

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

(leaf highlight-indent-guides
  :ensure t
  :hook
  (prog-mode-hook . (lambda () (highlight-indent-guides-mode t))))

(leaf hl-line-mode
  :custom ((global-hl-line-mode . t)))

(leaf indent
  :custom ((indent-tabs-mode . nil)))

(leaf hugo-utils
  :require t)

(leaf linum
  :emacs< "26.0.50"
  :config
  (global-linum-mode t))

(leaf lua-mode
  :ensure t)

(leaf markdown-mode
  :ensure t
  :hook (markdown-mode-hook . (lambda ()
                                (keymap-unset markdown-mode-map "M-n" t)
                                (keymap-unset markdown-mode-map "M-p" t))))

(leaf meson-mode
  :ensure t)

(leaf minibuf
  :custom ((history-delete-duplicates . t))
  :setq
  ;; Completion
  (completion-ignore-case . t))

(leaf minibuffer
  :custom ((read-file-name-completion-ignore-case . t)))

(leaf moody
  :ensure t
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(leaf mozc
  :if (executable-find "mozc_emacs_helper")
  :ensure t
  :custom ((default-input-method . "japanese-mozc")
           (mozc-candidate-style . 'echo-area)))

(leaf newst-reader
  :require t
  :config
  (leaf feeds
    :require t
    :config
    (defun news ()
    (interactive)
    (newsticker-show-news))))

(leaf package
  :custom ((package-native-compile . t)))

(leaf paren
  :config
  (show-paren-mode t)
  :custom ((show-paren-style . 'mixed)))

(leaf remocon
  :require t
  :config
  (remocon-mode t))

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
  ;; Save cursor position
  (if (fboundp 'save-place-mode)
      (save-place-mode 1)
    (setq-default save-place t)))

(leaf sh-script
  :mode
  (("PKGBUILD\\'" . sh-mode)))

(leaf simple
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)
  :bind (("C-h" . #'delete-backward-char)))

(leaf so-long
  :config
  (global-so-long-mode t))

(leaf source-line
  :require t)

(leaf startup
  :custom (;; Start up screen
           (inhibit-startup-screen . t)
           (initial-scratch-message . nil)
           (auto-save-list-file-prefix . nil)))

(leaf subword
  :config
  ;; Recognize words in camelCaseString.
  (global-subword-mode t))

(leaf rust-mode
  :ensure t)

(leaf undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  :custom ((undo-tree-mode-lighter . "")))

(leaf vc-hooks
  :custom (;; Follow symbolic links to versioned files
           (vc-follow-symlinks . t)))

(leaf verilog-mode
  :hook
  ;; Disable auto insertion of new line after `;'.
  (verilog-mode-hook . (lambda () (define-key verilog-mode-map ";" nil))))

(leaf vterm
  :if (equal system-type 'gnu/linux)
  :ensure t
  :bind-keymap
  (:vterm-mode-map
   ("C-v" . nil)
   ("C-v C-v" . #'vterm-send-C-v)
   ("C-v ESC" . #'vterm-copy-mode)
   ("C-y" . #'vterm-yank)
   ("ESC" . #'vterm-copy-mode)))

(leaf web-mode
  :ensure t
  :mode
  ("\\.php\\'" "\\.html?\\'" "\\.jsx\\'")
  :custom ((web-mode-markup-indent-offset . 2)
           (web-mode-auto-close-style . 2)
           (web-mode-enable-current-element-highlight . t)))

(leaf which-func
  :config
  (which-function-mode t))

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
         ("C-v" . nil)
         ("M-n" . scroll-up-line)
         ("M-p" . scroll-down-line)
         ("C-x -" . split-window-vertically)
         ("C-x |" . split-window-horizontally)))

(leaf xdisp
  :setq
  ;; Simplify window title
  (frame-title-format
   . '(:eval
       (let ((bn (buffer-name)))
         (dotimes (i 2 result)
           (setq result
                 (if (string-match "^ ?\\*" bn)
                     (concat
                      (cond
                       ((string= bn "*scratch*") "scratch")
                       ((string= bn "*vterm*") "Terminal")
                       ((string= bn "*Messages*") "Messages")
                       ((string-match "^ \\*Minibuf-[0-9]+\\*$" bn)
                        (if (= i 0)
                            (setq bn (buffer-name (nth 1 (buffer-list))))
                          bn))
                       (t "%b"))
                      " - Emacs")
                   "%b - Emacs")))))))

(leaf yaml-mode
  :ensure t
  :mode
  ("\\.clang-tidy\\'"))

;; Execute local lisp initialization.
;; Execute in the last step of init.el so that it doesn't disturb
;; other package's initialization.
(if (file-exists-p (locate-user-emacs-file "init-local.el"))
    (load-file (locate-user-emacs-file "init-local.el")))

(setq file-name-handler-alist my-saved-file-name-handler-alist)
(setq gc-cons-threshold 134217728)
