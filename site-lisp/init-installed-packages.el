;;; init-installed-packages.el --- Perform initialization installed packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Koki Fukuda

;; Author: Koki Fukuda <ko.fu.dev@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Postfix completion
(require 'postfix)
(global-set-key "\C-z" 'postfix-completion)
(add-to-list 'postfix-snippets-alist '("c" . "~/.emacs.d/postfix-snippets/c"))
(add-to-list 'postfix-snippets-alist '("cc" . "~/.emacs.d/postfix-snippets/c++"))
(add-to-list 'postfix-snippets-alist '("cxx" . "~/.emacs.d/postfix-snippets/c++"))
(add-to-list 'postfix-snippets-alist '("cpp" . "~/.emacs.d/postfix-snippets/c++"))

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

(provide 'init-installed-packages)
;;; init-installed-packages.el ends here
