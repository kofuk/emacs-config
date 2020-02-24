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

;; LSP
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c-mode-hook 'lsp-ui-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook 'lsp-ui-mode)

;; C/C++ comment style
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-comment-style 1)))

;; Company
(with-eval-after-load 'company
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

(use-package undo-tree
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package mozc
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area))

(provide 'init-installed-packages)
;;; init-installed-packages.el ends here
