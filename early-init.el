;;; -*- lexical-binding: t -*-

(if (eq system-type 'windows-nt)
    (cd "~"))

(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

(setq default-frame-alist '((width . 120)
                            (height . 40)
                            (cursor-type . bar)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (undecorated . t)))

(setq frame-inhibit-implied-resize t)
