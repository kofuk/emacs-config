;;; remocon.el --- Emacs remote controller           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Koki Fukuda

;; Author: Koki Fukuda
;; Keywords: tools

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

;; Expose emacs functionality to DBus interface.

;;; Code:

(require 'dbus)

(defun remocon--handle-open-buffer (path)
    (message (concat "Open file requested from remote: " (file-name-nondirectory path)))
    (find-file-other-window path)
    t)

(defun remocon--turn-on ()
  (let ((opener-interface (format "org.kofuk.EmacsOpener%d" (emacs-pid))))
    (dbus-register-method
     :session
     opener-interface
     "/org/kofuk/EmacsOpener"
     opener-interface
     "OpenBuffer"
     #'remocon--handle-open-buffer))
  (message "D-Bus service enabled."))

(defun remocon--turn-off ()
  (dbus-unregister-service
   :session
   (format "org.kofuk.EmacsOpener%d" (emacs-pid)))
  (message "D-Bus service disabled."))

(define-minor-mode remocon-mode
  "Expose emacs functionality via D-Bus (global minor mode)."
  :lighter " RC"
  :global t
  :group 'tools
  (if (not (equal system-type 'gnu/linux))
      (progn
        (message "remocon-mode is not supported on non-GNU/Linux environments.")
        (setq remocon-mode nil))
    (if remocon-mode
        (remocon--turn-on)
      (remocon--turn-off))))

(provide 'remocon)
;;; remocon.el ends here
