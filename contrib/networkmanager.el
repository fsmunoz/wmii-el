;;; networkmanager.el --- display NetworkManager connection information  -*- coding: iso-8859-1 -*-

;; Copyright (C) 2014 Frederico Muñoz

;; Author: Frederico Muñoz <fsmunoz@gmail.com>
;; Keywords: network dbus network-manager notification

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; uses dbus to display connection information

;;; Code:

(require 'dbus)

(defvar nm-mode-line-string nil
  "String to display in the mode line.")

(defvar nm-signal-registration nil
  "Object that represents the registration")

;; dbus subscription

(defun nm-subscribe ()
  (dbus-register-signal 
   :system    
   nil 
   nil
   "org.freedesktop.NetworkManager.Device"
   "StateChanged"
   'nm-state-changed-handler))


;;;###autoload (put 'battery-mode-line-string 'risky-local-variable t)

;;;###autoload
(define-minor-mode nm-mode
  "Toggle battery status display in mode line (Display Battery mode).
With a prefix argument ARG, enable Display Battery mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

The text displayed in the mode line is controlled by
`battery-mode-line-format' and `battery-status-function'.
The mode line is be updated every `battery-update-interval'
seconds."
  :global t :group 'net-utils
  (setq nm-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (if (not nm-mode)
      (progn
	(setq global-mode-string
	      (delq 'nm-mode-line-string global-mode-string))
	(message "Unregistering from bus")
	(dbus-unregister-object nm-signal-registration)
	(setq nm-mode nil)
	(setq nm-signal-registration nil))
    (add-to-list 'global-mode-string 'nm-mode-line-string t)
    (setq nm-signal-registration (nm-subscribe))
    (setq nm-mode-line-string (format "[%s]" (wmii-button-net)))))
	    

(provide 'nm)

;;; network-manager.el ends here
