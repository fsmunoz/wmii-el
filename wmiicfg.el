;;; wmiicfg.el --- configuration file for wmii-el 
;;
;;  Copyright (C) 2015 Frederico Muñoz <fsmunoz@gmail.com>
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 3 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation,Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; 
;; This file is an example config file that should be copied to, say,
;; ~/.emacs.d/lisp or somewhere in the load-path; by calling
;; (wmii-init) it is responsible to start the wmiir process and start
;; controlling wmii and configure and extend it.
;;
;; Some aspects of it are likely not applicable as-is due to their
;; dependency on certain features which will surely vary across
;; different OSs, distributions and personal preference. They are kept
;; as examples and meant to be changed at will.


;;; Code:

(require 'wmii)

;;; COLOURS

;; Main colour theme I'm not making the internal lines visible, this
;; means that there will be no distinction between managed and
;; unmanaged windows.
(setq wmii-normal-colors (wmii-color-tuple "goldenrod" "grey20" "grey20"))
(setq wmii-focus-colors (wmii-color-tuple "white" "grey10" "grey10"))
(setq wmii-bar-colors (wmii-color-tuple "goldenrod" "grey10" "grey10"))
(setq wmii-urgent-colors (wmii-color-tuple "goldenrod" "grey10" "red"))

;; Activate the changes
(wmii-set-normal-colors wmii-normal-colors)
(wmii-set-focus-colors wmii-focus-colors)
(wmii-set-urgent-colors wmii-urgent-colors)

;;; FONTS

;; A XFT example (requires artwiz fonts)
;;(setq wmii-font"xft:smoothansi-14")

;; A "regular" font specification (requires Terminus)
;;(setq wmii-font "-xos4-terminus-medium-r-normal--16-*-72-72-c-80-iso10646-1")

;; A safer default
(setq wmii-font "-dec-terminal-medium-r-normal--14-140-75-75-c-80-iso8859-1")

;; Active the change
(wmii-set-font wmii-font)

;;; PROGRAMS

;; Change the default term
(setq wmmi-term "urxvt")

;; Add some arguments
(setq wmii-term-args "-bg grey10 -fg grey80 +sb -fn -dec-terminal-medium-r-normal--14-140-75-75-c-80-iso8859-1")

;;; LAYOUT

;; Bar on top
(wmii-xwrite "/ctl" "bar on top")

;;; BUTTONS

;;; For each button created follows a function used to fill in the
;;; information (when applicable), and also the code that sends that
;;; information to the bar (mostly when timers are used)

;; For the date field
(wmii-create-button (wmii-color-tuple "ghost white" "grey10" "grey10") "rbar" "zdate")

(defun wmii-button-date ()
  "Date button for wmii bar"
    (format-time-string "%a %e %b %H:%M" (current-time)))

;; Update it every 10 secs
(wmii-write-to-button-with-timer "rbar" "zdate" 'wmii-button-date 10 t)

;;(wmii-create-button (wmii-color-tuple "cyan3" "grey10" "grey10") "rbar" "status")

;; The notice button, where most alerts can be directed to
(wmii-create-button (wmii-color-tuple "cyan2" "grey10" "grey10") "rbar" "!notice")

;; Shows all events received by wmii; useful for debugging but who
;; doesn't like to to watch hexadecimal numbers fly by?
(wmii-create-button (wmii-color-tuple "grey60" "grey10" "grey10") "rbar" "wmiievents")

;; See belor for an explanation of the wmii-event-hook
(add-hook 'wmii-event-hook (lambda (ev)
			     (let ((event-type (car (split-string ev)))
				   (event-data (cdr (split-string ev))))
			       (wmii-write-to-button "rbar" "wmiievents" ev))))

;; Battery status
(wmii-create-button (wmii-color-tuple "green2" "grey10" "grey10") "rbar" "ybat")

(require 'battery)
(defun wmii-button-battery ()
  (battery-format "%p%% (%tm) %L %dC" (funcall battery-status-function)))

;; Update every 10 secs; this can be avoided by using something like dbus
(wmii-write-to-button-with-timer "rbar" "ybat" 'wmii-button-battery 10 t)

;; Volume status
(wmii-create-button (wmii-color-tuple "gold" "grey10" "grey10") "rbar" "yvolume")

;; Volume requires EMMS... and so this shouldn't exist as a keybinding
;; in the main file, FIXME
;;
;; Something like this should be done to load EMMS

;; (require 'emms-setup)
;; (require 'emms-volume)
;; (emms-standard)
;; (emms-default-players)

(defun wmii-change-volume (&optional lower)
  (if lower
      (wmii-write-to-button "rbar" "yvolume" (format "VOL %s" (caddr (split-string (emms-volume-lower)))))
    (wmii-write-to-button "rbar" "yvolume" (format "VOL %s" (caddr (split-string (emms-volume-raise)))))))

;; Network status
;;
;; This one depends a lot on each specific environment; I am using
;; a custom-made Emacs Lisp mode which taps into dbus to get
;; NetworkManager notifications (included in the contrib/ directory)
;;
(wmii-create-button (wmii-color-tuple "gold" "grey10" "grey10") "rbar" "xxnet")

;; (defun wmii-button-net ()
;;   ;; FIXME: there is no reason apart from laziness to keep the nmcli
;;   ;; invocation here instead of dbus...
;;   (chomp (shell-command-to-string "nmcli -t --fields NAME,DEVICE con  show --active")))

;; (defun nm-state-changed-handler (new old reason)
;;   (setq nm-mode-line-string (format "[%s]" (wmii-button-net)))
;;   (wmii-write-to-button "rbar" "xxnet" (wmii-button-net)))

;;; EXTRA

;; rcirc integration; send last message to !notice; timer used to
;; blank it after 10 seconds.
;; Something similar can be used for ERC, Circe and others, obviously

(defun wmii-rcirc-notification (proc sender response target text)
  (wmii-write-to-button-with-timer "rbar" "!notice" (format "%s: %s" sender text) 10))

(add-hook 'rcirc-print-hooks 'wmii-rcirc-notification)

;; Adding event hooks Every event received by wmii is passed to the
;; functions in wmii-event-hook; we can take advantage of that to add
;; all sorts of additional functions.
;;
;; Here we turn on transparency for every created client; this could
;; be changed to limit it to only certain clients, etc.

(add-hook 'wmii-event-hook (lambda (ev)
			     (let ((event-type (car (split-string ev)))
				   (client (cadr (split-string ev))))
			       (when (equal (downcase event-type) "createclient")
				 (wmii-transparency-toggle client)))))

;;; INITIALISATION
;; This actually launches the script - although most things can be
;; changed at any time even after launch, of course.

(wmii-init)
