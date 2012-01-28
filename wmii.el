;;; wmii.el --- elisp controller for the wmii window-manager
;;
;;  Copyright (C) 2010 Frederico Muñoz <fsmunoz@gmail.com>
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


;;; Code:

(provide 'wmii)

;;; Global variables

(defvar wmiir nil "wmiir process")
(defvar wmii-term "xterm" "Default terminal emulator used by wmii")
(defvar wmii-event-hook nil)
(defvar wmii-focus-colors)
(defvar wmii-normal-colors)
(defvar wmii-font)
(defvar wmii-programs nil "List of all executables in path")
(defvar wmii-initial-view "1" "Name of the initial view")
(defvar wmii-key-action-alist
  '(("Mod4-Right" . (wmii-client-select "right"))
    ("Mod4-Left" . (wmii-client-select "left"))
    ("Mod4-Up" . (wmii-client-select "up"))
    ("Mod4-Down" . (wmii-client-select "down"))
    ("Mod4-d" . (wmii-set-colmode "sel" "sel" "default"))
    ("Mod4-s" . (wmii-set-colmode "sel" "sel" "stack-max"))
    ("Mod4-m" . (wmii-set-colmode "sel" "sel" "stack+max"))
    ("Mod4-space" . (wmii-toggle-layers))
    ("Mod4-Shift-Right" . (wmii-client-move "right"))
    ("Mod4-Shift-Left". (wmii-client-move "left"))
    ("Mod4-Shift-Up" . (wmii-client-move "up"))
    ("Mod4-Shift-Down" . (wmii-client-move "down"))
    ("Mod4-Shift-space" . (wmii-client-toggle-layer))
    ("Mod4-f" . (wmii-client-toggle-fullscreen))
    ("Mod4-Shift-k" . (wmii-client-close))
    ("Mod4-x" . (wmii-wimenu wmii-programs 'wmii-wimenu-process-filter-exec))
    ("Mod4-Shift-x" . (wmii-wimenu (sort (all-completions "" obarray 'fboundp) 'string-lessp) 'wmii-wimenu-process-filter-eval))
    ("Mod4-Return" . (wmii-execute wmii-term))
    ("Mod4-t" . (wmii-wimenu (wmii-tags) 'wmii-wimenu-process-filter-move-by-tag))
    ("Mod4-Shift-t" . (wmii-wimenu (wmii-tags) 'wmii-wimenu-process-filter-retag-client))
    ("Mod4-1" . (wmii-move-to-numbered-view 1))
    ("Mod4-2" . (wmii-move-to-numbered-view 2))
    ("Mod4-3" . (wmii-move-to-numbered-view 3))
    ("Mod4-4" . (wmii-move-to-numbered-view 4))
    ("Mod4-5" . (wmii-move-to-numbered-view 5))
    ("Mod4-6" . (wmii-move-to-numbered-view 6))
    ("Mod4-7" . (wmii-move-to-numbered-view 7))
    ("Mod4-8" . (wmii-move-to-numbered-view 8))
    ("Mod4-9" . (wmii-move-to-numbered-view 9))
    ("Mod4-0" . (wmii-move-to-numbered-view 0))
    ("Mod4-Shift-1" . (wmii-client-numbered-retag 1))
    ("Mod4-Shift-2" . (wmii-client-numbered-retag 2))
    ("Mod4-Shift-3" . (wmii-client-numbered-retag 3))
    ("Mod4-Shift-4" . (wmii-client-numbered-retag 4))
    ("Mod4-Shift-5" . (wmii-client-numbered-retag 5))
    ("Mod4-Shift-6" . (wmii-client-numbered-retag 6))
    ("Mod4-Shift-7" . (wmii-client-numbered-retag 7))
    ("Mod4-Shift-8" . (wmii-client-numbered-retag 8))
    ("Mod4-Shift-9" . (wmii-client-numbered-retag 9))
    ("Mod4-Shift-0" . (wmii-client-numbered-retag 0)))
    "Key-action alist composed of keybindings (in wmii notation) and an expression that is to be evaluated")

;;; Primitives

(defun wmii-read (file)
  (process-lines "wmiir" "read" file))

(defun wmii-list (dir)
  (process-lines "wmiir" "ls" dir))

(defun wmii-xwrite (file contents)
  (shell-command-to-string (format "wmiir xwrite %s %s" file contents)))

(defun wmii-write (file contents)
  (let ((wmiir (start-process "wmiir" nil "wmiir" "write" file))
        (content-lines (combine-and-quote-strings contents "\n")))
    (process-send-string wmiir content-lines)
    ;; For some reason two are needed or the process doesn't exit
    (process-send-eof wmiir)
    (process-send-eof wmiir)
    content-lines))

(defun wmii-create (file contents)
    (shell-command-to-string (format "echo %s | wmiir create %s" contents file)))

(defun wmii-remove (file)
    (shell-command-to-string (format "wmiir remove %s" file)))

;;; Path-related functions (mainly used to build lists to pass to wimenu)

(defun wmii-directory-executable-files (path)
  "Like directory-files, but only returns non-special executable files"
  (remove-if-not (lambda (file) 
                   (and (file-executable-p file)
                        (file-regular-p file)))
                 (directory-files path t)))

(defun wmii-strip-directory (files)
  "Removes the directory path component from each element in FILES, which should be a list of files with complete path."
  (mapcar (lambda (file)
            (file-name-nondirectory file))
          files))

(defun wmii-make-executable-list (paths)
  (remove-duplicates 
   (mapcan (lambda (path)
             (wmii-strip-directory (wmii-directory-executable-files path)))
           paths)) )

(defun wmii-make-executable-list-fullpath (paths)
  (mapcan (lambda (path)
            (wmii-directory-executable-files path))
          paths))

;;; wimenu main function, and process filters used by it

(defun wmii-wimenu-process-filter-exec (process output)
  (start-process output nil output))

(defun wmii-wimenu-process-filter-eval (process output)
  (eval (read (concat "(" output ")"))))

(defun wmii-wimenu-process-filter-move-by-tag (process output)
  (wmii-xwrite "/ctl" (format "view %s" output)))

(defun wmii-wimenu-process-filter-retag-client (process output)
  (wmii-xwrite (format "/client/%s/tags" (wmii-selected-client)) output))


(defun wmii-wimenu (commands &optional filter)
  (let ((wimenu (start-process "wimenu" nil "wimenu"))
        (input (combine-and-quote-strings commands "\n")))
    (set-process-filter wimenu filter)
    (process-send-string wimenu input)
    ;; For some reason THREE! are needed or the process doesn't exit
    (process-send-eof wimenu)
    (process-send-eof wimenu)
    (process-send-eof wimenu)))

;;; Misc. utilities

(defun wmii-selected-tag ()
  (car (wmii-read "/tag/sel/ctl")))

(defun wmii-selected-client ()
  (find-if (lambda (elt)
             (string-match "^0x" elt))
           (wmii-read "/client/sel/ctl")))

(defun wmii-tags ()
  (remove-if (lambda (elt)
               (string-equal "sel" elt))
             (mapcar 'directory-file-name (wmii-list "/tag"))))

(defun wmii-move-to-numbered-view (view)
  (wmii-xwrite "/ctl" (format "view %d" view)))

(defun wmii-client-numbered-retag (tag)
  (wmii-xwrite "/client/sel/tags" tag))

(defun wmii-execute (program)
  (start-process program nil program))

(defun wmii-create-notice-bar (fg bg border)
  (wmii-create "/rbar/!notice" (wmii-color-tuple fg bg border)))

(defun wmii-create-status-bar (fg bg border)
  (wmii-create "/rbar/status" (wmii-color-tuple fg bg border)))

(defun wmii-set-border (width)
  (wmii-xwrite "/ctl" (format "border %s" width)))

(defun wmii-set-colmode (view col colmode)
  (wmii-xwrite (format "/tag/%s/ctl" view)
               (format "colmode %s %s" col colmode)))

(defun wmii-client-select (direction)
  (wmii-xwrite "/tag/sel/ctl" (format "select %s" direction)))

(defun wmii-client-move (direction)
  (wmii-xwrite "/tag/sel/ctl" (format "send sel %s" direction)))

(defun wmii-client-toggle-layer ()
  (wmii-xwrite "/tag/sel/ctl" "send sel toggle"))

(defun wmii-client-toggle-fullscreen ()
  (wmii-xwrite "/client/sel/ctl" "Fullscreen toggle"))

(defun wmii-toggle-layers ()
  (wmii-xwrite "/tag/sel/ctl" "select toggle"))

(defun wmii-client-close ()
  (wmii-xwrite "/client/sel/ctl" "kill"))

;;; Functions related to colours and fonts

(defun wmii-color-value (color-name)
  "Returns the RGB value of the color in hex, prepended with #"
  (format "#%s" (mapconcat (lambda (c)
                             (format "%02x" (/ c 257)))
                           (color-values color-name) "")))

(defun wmii-color-tuple (fg bg border)
  (format "%s %s %s"
          (wmii-color-value fg)
          (wmii-color-value bg)
          (wmii-color-value border)))
  
(defun wmii-set-focus-colors (tuple)
  (wmii-xwrite "/ctl" (format "focuscolors %S" tuple))
  tuple)

(defun wmii-set-normal-colors (tuple)
  (wmii-xwrite "/ctl" (format "normcolors %S" tuple))
    tuple)

(defun wmii-set-font (font)
  (wmii-xwrite "/ctl" (format "font %s" font)))

;;; Main event related functions

(defun wmii-event-filter (process output)
  "Filter that captures the OUTPUT of the wmiir PROCESS; each event is isolated and passed to the wmii-process-event function"
  (mapc 'wmii-process-event
        (split-string(format "%s" output) "\n" t)))

;; not used for now
;; (defun wmii-process-event-clientfocus (data)
;;   (message (format "Client Focus data: %s " data)))

;; (defun wmii-process-event-areafocus (data)
;;   (message (format " Focus data: %s " data)))

(defun wmii-process-event-createtag (data)
  (let ((tag (car data)))
    (wmii-create (format "/lbar/%s" tag) tag)
    tag))

(defun wmii-process-event-destroytag (data)
  (let ((tag (car data)))
    (wmii-remove (format "/lbar/%s" tag))
    tag))

(defun wmii-process-event-focustag (data)
  (let ((tag (car data)))
    (wmii-xwrite (format "/lbar/%s" tag) (format "%S %s" wmii-focus-colors tag))
    tag))

(defun wmii-process-event-unfocustag (data)
  (let ((tag (car data)))
    (wmii-xwrite (format "/lbar/%s" tag) (format "%S %s" wmii-normal-colors tag))
    tag))

(defun wmii-process-event-leftbarclick (data)
  (let ((tag (second data))
        (button (first data)))
    (wmii-xwrite "/ctl" (format "view %s" tag))))

(defun wmii-process-event-key (data)
  (let* ((key (car data))
         (action (cdr (assoc key wmii-key-action-alist))))
    (eval action)))

(defun wmii-event-loop ()
  "Starts the wmiir process and creates the filter that will capture the output"
  (let ((wmiir (start-process "wmiir" nil "wmiir" "read" "/event")))
    (set-process-filter wmiir 'wmii-event-filter)
    wmiir))

;;; Keybindings and key event processing

(defun wmii-defined-keys ()
  (wmii-read "/keys"))

(defun wmii-add-keybindings ()
  "Add all the keybindings defined in the wmii-key-action-alist"
  (let ((keys (mapcar (lambda (ka) (car ka)) wmii-key-action-alist)))
    (wmii-write "/keys" keys)))

(defun wmii-process-event (ev)
  (let* ((event-type (car (split-string ev)))
         (event-data (cdr (split-string ev)))
         (event-function (intern (format "wmii-process-event-%s" (downcase event-type)))))
    (if (fboundp event-function)
        (funcall event-function event-data))
    (run-hook-with-args 'wmii-event-hook ev)))

;;; Functions used for initialisation

(defun wmii-setup-tag-bar ()
  (let ((sel-tag (wmii-selected-tag)))
    (mapc (lambda (tag)
            (if (string-equal sel-tag tag)
                (progn
                  (wmii-xwrite "/event" (format "CreateTag %s" tag))
                  (wmii-xwrite "/event" (format "FocusTag %s" tag)))
              (wmii-xwrite "/event" (format "CreateTag %s" tag))))
   (wmii-tags))))

(defun wmii-init ()
  "Synthetise the initial tag events, add keybindings and start the event loop"
  (when (or (not (processp wmiir))
            (not (eq (process-status wmiir) 'run)))
    (wmii-set-normal-colors wmii-normal-colors)
    (wmii-set-focus-colors wmii-focus-colors)
    (wmii-set-font wmii-font)
    (setq wmii-programs (wmii-make-executable-list exec-path))
    (setq wmiir (wmii-event-loop))
    (wmii-xwrite "/event" (format "CreateTag %s" wmii-initial-view))
    (wmii-xwrite "/event" (format "FocusTag %s" wmii-initial-view))
    (wmii-setup-tag-bar)
    (wmii-add-keybindings)))
