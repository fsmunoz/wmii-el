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
;;; TODO: change to defcustom and use the customize package

(defvar wmiir nil "wmiir process")
(defvar wmii-term "xterm" "Default terminal emulator used by wmii")
(defvar wmii-term-args nil "Default arguments passed to the terminal emulator")
(defvar wmii-lock-command "xtrlock" "Lock command")
(defvar wmii-event-hook nil "List of functions called each time an event is processed")
(defvar wmii-focus-colors "Focus colors")
(defvar wmii-normal-colors "Normal colors")
(defvar wmii-bar-colors "Colors used in the status bar")
(defvar wmii-urgent-colors "Colors used in the status bar for urgent notification")
(defvar wmii-font "Used font")
(defvar wmii-programs nil "List of all executables in path")
(defvar wmii-initial-view "1" "Name of the initial view")
(defvar wmii-client-menu-entries 
      '(("Kill" . wmii-client-kill)
	("Slay" . wmii-client-slay)
	("Fullscreen" . wmii-client-toggle-fullscreen)
	("Control" . wmii-client-state-popup)
	)
      "Popup and actions used for click events on the clients")

;; WIP, partially due to a buf in wmii (patch sent)
(defvar wmii-clients-ring (make-ring 3) "Client ring (for cycling)")
(defvar wmii-active-window nil "Active window")

(defvar wmii-key-action-alist
  '(("Mod4-Right" . (wmii-client-select-by-direction "right"))
    ("Mod4-Left" . (wmii-client-select-by-direction "left"))
    ("Mod4-Up" . (wmii-client-select-by-direction "up"))
    ("Mod4-Down" . (wmii-client-select-by-direction "down"))
    ("Mod4-d" . (wmii-set-colmode "sel" "sel" "default"))
    ("Mod4-s" . (wmii-set-colmode "sel" "sel" "stack-max"))
    ("Mod4-m" . (wmii-set-colmode "sel" "sel" "stack+max"))
    ("Mod4-space" . (wmii-toggle-layers))
    ("Mod4-XF86Forward" . (wmii-next-view))
    ("Mod4-XF86Back". (wmii-previous-view))
    ("Mod4-Shift-Right" . (wmii-client-move "right"))
    ("Mod4-Shift-Left". (wmii-client-move "left"))
    ("Mod4-Shift-Up" . (wmii-client-move "up"))
    ("Mod4-Shift-Down" . (wmii-client-move "down"))
    ("Mod4-Shift-space" . (wmii-client-toggle-layer))
    ("Mod4-e" . (wmii-focus-emacs))
    ("Mod4-q" .  (wmii-wimenu (wmii-client-names) 'wmii-wimenu-process-filter-ask-and-goto-client))
    ("Mod4-r" . (wmii-transparency-toggle))
    ("Mod4-Control-Up" . (wmii-transparency-increase))   ; requires xcompmgr our similar
    ("Mod4-Control-Down" . (wmii-transparency-decrease)) ; idem
    ("Mod4-Tab" . (wmii-goto-previous-client))    
    ("Mod4-f" . (wmii-client-toggle-fullscreen))
    ("Mod4-Shift-k" . (wmii-client-kill))
    ("Mod4-x" . (wmii-wimenu wmii-programs 'wmii-wimenu-process-filter-exec))
    ("Mod4-Shift-x" . (wmii-wimenu (sort (all-completions "" obarray 'fboundp) 'string-lessp) 'wmii-wimenu-process-filter-eval))
    ("Mod4-Return" . (wmii-execute wmii-term wmii-term-args))
    ("Mod4-t" . (wmii-wimenu (wmii-tags) 'wmii-wimenu-process-filter-move-by-tag))
    ("Mod4-Shift-t" . (wmii-wimenu (wmii-tags) 'wmii-wimenu-process-filter-retag-client))
    ("Mod4-1" . (wmii-goto-view 1))
    ("Mod4-2" . (wmii-goto-view 2))
    ("Mod4-3" . (wmii-goto-view 3))
    ("Mod4-4" . (wmii-goto-view 4))
    ("Mod4-5" . (wmii-goto-view 5))
    ("Mod4-6" . (wmii-goto-view 6))
    ("Mod4-7" . (wmii-goto-view 7))
    ("Mod4-8" . (wmii-goto-view 8))
    ("Mod4-9" . (wmii-goto-view 9))
    ("Mod4-0" . (wmii-goto-view 0))
    ("Mod4-Shift-1" . (wmii-client-numbered-retag 1))
    ("Mod4-Shift-2" . (wmii-client-numbered-retag 2))
    ("Mod4-Shift-3" . (wmii-client-numbered-retag 3))
    ("Mod4-Shift-4" . (wmii-client-numbered-retag 4))
    ("Mod4-Shift-5" . (wmii-client-numbered-retag 5))
    ("Mod4-Shift-6" . (wmii-client-numbered-retag 6))
    ("Mod4-Shift-7" . (wmii-client-numbered-retag 7))
    ("Mod4-Shift-8" . (wmii-client-numbered-retag 8))
    ("Mod4-Shift-9" . (wmii-client-numbered-retag 9))
    ("Mod4-Shift-0" . (wmii-client-numbered-retag 0))
    ("XF86AudioRaiseVolume" . (wmii-change-volume))
    ("XF86AudioLowerVolume" . (wmii-change-volume t))
    ("XF86Sleep" . (wmii-sleep-system))
    ("Print" . (wmii-execute "gnome-screenshot" "-i"))
    ("XF86ScreenSaver" . (wmii-execute wmii-lock-command)))
    "Key-action alist composed of keybindings (in wmii notation) and an expression that is to be evaluated")


;;; Utility functions

;; This must be the 1x10^14 chomp function defined... if there is
;; something already in Emacs then I have missed it
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

;;; wmii primitives

(defun wmii-read (file)
  "Reads FILE from the wmii filesystem"
  (process-lines "wmiir" "read" file))

(defun wmii-list (dir)
  "Lists file at DIR of the wmii filesystem"
  (process-lines "wmiir" "ls" dir))

(defun wmii-xwrite (file content)
  "Writes CONTENT to FILE in the wmii filesystem; this changes a
specific value in the file and does not replace all the content."
  (shell-command-to-string (format "wmiir xwrite %s %S" file content)))

(defun wmii-write (file content)
  "Write CONTENT to FILE; this replaces all the content in the
file (if any)."
  (let ((wmiir (start-process "wmiir" nil "wmiir" "write" file))
        (content-lines (combine-and-quote-strings content "\n")))
    (process-send-string wmiir content-lines)
    ;; For some reason two are needed or the process doesn't exit
    (process-send-eof wmiir)
    (process-send-eof wmiir)
    content-lines))

(defun wmii-create (file content)
  "Creates a new FILE with CONTENT"
    (shell-command-to-string (format "echo %s | wmiir create %s" content file)))

(defun wmii-remove (file)
  "Removes FILE"
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
	     (when (file-exists-p path)
             (wmii-strip-directory (wmii-directory-executable-files path))))
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

(defun wmii-wimenu-process-filter-ask-and-goto-client (process output)
  (wmii-focus-client (wmii-find-client-by-prop output)))


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

(defun wmii-goto-view (view)
  (wmii-xwrite "/ctl" (format "view %s" view)))

(defun wmii-next-view ()
  (wmii-goto-view (nth 1 (member (wmii-selected-tag) (wmii-tags)))))

(defun wmii-next-view ()
  (let* ((selected (wmii-selected-tag))
	 (tags (wmii-tags))
	 (target (nth 
		  (1+ (position selected tags :test 'equal )) tags)))
    (if target
	(wmii-goto-view target)
      (wmii-goto-view (first tags)))))

(defun wmii-previous-view ()
  (let* ((selected (wmii-selected-tag))
	 (tags (wmii-tags))
	 (target (nth 
		  (1- (position selected tags :test 'equal )) tags)))
    (if (not (equal selected target))
	(wmii-goto-view target)
      (wmii-goto-view (car (last tags))))))

(defun wmii-client-numbered-retag (tag)
  (wmii-xwrite "/client/sel/tags" tag))

(defun wmii-execute (program &optional args)
  (cond
   ((listp args)
    (apply 'start-process program nil program args))
   ((stringp args)
    (apply 'start-process program nil program (split-string args)))
   (t
    (apply 'start-process program nil program))))

(defun wmii-create-notice-bar (fg bg border)
  (wmii-create "/rbar/!notice" (wmii-color-tuple fg bg border)))

(defun wmii-create-status-bar (fg bg border)
  (wmii-create "/rbar/status" (wmii-color-tuple fg bg border)))

(defun wmii-set-border (width)
  (wmii-xwrite "/ctl" (format "border %s" width)))

(defun wmii-set-colmode (view col colmode)
  (wmii-xwrite (format "/tag/%s/ctl" view)
               (format "colmode %s %s" col colmode)))

(defun wmii-client-select (client)
  (wmii-xwrite "/tag/sel/ctl" (format "select client %s" client)))

(defun wmii-client-select-by-direction (direction)
  (wmii-xwrite "/tag/sel/ctl" (format "select %s" direction)))

(defun wmii-client-move (direction)
  (wmii-xwrite "/tag/sel/ctl" (format "send sel %s" direction)))

(defun wmii-client-toggle-layer ()
  (wmii-xwrite "/tag/sel/ctl" "send sel toggle"))

(defun wmii-client-toggle-fullscreen ()
  (wmii-xwrite "/client/sel/ctl" "Fullscreen toggle"))

(defun wmii-toggle-layers ()
  (wmii-xwrite "/tag/sel/ctl" "select toggle"))

(defun wmii-client-kill ()
  (wmii-xwrite "/client/sel/ctl" "kill"))

(defun wmii-client-slay ()
  (wmii-xwrite "/client/sel/ctl" "slay"))

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
  (wmii-xwrite "/ctl" (format "focuscolors %s" tuple))
  tuple)

(defun wmii-set-normal-colors (tuple)
  (wmii-xwrite "/ctl" (format "normcolors %s" tuple))
    tuple)

(defun wmii-set-urgent-colors (tuple)
  (wmii-xwrite "/ctl" (format "urgcolors %s" tuple))
    tuple)

(defun wmii-set-font (font)
  (wmii-xwrite "/ctl" (format "font %s" font)))

;;; Main event related functions

(defun wmii-event-filter (process output)
  "Filter that captures the OUTPUT of the wmiir PROCESS; each event is isolated and passed to the wmii-process-event function"
  (mapc 'wmii-process-event
        (split-string(format "%s" output) "\n" t)))

;; not used for now
;; (defun wmii-process-event-areafocus (data)
;;   (message (format " Focus data: %s " data)))

(defun wmii-process-event-clientfocus (data)
  (let ((client (car data)))
    (unless (equal client "<nil>")
      (ring-insert wmii-clients-ring client))))

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
    (wmii-xwrite (format "/lbar/%s" tag) (format "%s %s" wmii-focus-colors tag))
    tag))

(defun wmii-process-event-unfocustag (data)
  (let ((tag (car data)))
    (wmii-xwrite (format "/lbar/%s" tag) (format "%s %s" wmii-bar-colors tag))
    tag))

(defun wmii-process-event-leftbarclick (data)
  (let ((tag (second data))
        (button (first data)))
    (wmii-xwrite "/ctl" (format "view %s" tag))))

(defun wmii-process-event-clientmousedown (data &optional entries)
  (let ((client (car data))
	(button (string-to-int (cadr data)))
	(entries (or entries wmii-client-menu-entries)))
    (when (eq button 3)
      (wmii-9menu entries))))

(defun wmii-process-event-leftbarclick (data)
  (let ((mouseb (string-to-int (car data)))
	(button (cadr data)))
    (if (eq mouseb 4)
	(wmii-next-view)
      (wmii-previous-view))))

(defun wmii-process-event-clientclick (data &optional entries)
  (let ((client (car data))
	(button (string-to-int (cadr data)))
	(entries (or entries wmii-client-menu-entries)))
    (when (eq button 4)
      (wmii-client-select-by-direction "up"))
    (when (eq button 5)
      (wmii-client-select-by-direction "down"))))

(defun wmii-process-event-urgenttag (data)
  (let ((type (car data))
	(tag  (cadr data)))
    (unless (equal tag (wmii-selected-tag)) ;; only mark view if it's not the current one.
      (wmii-xwrite (format "/lbar/%s" tag) (format "%s %s" wmii-urgent-colors tag))
    tag)))

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

;;; Buttons

(defun wmii-create-button (tuple bar label)
  (wmii-create (format "/%s/%s" bar label) (format "%S " tuple label)))

;; TODO: this two require some more thought, they exist for very
;; specific cases and are less than ideal.
(defun wmii-write-to-button (bar button message)
  (if (symbolp message)
      (wmii-xwrite (format "/%s/%s" bar button) (funcall message))
    (wmii-xwrite (format "/%s/%s" bar button) message)))

(defun wmii-write-to-button-with-timer (bar button message &optional secs repeat)
  (let ((secs (or secs 5)))
    (wmii-write-to-button bar button message)
    (if repeat
	(run-with-timer secs secs  'wmii-write-to-button bar button message)
      (run-with-timer secs nil 'wmii-write-to-button bar button ""))))

;;; Transparency - depends on "transset" binary...

(defun wmii-transparency-toggle (&optional window)
  (let ((window (or window (wmii-selected-client))))
    (wmii-execute "transset" (format "-i %s -t" window))))

(defun wmii-transparency-increase (&optional window amount)
  (let ((window (or window (wmii-selected-client))))
    (wmii-execute "transset" (format "-i %s --dec 0.1" window))))

(defun wmii-transparency-decrease (&optional window amount)
  (let ((window (or window (wmii-selected-client))))
    (wmii-execute "transset" (format "-i %s --inc 0.1" window))))

;; Window cycling - stil wip

(defun wmii-goto-previous-client ()
  (wmii-focus-client (ring-ref wmii-clients-ring 1)))

;; Misc.

(defun wmii-popup (message)
  "Shows a popup message. Uses wmii9menu but doesn't care about any return value"
  (wmii-execute "wmii9menu" message))

(defun wmii-client-state ()
  (wmii-read "/client/sel/ctl"))

(defun wmii-client-state-popup ()
  (wmii-popup (wmii-client-state)))

(defun wmii-9menu (entries)
  "Uses assoc lit to pass to wmii9mene. The ENTRIES are an
association list composed of (LABEL . COMMAND) cons; if COMMAND
is a string it will be executed as an external shell command, if it's a symbol it will be evaluate as a Lisp sexp"
  (let* ((entries (mapconcat (lambda (cons)
			       (if (symbolp (cdr cons))
				   (format "%s:\\'%s" (car cons) (symbol-name (cdr cons)))
				 (format "%s:%s" (car cons) (cdr cons))))
			     entries
			     " "))
	 (cmd (chomp (shell-command-to-string (format "wmii9menu -- %s" entries)))))
    (when (not (equal cmd ""))
      (if (string-match "^'" cmd)
	  (funcall (intern (substring cmd 1)))
	(wmii-execute cmd)))))

(defun wmii-client-list ()
  "Returns all the clients"
  (let ((clients (remove "sel/" (wmii-list "/client"))))
	    (mapcar (lambda (client)
		      (directory-file-name client))
		    clients )))

(defun wmii-clients-props ()
  "Returns all the clients id with props"
  (let ((clients (wmii-list "/client")))
	(mapcar (lambda (client)
		  (let ((props (wmii-read (format "/client/%s/props" client))))
		    (cons (directory-file-name client) props)))
		clients )))
(defun wmii-client-names ()
  (mapcar (lambda (elt)
	    (downcase (cadr elt)))
   (wmii-clients-props)))

(defun wmii-find-client-by-prop (prop)
  (loop for client in (wmii-clients-props) if (string-match prop (downcase (cadr client))) return (car client)))

(defun wmii-tag-windows (tag)
  "Returns a list of windows in TAG; result is an association list"
  (let ((window-list (wmii-read (format "/tag/%s/index" tag))))
    (remq nil
    (mapcar (lambda (elt)
	      (let ((props (split-string elt)))
		(if (not (equal (car props) "#"))
		    (cons (nth 1 props) (nth 4 props)))))
     
     window-list))))

(defun wmii-client-tags (client)
  "Returns the tag associated with a client"
  (let ((tags (car (wmii-read (format "/client/%s/tags" client)))))
    (split-string tags  "+")))

(defun wmii-focus-client (&optional client)
  (let ((client (or client wmii-emacs-client-id)))
    (wmii-goto-view (car (wmii-client-tags client)))
    (wmii-client-select client)))

;; Important enough to have its own function :)
(defun wmii-focus-emacs ()
  (wmii-focus-client (wmii-find-client-by-prop "emacs")))


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
