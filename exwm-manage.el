;;; exwm-manage.el --- Window Management Module for  -*- lexical-binding: t -*-
;;;                    EXWM

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the fundamental module of EXWM that deals with window management.

;;; Code:

(require 'exwm-core)

(defgroup exwm-manage nil
  "Manage."
  :group 'exwm)

(defcustom exwm-manage-finish-hook nil
  "Normal hook run after a window is just managed.
This hook runs in the context of the corresponding `exwm-mode' buffer."
  :type 'hook)

(defcustom exwm-manage-force-tiling nil
  "Non-nil to force managing all X windows in tiling layout.
You can still make the X windows floating afterwards."
  :type 'boolean)

(defcustom exwm-manage-startup-id t
  "Non-nil to place windows using startup notifications.

A program Emacs starts is recorded against the workspace and window
that were current at launch.  If that workspace is still current when
the window appears, the window is shown there.  If you have switched
workspace, the window is placed on the workspace where it was started
and the workspace you are on is left as it is.  When the original
window no longer exists, the buffer is left on that workspace and the
selected window is not changed.

Launchers that send `_NET_STARTUP_INFO' can name a workspace with
DESKTOP.  That workspace is used when the program was not started by
Emacs.

Set this to nil to keep the old behavior: a new window opens on the
workspace that is current when it appears."
  :type 'boolean
  :initialize #'custom-initialize-default)

(defcustom exwm-manage-ping-timeout 3
  "Seconds to wait before killing a client."
  :type 'integer)

(defcustom exwm-manage-configurations nil
  "Per-application configurations.

Configuration options allow to override various default behaviors of EXWM
and only take effect when they are present.  Note for certain options
specifying nil is not exactly the same as leaving them out.  Currently
possible choices:
* floating: Force floating (non-nil) or tiling (nil) on startup.
* stay-tiled: Keep the window tiled.  A client request to float is ignored.
  `exwm-floating-set-floating' can still float it.
* x/y/width/height: Override the initial geometry (floating X window only).
* border-width: Override the border width (only visible when floating).
* fullscreen: Force full screen (non-nil) on startup.
* floating-mode-line: `mode-line-format' used when floating.
* tiling-mode-line: `mode-line-format' used when tiling.
* floating-header-line: `header-line-format' used when floating.
* tiling-header-line: `header-line-format' used when tiling.
* char-mode: Force char-mode (non-nil) on startup.
* prefix-keys: `exwm-input-prefix-keys' local to this X window.
* simulation-keys: `exwm-input-simulation-keys' local to this X window.
* workspace: The initial workspace.
* managed: Force to manage (non-nil) or not manage (nil) the X window.
* dont-steal-focus: Non-nil ignores `_NET_ACTIVE_WINDOW' from this X window.

For each X window managed for the first time, matching criteria (sexps) are
evaluated sequentially and the first configuration with a non-nil matching
criterion would be applied.  Apart from generic forms, one would typically
want to match against EXWM internal variables such as `exwm-title',
`exwm-class-name' and `exwm-instance-name'."
  :type '(alist :key-type (sexp :tag "Matching criterion" nil)
                :value-type
                (plist :tag "Configurations"
                       :options
                       (((const :tag "Floating" floating) boolean)
                        ((const :tag "Stay tiled" stay-tiled) boolean)
                        ((const :tag "X" x) number)
                        ((const :tag "Y" y) number)
                        ((const :tag "Width" width) number)
                        ((const :tag "Height" height) number)
                        ((const :tag "Border width" border-width) integer)
                        ((const :tag "Fullscreen" fullscreen) boolean)
                        ((const :tag "Floating mode-line" floating-mode-line)
                         sexp)
                        ((const :tag "Tiling mode-line" tiling-mode-line) sexp)
                        ((const :tag "Floating header-line"
                                floating-header-line)
                         sexp)
                        ((const :tag "Tiling header-line" tiling-header-line)
                         sexp)
                        ((const :tag "Char-mode" char-mode) boolean)
                        ((const :tag "Prefix keys" prefix-keys)
                         (repeat key-sequence))
                        ((const :tag "Simulation keys" simulation-keys)
                         (alist :key-type (key-sequence :tag "From")
                                :value-type (key-sequence :tag "To")))
                        ((const :tag "Workspace" workspace) integer)
                        ((const :tag "Managed" managed) boolean)
                        ((const :tag "Don't steal focus" dont-steal-focus)
                         boolean)
                        ;; For forward compatibility.
                        ((other) sexp))))
  ;; TODO: This is admittedly ugly.  We'd be better off with an event type.
  :get (lambda (symbol)
         (mapcar (lambda (pair)
                   (let* ((match (car pair))
                          (config (cdr pair))
                          (prefix-keys (plist-get config 'prefix-keys)))
                     (when prefix-keys
                       (setq config (copy-tree config)
                             config (plist-put config 'prefix-keys
                                               (mapcar (lambda (i)
                                                         (if (sequencep i)
                                                             i
                                                           (vector i)))
                                                       prefix-keys))))
                     (cons match config)))
                 (default-value symbol)))
  :set (lambda (symbol value)
         (set symbol
              (mapcar (lambda (pair)
                        (let* ((match (car pair))
                               (config (cdr pair))
                               (prefix-keys (plist-get config 'prefix-keys)))
                          (when prefix-keys
                            (setq config (copy-tree config)
                                  config (plist-put config 'prefix-keys
                                                    (mapcar (lambda (i)
                                                              (if (sequencep i)
                                                                  (aref i 0)
                                                                i))
                                                            prefix-keys))))
                          (cons match config)))
                      value))))

;; The _MOTIF_WM_HINTS atom (see <Xm/MwmUtil.h> for more details)
;; It's currently only used in 'exwm-manage' module
(defvar exwm-manage--_MOTIF_WM_HINTS nil "_MOTIF_WM_HINTS atom.")

(defvar exwm-manage--desktop nil "The desktop X window.")

(defvar exwm-manage--startup-records nil
  "Alist of startup notifications.
Each element is (ID WORKSPACE-INDEX WINDOW TIME).  ID is the
startup-notification string.  WINDOW is the Emacs window selected
when Emacs launched the program, or nil for an external launcher.
TIME is `float-time' when the record was created.")

(defvar exwm-manage--startup-sequence 0
  "Counter mixed into startup-notification ids.")

(defvar exwm-manage--startup-message nil
  "Startup-notification message still being assembled.")

(defvar exwm-manage--display-window nil
  "Window in which to place a client without selecting it.
Bound while managing a window whose startup workspace is no longer
current.")

(defvar exwm-manage--_NET_STARTUP_ID nil)
(defvar exwm-manage--_NET_STARTUP_INFO nil)
(defvar exwm-manage--_NET_STARTUP_INFO_BEGIN nil)
(defvar exwm-manage--wm-client-leader nil)

(defvar exwm-manage--frame-outer-id-list nil
  "List of window-outer-id's of all frames.")

(defvar exwm-input--skip-buffer-list-update)
(defvar exwm-input-prefix-keys)
(defvar exwm-workspace--current)
(defvar exwm-workspace--id-struts-alist)
(defvar exwm-workspace--list)
(defvar exwm-workspace--switch-history-outdated)
(defvar exwm-workspace-current-index)
(declare-function exwm--update-class "exwm.el" (id &optional force))
(declare-function exwm--update-hints "exwm.el" (id &optional force))
(declare-function exwm--update-normal-hints "exwm.el" (id &optional force))
(declare-function exwm--update-protocols "exwm.el" (id &optional force))
(declare-function exwm--update-struts "exwm.el" (id))
(declare-function exwm--update-title "exwm.el" (id))
(declare-function exwm--update-icon "exwm.el" (id &optional force))
(declare-function exwm--update-transient-for "exwm.el" (id &optional force))
(declare-function exwm--update-desktop "exwm.el" (id &optional force))
(declare-function exwm--update-window-type "exwm.el" (id &optional force))
(declare-function exwm-floating--set-floating "exwm-floating.el" (id))
(declare-function exwm-floating--unset-floating "exwm-floating.el" (id))
(declare-function exwm-input-grab-keyboard "exwm-input.el" (&optional id))
(declare-function exwm-input-release-keyboard "exwm-input.el" (&optional id))
(declare-function exwm-input-set-local-simulation-keys "exwm-input.el")
(declare-function exwm-layout--fullscreen-p "exwm-layout.el" ())
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout-set-fullscreen "exwm-layout.el" (&optional id))
(declare-function exwm-workspace--get-geometry "exwm-workspace.el" (frame))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))
(declare-function exwm-workspace--update-struts "exwm-workspace.el" ())
(declare-function exwm-workspace--update-workareas "exwm-workspace.el" ())
(declare-function exwm-workspace--workarea "exwm-workspace.el" (frame))
(declare-function exwm-workspace--active-p "exwm-workspace.el" (frame))
(declare-function exwm-workspace--raise-child-frames "exwm-workspace.el" ())
(declare-function exwm-layout--hide "exwm-layout.el" (id))

(defun exwm-manage-get-pid (&optional id)
  "Return the PID of the X window ID, if known.

If ID is unspecified, the PID of the current window is returned."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when-let* ((response
               (and id (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:ewmh:get-_NET_WM_PID
                                          :window id)))))
    (slot-value response 'value)))

(defun exwm-manage--update-geometry (id &optional force)
  "Update geometry of X window ID.
Override current geometry if FORCE is non-nil."
  (exwm--log "id=#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and exwm--geometry (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GetGeometry :drawable id))))
        (setq exwm--geometry
              (or reply
                  ;; Provide a reasonable fallback value.
                  (make-instance 'xcb:RECTANGLE
                                 :x 0
                                 :y 0
                                 :width (/ (x-display-pixel-width) 2)
                                 :height (/ (x-display-pixel-height) 2))))))))

(defun exwm-manage--update-ewmh-state (id)
  "Update _NET_WM_STATE of X window ID."
  (exwm--log "id=#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless exwm--ewmh-state
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:ewmh:get-_NET_WM_STATE
                                      :window id))))
        (when reply
          (setq exwm--ewmh-state (append (slot-value reply 'value) nil)))))))

(defun exwm-manage--update-mwm-hints (id &optional force)
  "Update _MOTIF_WM_HINTS of X window ID.
Override current hinds if FORCE is non-nil."
  (exwm--log "id=#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not exwm--mwm-hints-decorations) (not force))
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:-GetProperty
                                      :window id
                                      :property exwm-manage--_MOTIF_WM_HINTS
                                      :type exwm-manage--_MOTIF_WM_HINTS
                                      :long-length 5))))
        (when reply
          ;; Check MotifWmHints.decorations.
          (with-slots (value) reply
            (setq value (append value nil))
            (when (and value
                       ;; See <Xm/MwmUtil.h> for fields definitions.
                       (/= 0 (logand
                              (elt value 0) ;MotifWmHints.flags
                              2))           ;MWM_HINTS_DECORATIONS
                       (= 0
                          (elt value 2))) ;MotifWmHints.decorations
              (setq exwm--mwm-hints-decorations nil))))))))

(defun exwm-manage--update-default-directory (id)
  "Update the `default-directory' of X window ID.
Sets the `default-directory' of the EXWM buffer associated with X window to
match its current working directory.

This only works when procfs is mounted, which may not be the case on some BSDs."
  (with-current-buffer (exwm--id->buffer id)
    (if-let* ((pid (exwm-manage-get-pid))
              (cwd (file-symlink-p (format "/proc/%d/cwd" pid)))
              ((file-accessible-directory-p cwd)))
        (setq default-directory (file-name-as-directory cwd))
      (setq default-directory (expand-file-name "~/")))))

(defun exwm-manage--set-client-list ()
  "Set _NET_CLIENT_LIST."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST
                     :window exwm--root
                     :data (vconcat (mapcar #'car exwm--id-buffer-alist)))))

(cl-defun exwm-manage--get-configurations ()
  "Retrieve configurations for this buffer."
  (exwm--log)
  (when (derived-mode-p 'exwm-mode)
    (dolist (i exwm-manage-configurations)
      (save-current-buffer
        (when (with-demoted-errors "Problematic configuration: %S"
                (eval (car i) t))
          (cl-return-from exwm-manage--get-configurations (cdr i)))))))

(defun exwm-manage--startup-field (message key)
  "Return the value of KEY in startup-notification MESSAGE."
  (when (and (stringp message)
             (string-match
              (concat (regexp-quote key)
                      "=\\(?:\"\\([^\"]*\\)\"\\|\\([^[:space:]]+\\)\\)")
              message))
    (or (match-string 1 message) (match-string 2 message))))

(defun exwm-manage--expire-startup ()
  "Drop startup records older than five minutes."
  (let ((limit (- (float-time) 300))
        keep)
    (dolist (rec exwm-manage--startup-records)
      (when (>= (or (nth 3 rec) 0) limit)
        (push rec keep)))
    (setq exwm-manage--startup-records (nreverse keep))))

(defun exwm-manage--consume-startup-message (message)
  "Record or drop a complete startup-notification MESSAGE."
  (let ((id (exwm-manage--startup-field message "ID"))
        (desktop (exwm-manage--startup-field message "DESKTOP"))
        (index nil))
    (when (and desktop (string-match-p "\\`[0-9]+\\'" desktop))
      (setq index (string-to-number desktop)))
    (cond ((or (null id) (string-prefix-p "remove:" message))
           (when id
             (setq exwm-manage--startup-records
                   (assoc-delete-all id exwm-manage--startup-records))))
          ((or (string-prefix-p "new:" message)
               (string-prefix-p "change:" message))
           (let ((old (assoc id exwm-manage--startup-records)))
             (if old
                 (when index
                   (setcar (cdr old) index))
               (push (list id (or index exwm-workspace-current-index)
                           nil (float-time))
                     exwm-manage--startup-records)))))))

(defun exwm-manage--client-bytes (data)
  "Return the 20 bytes carried by client-message DATA."
  (let ((bytes (ignore-errors (slot-value data 'data8))))
    (if (and (sequencep bytes) (= (length bytes) 20))
        (append bytes nil)
      (let ((words (append (ignore-errors (slot-value data 'data32)) nil))
            out)
        (dolist (word words)
          (setq word (or word 0)
                out (nconc out (list (logand word #xff)
                                     (logand (ash word -8) #xff)
                                     (logand (ash word -16) #xff)
                                     (logand (ash word -24) #xff)))))
        out))))

(defun exwm-manage--bytes-to-string (value)
  "Decode a property or message VALUE into a string."
  (let ((text
         (cond ((not value) nil)
               ((stringp value)
                (substring value 0
                           (or (cl-position 0 value) (length value))))
               ((sequencep value)
                (let* ((bytes (append value nil))
                       (end (cl-position 0 bytes)))
                  (when end
                    (setq bytes (seq-take bytes end)))
                  (decode-coding-string
                   (apply #'unibyte-string bytes) 'utf-8 t))))))
    (and (stringp text) (> (length text) 0) text)))

(defun exwm-manage--on-startup-info (_window data begin)
  "Assemble a `_NET_STARTUP_INFO' message from DATA.
BEGIN non-nil starts a new message."
  (let* ((bytes (exwm-manage--client-bytes data))
         (end (cl-position 0 bytes))
         (chunk (exwm-manage--bytes-to-string
                 (if end (seq-take bytes end) bytes))))
    (when begin
      (setq exwm-manage--startup-message nil))
    (setq exwm-manage--startup-message
          (concat exwm-manage--startup-message chunk))
    (when end
      (exwm-manage--consume-startup-message exwm-manage--startup-message)
      (setq exwm-manage--startup-message nil))))

(defun exwm-manage--property-value (window atom)
  "Return the raw value of ATOM on WINDOW, or nil."
  (let ((reply (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:GetProperty
                                  :delete 0
                                  :window window
                                  :property atom
                                  :type xcb:GetPropertyType:Any
                                  :long-offset 0
                                  :long-length 256))))
    (when (and reply (> (or (slot-value reply 'value-len) 0) 0))
      (slot-value reply 'value))))

(defun exwm-manage--leader-window (window)
  "Return WM_CLIENT_LEADER of WINDOW, or nil."
  (let ((value (and exwm-manage--wm-client-leader
                    (exwm-manage--property-value
                     window exwm-manage--wm-client-leader))))
    (cond ((numberp value) value)
          ((and (sequencep value) (not (stringp value)) (> (length value) 0))
           (elt value 0)))))

(defun exwm-manage--read-startup-id (window)
  "Return the `_NET_STARTUP_ID' of WINDOW, checking its leader."
  (when exwm-manage--_NET_STARTUP_ID
    (or (exwm-manage--bytes-to-string
         (exwm-manage--property-value window exwm-manage--_NET_STARTUP_ID))
        (let ((leader (exwm-manage--leader-window window)))
          (when (and leader (/= leader 0) (/= leader window))
            (exwm-manage--bytes-to-string
             (exwm-manage--property-value
              leader exwm-manage--_NET_STARTUP_ID)))))))

(defun exwm-manage--startup-index (window)
  "Return (WORKSPACE-INDEX . EMACS-WINDOW) for WINDOW, or nil."
  (when exwm-manage-startup-id
    (exwm-manage--expire-startup)
    (let* ((sid (exwm-manage--read-startup-id window))
           (rec (and sid (assoc sid exwm-manage--startup-records)))
           (index (and rec (nth 1 rec))))
      (when (and (integerp index)
                 (<= 0 index)
                 (< index (length exwm-workspace--list)))
        (cons index (nth 2 rec))))))

(defun exwm-manage--send-startup-message (text)
  "Send startup-notification TEXT to the root window."
  (when (and exwm--connection
             exwm-manage--_NET_STARTUP_INFO
             exwm-manage--_NET_STARTUP_INFO_BEGIN)
    (let ((bytes (append (encode-coding-string text 'utf-8 t) (list 0)))
          (begin t))
      (while bytes
        (let* ((n (min 20 (length bytes)))
               (chunk (append (seq-take bytes n)
                              (make-list (- 20 n) 0))))
          (setq bytes (nthcdr n bytes))
          (xcb:+request exwm--connection
              (make-instance 'xcb:SendEvent
                             :propagate 0
                             :destination exwm--root
                             :event-mask xcb:EventMask:PropertyChange
                             :event (xcb:marshal
                                     (make-instance
                                      'xcb:ClientMessage
                                      :format 8
                                      :window exwm--root
                                      :type (if begin
                                                exwm-manage--_NET_STARTUP_INFO_BEGIN
                                              exwm-manage--_NET_STARTUP_INFO)
                                      :data (make-instance 'xcb:ClientMessageData
                                                           :data8 chunk))
                                     exwm--connection)))
          (setq begin nil)))
      (xcb:flush exwm--connection))))

(defun exwm-manage--finish-startup (window)
  "End WINDOW's startup notification, if it has one."
  (let ((sid (exwm-manage--read-startup-id window)))
    (when sid
      (setq exwm-manage--startup-records
            (assoc-delete-all sid exwm-manage--startup-records))
      (exwm-manage--send-startup-message (format "remove: ID=\"%s\"" sid)))))

(defun exwm-manage--new-startup-id ()
  "Return a new startup-notification id."
  (format "exwm-%s-%s_TIME%s"
          (emacs-pid)
          (setq exwm-manage--startup-sequence
                (1+ exwm-manage--startup-sequence))
          (floor (* (float-time) 1000))))

(defun exwm-manage--remember-startup (id)
  "Remember ID against the current workspace and selected window."
  (exwm-manage--expire-startup)
  (push (list id
              exwm-workspace-current-index
              (or (minibuffer-selected-window) (selected-window))
              (float-time))
        exwm-manage--startup-records))

(defun exwm-manage--startup-env-p (args)
  "Whether make-process ARGS already carry DESKTOP_STARTUP_ID."
  (let ((env (if (plist-member args :environment)
                 (plist-get args :environment)
               process-environment)))
    (catch 'found
      (dolist (entry env)
        (when (and (stringp entry)
                   (string-prefix-p "DESKTOP_STARTUP_ID=" entry))
          (throw 'found t))))))

(defun exwm-manage--startup-environment (args id)
  "Return the environment of ARGS with DESKTOP_STARTUP_ID set to ID."
  (let ((env (if (plist-member args :environment)
                 (plist-get args :environment)
               process-environment))
        cleaned)
    (dolist (entry env)
      (unless (and (stringp entry)
                   (string-prefix-p "DESKTOP_STARTUP_ID=" entry))
        (push entry cleaned)))
    (cons (concat "DESKTOP_STARTUP_ID=" id) (nreverse cleaned))))

(defun exwm-manage--startup-make-process (args)
  "Add a startup id to `make-process' ARGS when EXWM should track it."
  (if (not (and exwm-manage-startup-id
                exwm--connection
                (slot-value exwm--connection 'connected)
                (not (file-remote-p default-directory))
                (plist-get args :command)
                (not (exwm-manage--startup-env-p args))))
      args
    (let ((id (exwm-manage--new-startup-id))
          (args (copy-sequence args)))
      (exwm-manage--remember-startup id)
      (setq args (plist-put args :environment
                            (exwm-manage--startup-environment args id)))
      (exwm-manage--send-startup-message
       (format "new: ID=\"%s\" NAME=\"Emacs\" DESKTOP=%d"
               id exwm-workspace-current-index))
      args)))

(defun exwm-manage--manage-window (id)
  "Manage window ID."
  (exwm--log "Try to manage #x%x" id)
  (catch 'return
    ;; Ensure it's alive
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask (exwm--get-client-event-mask)))
      (throw 'return 'dead))
    ;; Add this X window to save-set.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeSaveSet
                       :mode xcb:SetMode:Insert
                       :window id))
    (with-current-buffer (let ((exwm-input--skip-buffer-list-update t))
                           (generate-new-buffer "*EXWM*"))
      ;; Keep the oldest X window first.
      (setq exwm--id-buffer-alist
            (nconc exwm--id-buffer-alist `((,id . ,(current-buffer)))))
      (exwm-mode)
      (setq exwm--id id
            exwm--frame exwm-workspace--current)
      (exwm--update-window-type id)
      (exwm--update-class id)
      (exwm--update-transient-for id)
      (exwm--update-normal-hints id)
      (exwm--update-hints id)
      (exwm-manage--update-geometry id)
      (exwm-manage--update-mwm-hints id)
      (exwm--update-title id)
      (exwm--update-icon id)
      (exwm--update-protocols id)
      (setq exwm--configurations (exwm-manage--get-configurations))
      ;; OverrideRedirect is not checked here.
      (when (and
             ;; The user has specified to manage it.
             (not (plist-get exwm--configurations 'managed))
             (or
              ;; The user has specified not to manage it.
              (plist-member exwm--configurations 'managed)
              ;; This is not a type of X window we can manage.
              (and exwm-window-type
                   (not (cl-intersection
                         exwm-window-type
                         (list xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                               xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                               xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL))))
              ;; Check the _MOTIF_WM_HINTS property to not manage floating X
              ;; windows without decoration.
              (and (not exwm--mwm-hints-decorations)
                   (not exwm--hints-input)
                   ;; Floating windows only
                   (or exwm-transient-for exwm--fixed-size
                       (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                             exwm-window-type)
                       (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                             exwm-window-type)))))
        (exwm--log "No need to manage #x%x" id)
        ;; Update struts.
        (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK exwm-window-type)
          (exwm--update-struts id))
        ;; Remove all events
        (xcb:+request exwm--connection
            (make-instance 'xcb:ChangeWindowAttributes
                           :window id :value-mask xcb:CW:EventMask
                           :event-mask
                           (if (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK
                                     exwm-window-type)
                               ;; Listen for PropertyChange (struts) and
                               ;; UnmapNotify/DestroyNotify event of the dock.
                               (exwm--get-client-event-mask)
                             xcb:EventMask:NoEvent)))
        ;; Configure the tiling mode-line & header-line
        (pcase-dolist (`(,var . ,setting) '((mode-line-format . tiling-mode-line)
                                            (header-line-format . tiling-header-line)))
          (when (plist-member exwm--configurations setting)
            (set var (plist-get exwm--configurations setting))))
        ;; The window needs to be mapped
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window id))
        (with-slots (x y width height) exwm--geometry
          ;; Center window of type _NET_WM_WINDOW_TYPE_SPLASH
          (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_SPLASH exwm-window-type)
            (with-slots ((x* x) (y* y) (width* width) (height* height))
                (exwm-workspace--workarea exwm--frame)
              (exwm--set-geometry id
                                  (+ x* (/ (- width* width) 2))
                                  (+ y* (/ (- height* height) 2))
                                  nil
                                  nil))))
        ;; Check for desktop.
        (when (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP exwm-window-type)
          ;; There should be only one desktop X window.
          (setq exwm-manage--desktop id)
          ;; Put it at bottom.
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window id
                             :value-mask xcb:ConfigWindow:StackMode
                             :stack-mode xcb:StackMode:Below)))
        (xcb:flush exwm--connection)
        (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
        (let ((kill-buffer-query-functions nil)
              (exwm-input--skip-buffer-list-update t))
          (kill-buffer (current-buffer)))
        (throw 'return 'ignored))
      (let* ((configured (plist-get exwm--configurations 'workspace))
             (use-configured (and (integerp configured)
                                  (<= 0 configured)
                                  (< configured (length exwm-workspace--list))))
             (startup (unless use-configured
                        (exwm-manage--startup-index id)))
             (index (cond (use-configured configured)
                          (startup (car startup))))
             ;; A startup id for another workspace must not pull the
             ;; user back there.  A manage-configuration workspace keeps
             ;; the previous behavior and does select that workspace.
             (exwm-manage--display-window
              (when (and startup index
                         (/= index exwm-workspace-current-index))
                (let ((frame (elt exwm-workspace--list index))
                      (window (cdr startup)))
                  (if (and (window-live-p window)
                           (eq (window-frame window) frame))
                      window
                    (frame-selected-window frame))))))
        (when index
          (setq exwm--frame (elt exwm-workspace--list index)))
        ;; Manage the window
        (exwm--log "Manage #x%x" id)
      (xcb:+request exwm--connection    ;remove border
          (make-instance 'xcb:ConfigureWindow
                         :window id :value-mask xcb:ConfigWindow:BorderWidth
                         :border-width 0))
      (dolist (button       ;grab buttons to set focus / move / resize
               (list xcb:ButtonIndex:1 xcb:ButtonIndex:2 xcb:ButtonIndex:3))
        (xcb:+request exwm--connection
            (make-instance 'xcb:GrabButton
                           :owner-events 0 :grab-window id
                           :event-mask xcb:EventMask:ButtonPress
                           :pointer-mode xcb:GrabMode:Sync
                           :keyboard-mode xcb:GrabMode:Async
                           :confine-to xcb:Window:None :cursor xcb:Cursor:None
                           :button button :modifiers xcb:ModMask:Any)))
      (exwm-manage--set-client-list)
      (xcb:flush exwm--connection)
      (setq exwm--stay-tiled
            (and (plist-get exwm--configurations 'stay-tiled) t))
      (let ((exwm-input--skip-buffer-list-update
             (or exwm-manage--display-window
                 exwm-input--skip-buffer-list-update))
            (origin (selected-frame)))
        (if (or exwm--stay-tiled
                (and (plist-member exwm--configurations 'floating)
                     (not (plist-get exwm--configurations 'floating)))
                (and (not (plist-member exwm--configurations 'floating))
                     (or exwm-manage-force-tiling
                         (not (or exwm-transient-for exwm--fixed-size
                                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_UTILITY
                                        exwm-window-type)
                                  (memq xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG
                                        exwm-window-type))))))
            (if exwm-manage--display-window
                (exwm-floating--unset-floating id)
              (with-selected-window (frame-selected-window exwm--frame)
                (exwm-floating--unset-floating id)))
          (exwm-floating--set-floating id))
        (when (and exwm-manage--display-window
                   (frame-live-p origin)
                   (not (eq (selected-frame) origin)))
          (select-frame origin 'norecord))
        (when (and exwm-manage--display-window
                   (not (exwm-workspace--active-p exwm--frame)))
          (exwm-layout--hide id)))
      (if (plist-get exwm--configurations 'char-mode)
          (exwm-input-release-keyboard id)
        (exwm-input-grab-keyboard id))
      (when-let* ((simulation-keys (plist-get exwm--configurations 'simulation-keys)))
        (exwm-input-set-local-simulation-keys simulation-keys))
      (when-let* ((prefix-keys (plist-get exwm--configurations 'prefix-keys)))
        (setq-local exwm-input-prefix-keys prefix-keys))
      (setq exwm-workspace--switch-history-outdated t)
      (exwm--update-desktop id)
      (exwm-manage--update-ewmh-state id)
      (exwm-manage--update-default-directory id)
      (when (or (plist-get exwm--configurations 'fullscreen)
                (exwm-layout--fullscreen-p))
        (setq exwm--ewmh-state (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN
                                     exwm--ewmh-state))
        (exwm-layout-set-fullscreen id))
      (exwm-manage--finish-startup id)
      (run-hooks 'exwm-manage-finish-hook)))))

(defun exwm-manage--unmanage-window (id &optional withdraw-only)
  "Unmanage window ID.

If WITHDRAW-ONLY is non-nil, the X window will be properly placed back to the
root window.  Set WITHDRAW-ONLY to `quit' if this functions is used when window
manager is shutting down."
  (let ((buffer (exwm--id->buffer id)))
    (exwm--log "Unmanage #x%x (buffer: %s, widthdraw: %s)"
               id buffer withdraw-only)
    (setq exwm--id-buffer-alist (assq-delete-all id exwm--id-buffer-alist))
    ;; Update workspaces when a dock is destroyed.
    (when (and (null withdraw-only)
               (assq id exwm-workspace--id-struts-alist))
      (setq exwm-workspace--id-struts-alist
            (assq-delete-all id exwm-workspace--id-struts-alist))
      (exwm-workspace--update-struts)
      (exwm-workspace--update-workareas)
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f)))
    (when (and (buffer-live-p buffer)
               ;; Invoked from `exwm-manage--exit' upon disconnection.
               (slot-value exwm--connection 'connected))
      (with-current-buffer buffer
        ;; Unmap the X window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window id))
        ;;
        (setq exwm-workspace--switch-history-outdated t)
        ;;
        (when withdraw-only
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window id :value-mask xcb:CW:EventMask
                             :event-mask xcb:EventMask:NoEvent))
          ;; Delete WM_STATE property
          (xcb:+request exwm--connection
              (make-instance 'xcb:DeleteProperty
                             :window id :property xcb:Atom:WM_STATE))
          (cond
           ((eq withdraw-only 'quit)
            ;; Remap the window when exiting.
            (xcb:+request exwm--connection
                (make-instance 'xcb:MapWindow :window id)))
           (t
            ;; Remove _NET_WM_DESKTOP.
            (xcb:+request exwm--connection
                (make-instance 'xcb:DeleteProperty
                               :window id
                               :property xcb:Atom:_NET_WM_DESKTOP)))))
        (when exwm--floating-frame
          ;; Unmap the floating frame before destroying its container.
          (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id))
                (container (frame-parameter exwm--floating-frame
                                            'exwm-container)))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow :window window))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ReparentWindow
                               :window window :parent exwm--root :x 0 :y 0))
            (xcb:+request exwm--connection
                (make-instance 'xcb:DestroyWindow :window container))))
        (when (exwm-layout--fullscreen-p)
          (let ((window (get-buffer-window)))
            (when window
              (set-window-dedicated-p window nil))))
        (exwm-manage--set-client-list)
        (xcb:flush exwm--connection))
      (let ((kill-buffer-func
             (lambda (buffer)
               (when (buffer-local-value 'exwm--floating-frame buffer)
                 (select-window
                  (frame-selected-window exwm-workspace--current)))
               (with-current-buffer buffer
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer))))))
        (exwm--defer 0 kill-buffer-func buffer)))))

(defun exwm-manage--scan ()
  "Search for existing windows and try to manage them."
  (exwm--log)
  (let* ((tree (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:QueryTree
                                  :window exwm--root)))
         reply)
    (dolist (i (slot-value tree 'children))
      (setq reply (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:GetWindowAttributes
                                     :window i)))
      ;; It's possible the X window has been destroyed.
      (when reply
        (with-slots (override-redirect map-state) reply
          (when (and (= 0 override-redirect)
                     (= xcb:MapState:Viewable map-state))
            (xcb:+request exwm--connection
                (make-instance 'xcb:UnmapWindow
                               :window i))
            (xcb:flush exwm--connection)
            (exwm-manage--manage-window i)))))))

(defun exwm-manage--ping ()
  "Send a ping to the current EXWM window.
On reply, `exwm--ping' will be incremented."
  (cl-assert exwm--id)
  (xcb:+request exwm--connection
      (make-instance 'xcb:SendEvent
                     :propagate 0
                     :destination exwm--id
                     :event-mask xcb:EventMask:NoEvent
                     :event (xcb:marshal
                             (make-instance 'xcb:ewmh:_NET_WM_PING
                                            :window exwm--id
                                            :timestamp 0
                                            :client-window exwm--id)
                             exwm--connection)))
  (xcb:flush exwm--connection))

(defun exwm-manage--kill-buffer-timeout-function (last-ping buffer)
  "Called from a timer to potentially kill unresponsive windows.

BUFFFER is the BUFFER to potentially kill.
Unless the BUFFER's `exwm--ping' greater than LAST-PING, the X window
is considered to be unresponsive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (< last-ping exwm--ping)
        (when (yes-or-no-p (format "'%s' is not responding.  \
Would you like to force close it? " (buffer-name)))
          (setq exwm--protocols
                (delq xcb:Atom:WM_DELETE_WINDOW
                      exwm--protocols))
          (kill-buffer buffer))))))

(defun exwm-manage--kill-buffer-query-function ()
  "Run in `kill-buffer-query-functions'."
  (exwm--log "id=#x%x; buffer=%s" (or exwm--id 0) (current-buffer))
  (catch 'return
    (when (or (not exwm--connection)
              (not (slot-value exwm--connection 'connected)))
      (throw 'return t))
    (when (or (not exwm--id)
              (xcb:+request-checked+request-check exwm--connection
                  (make-instance 'xcb:ChangeWindowAttributes
                                 :window exwm--id
                                 :value-mask xcb:CW:EventMask
                                 :event-mask (exwm--get-client-event-mask))))
      ;; The X window is no longer alive so just close the buffer.
      (when exwm--floating-frame
        (let ((window (frame-parameter exwm--floating-frame 'exwm-outer-id))
              (container (frame-parameter exwm--floating-frame
                                          'exwm-container)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window window))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window window
                             :parent exwm--root
                             :x 0 :y 0))
          (xcb:+request exwm--connection
              (make-instance 'xcb:DestroyWindow
                             :window container))))
      (xcb:flush exwm--connection)
      (throw 'return t))
    (unless (memq xcb:Atom:WM_DELETE_WINDOW exwm--protocols)
      ;; The X window does not support WM_DELETE_WINDOW; destroy it.
      (xcb:+request exwm--connection
          (make-instance 'xcb:DestroyWindow :window exwm--id))
      (xcb:flush exwm--connection)
      ;; Wait for DestroyNotify event.
      (throw 'return nil))
    ;; Try to close the X window with WM_DELETE_WINDOW client message.
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:SendEvent
                       :destination exwm--id
                       :event (xcb:marshal
                               (make-instance 'xcb:icccm:WM_DELETE_WINDOW
                                              :window exwm--id)
                               exwm--connection)))
    (xcb:flush exwm--connection)
    ;; PING the window (if the PING protocol is supported) and
    ;; schedule a future deletion in case the application doesn't
    ;; respond. If the window doesn't support the PING protocol,
    ;; there's nothing we can do here. The application may actually be
    ;; responsive, it may just be refusing to close because it's
    ;; asking the user to, e.g., save a document.
    (when (memq xcb:Atom:_NET_WM_PING exwm--protocols)
      (let ((last-ping exwm--ping))
        (exwm-manage--ping)
        (run-with-timer exwm-manage-ping-timeout nil
                        #'exwm-manage--kill-buffer-timeout-function
                        last-ping (current-buffer))))
    ;; At this point, we don't close the buffer but instead wait
    ;; for the window to close itself. Once that happens, we'll
    ;; receive an event and kill the buffer.
    nil))

(defun exwm-manage--kill-client (&optional id)
  "Kill the X client associated with the window ID.
If ID is nil, kill the X client associated with the current buffer.

NOTE: This command is the equivalent of the xkill program. If you just
want to close a window, delete the associated buffer."
  (unless id (setq id (exwm--buffer->id (current-buffer))))
  (exwm--log "id=#x%x" id)
  (xcb:+request exwm--connection (make-instance 'xcb:KillClient :resource id))
  (xcb:flush exwm--connection))

(defun exwm-manage--add-frame (frame)
  "Run in `after-make-frame-functions'.
FRAME is the newly created frame."
  (exwm--log "frame=%s" frame)
  (when (display-graphic-p frame)
    (push (string-to-number (frame-parameter frame 'outer-window-id))
          exwm-manage--frame-outer-id-list)))

(defun exwm-manage--remove-frame (frame)
  "Run in `delete-frame-functions'.
FRAME is the frame to be deleted."
  (exwm--log "frame=%s" frame)
  (when (display-graphic-p frame)
    (setq exwm-manage--frame-outer-id-list
          (delq (string-to-number (frame-parameter frame 'outer-window-id))
                exwm-manage--frame-outer-id-list))))

(defun exwm-manage--send-ConfigureNotify (window x y width height)
  "Send a ConfigureNotify event to WINDOW with X Y WIDTH and HEIGHT."
  (exwm--log "Reply with ConfigureNotify: %dx%d+%d+%d" width height x y)
  (xcb:+request exwm--connection
      (make-instance 'xcb:SendEvent
                     :propagate 0 :destination window
                     :event-mask xcb:EventMask:StructureNotify
                     :event (xcb:marshal
                             (make-instance
                              'xcb:ConfigureNotify
                              :event window :window window
                              :above-sibling xcb:Window:None
                              :x x :y y
                              :width width
                              :height height
                              :border-width 0 :override-redirect 0)
                             exwm--connection))))

(defun exwm-manage--on-ConfigureRequest (data _synthetic)
  "Handle ConfigureRequest event.
DATA contains unmarshalled ConfigureRequest event data."
  (exwm--log)
  (with-slots (window x y width height
                      border-width sibling stack-mode value-mask)
      (xcb:unmarshal-new 'xcb:ConfigureRequest data)
    (exwm--log "#x%x (#x%x) @%dx%d%+d%+d; \
border-width: %d; sibling: #x%x; stack-mode: %d"
               window value-mask width height x y
               border-width sibling stack-mode)
    (if-let* ((buffer (exwm--id->buffer window)))
        (with-current-buffer buffer
          (if (exwm-layout--fullscreen-p)
              ;; Fit fullscreen windows to the workspace.
              (with-slots (x y width height)
                  (exwm-workspace--get-geometry exwm--frame)
                (exwm-manage--send-ConfigureNotify
                 window x y width height))
            (let* ((edges (exwm--window-inside-absolute-pixel-edges
                           (get-buffer-window buffer t)))
                   (window-x (elt edges 0))
                   (window-y (elt edges 1))
                   (window-width (- (elt edges 2) window-x))
                   (window-height (- (elt edges 3) window-y)))
              (if (not exwm--floating-frame)
                  ;; If the window isn't floating, fit it to its Emacs window.
                  (exwm-manage--send-ConfigureNotify
                   window window-x window-y
                   window-width window-height)
                ;; Finally, resize the floating window.
                (exwm--log "ConfigureWindow (resize floating X window)")
                (let* ((frame-id (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id))
                       (frame-edges (frame-edges exwm--floating-frame
                                                 'outer-edges))
                       (frame-width (- (elt frame-edges 2)
                                       (elt frame-edges 0)))
                       (frame-height (- (elt frame-edges 3)
                                        (elt frame-edges 1))))
                  (exwm--set-geometry
                   frame-id
                   nil nil
                   (unless (= 0 (logand value-mask xcb:ConfigWindow:Width))
                     (+ frame-width (- width window-width)))
                   (unless (= 0 (logand value-mask xcb:ConfigWindow:Height))
                     (+ frame-height (- height window-height)))))))))
      (exwm--log "ConfigureWindow (preserve geometry)")
      ;; Configure the unmanaged window.
      ;; But Emacs frames should be excluded.  Generally we don't
      ;; receive ConfigureRequest events from Emacs frames since we
      ;; have set OverrideRedirect on them, but this is not true for
      ;; Lucid build (as of 25.1).
      (unless (memq window exwm-manage--frame-outer-id-list)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window window
                           :value-mask value-mask
                           :x x :y y :width width :height height
                           :border-width border-width
                           :sibling sibling
                           :stack-mode stack-mode)))))
  (xcb:flush exwm--connection))

(defun exwm-manage--on-MapRequest (data _synthetic)
  "Handle MapRequest event.
DATA contains unmarshalled MapRequest event data."
  (with-slots (parent window)
      (xcb:unmarshal-new 'xcb:MapRequest data)
    (exwm--log "id=#x%x parent=#x%x" window parent)
    (if (assoc window exwm--id-buffer-alist)
        (with-current-buffer (exwm--id->buffer window)
          (if (exwm-layout--iconic-state-p)
              ;; State change: iconic => normal.
              (when (eq exwm--frame exwm-workspace--current)
                (pop-to-buffer-same-window (current-buffer)))
            (exwm--log "#x%x is already managed" window)))
      (if (/= exwm--root parent)
          (progn (xcb:+request exwm--connection
                     (make-instance 'xcb:MapWindow :window window))
                 (xcb:flush exwm--connection))
        (exwm--log "#x%x" window)
        (exwm-manage--manage-window window)))))

(defun exwm-manage--on-UnmapNotify (data _synthetic)
  "Handle UnmapNotify event.
DATA contains unmarshalled UnmapNotify event data."
  (with-slots (window)
      (xcb:unmarshal-new 'xcb:UnmapNotify data)
    (exwm--log "id=#x%x" window)
    (exwm-manage--unmanage-window window t)))

(defun exwm-manage--on-MapNotify (data _synthetic)
  "Handle MapNotify event.
DATA contains unmarshalled MapNotify event data."
  (with-slots (window)
      (xcb:unmarshal-new 'xcb:MapNotify data)
    (when (assoc window exwm--id-buffer-alist)
      (exwm--log "id=#x%x" window)
      ;; With this we ensure that a "window hierarchy change" happens after
      ;; mapping the window, as some servers (XQuartz) do not generate it.
      (with-current-buffer (exwm--id->buffer window)
        (if exwm--floating-frame
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window window
                               :value-mask xcb:ConfigWindow:StackMode
                               :stack-mode xcb:StackMode:Above))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ConfigureWindow
                             :window window
                             :value-mask (logior xcb:ConfigWindow:Sibling
                                                 xcb:ConfigWindow:StackMode)
                             :sibling exwm--guide-window
                             :stack-mode xcb:StackMode:Above))))
      ;; MapNotify stacks the client above the guide window after
      ;; `exwm-layout--show' has returned.  Raise child frames again so
      ;; that restack does not cover them.  A floating MapNotify uses
      ;; StackMode Above with no sibling, which would otherwise put the
      ;; client at the top.
      (exwm-workspace--raise-child-frames)
      (xcb:flush exwm--connection))))

(defun exwm-manage--on-DestroyNotify (data synthetic)
  "Handle DestroyNotify event.
DATA contains unmarshalled DestroyNotify event data.
SYNTHETIC indicates whether the event is a synthetic event."
  (unless synthetic
    (exwm--log)
    (with-slots (window) (xcb:unmarshal-new 'xcb:DestroyNotify data)
      (exwm--log "#x%x" window)
      (exwm-manage--unmanage-window window))))

(defun exwm-manage--init ()
  "Initialize manage module."
  ;; Intern _MOTIF_WM_HINTS
  (exwm--log)
  (setq exwm-manage--_MOTIF_WM_HINTS (exwm--intern-atom "_MOTIF_WM_HINTS")
        exwm-manage--_NET_STARTUP_ID (exwm--intern-atom "_NET_STARTUP_ID")
        exwm-manage--_NET_STARTUP_INFO (exwm--intern-atom "_NET_STARTUP_INFO")
        exwm-manage--_NET_STARTUP_INFO_BEGIN
        (exwm--intern-atom "_NET_STARTUP_INFO_BEGIN")
        exwm-manage--wm-client-leader (exwm--intern-atom "WM_CLIENT_LEADER")
        exwm-manage--startup-records nil
        exwm-manage--startup-message nil
        exwm-manage--frame-outer-id-list nil)
  (advice-add 'make-process :filter-args #'exwm-manage--startup-make-process)
  (dolist (frame (frame-list))
    (when (display-graphic-p frame)
      (exwm-manage--add-frame frame)))
  (add-hook 'after-make-frame-functions #'exwm-manage--add-frame)
  (add-hook 'delete-frame-functions #'exwm-manage--remove-frame)
  (xcb:+event exwm--connection 'xcb:ConfigureRequest
              #'exwm-manage--on-ConfigureRequest)
  (xcb:+event exwm--connection 'xcb:MapRequest #'exwm-manage--on-MapRequest)
  (xcb:+event exwm--connection 'xcb:UnmapNotify #'exwm-manage--on-UnmapNotify)
  (xcb:+event exwm--connection 'xcb:MapNotify #'exwm-manage--on-MapNotify)
  (xcb:+event exwm--connection 'xcb:DestroyNotify
              #'exwm-manage--on-DestroyNotify))

(defun exwm-manage--exit ()
  "Exit the manage module."
  ;; A clean exit only.  A crash never gets here.  `exwm-session'
  ;; starts Emacs again, and the next startup manages clients that
  ;; are still children of the root.
  (exwm--log)
  (dolist (pair exwm--id-buffer-alist)
    (exwm-manage--unmanage-window (car pair) 'quit))
  (remove-hook 'after-make-frame-functions #'exwm-manage--add-frame)
  (remove-hook 'delete-frame-functions #'exwm-manage--remove-frame)
  (advice-remove 'make-process #'exwm-manage--startup-make-process)
  (setq exwm-manage--_MOTIF_WM_HINTS nil
        exwm-manage--_NET_STARTUP_ID nil
        exwm-manage--_NET_STARTUP_INFO nil
        exwm-manage--_NET_STARTUP_INFO_BEGIN nil
        exwm-manage--wm-client-leader nil
        exwm-manage--startup-records nil
        exwm-manage--startup-message nil))

(provide 'exwm-manage)
;;; exwm-manage.el ends here
