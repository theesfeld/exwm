;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

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

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(require 'exwm-core)

(defgroup exwm-layout nil
  "Layout."
  :group 'exwm)

(defcustom exwm-layout-auto-iconify t
  "Non-nil to automatically iconify unused X windows when possible."
  :type 'boolean)

(defcustom exwm-layout-fullscreen-release-keyboard t
  "Non-nil to release the keyboard when an application is fullscreened.
That is, when t, Emacs won't intercept keys sent to fullscreen applications."
  :type 'boolean)

(defcustom exwm-layout-minibuffer-unfullscreen nil
  "When non-nil, leave fullscreen while the minibuffer is active.
A fullscreen X client covers the minibuffer.  The client is restored
for the duration of the minibuffer, then made fullscreen again unless
the user has left fullscreen."
  :type 'boolean)

(defvar exwm-layout--minibuffer-fullscreen nil
  "X window ids taken out of fullscreen for the minibuffer.")

(defvar-local exwm--fullscreen-for-minibuffer nil
  "Non-nil while fullscreen is suspended so the minibuffer can be seen.")

(defcustom exwm-layout-show-all-buffers nil
  "Non-nil to allow switching to buffers on other workspaces."
  :type 'boolean)

(defconst exwm-layout--floating-hidden-position -101
  "Where to place hidden floating X windows.")

(defun exwm-layout--raise-floating ()
  "Raise the current floating window above other floating windows.
The client is a sibling of its container, so the container is raised
and the client is stacked directly above it."
  (when (and exwm--floating-frame exwm--id)
    (let ((container (frame-parameter exwm--floating-frame 'exwm-container)))
      (when container
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window container
                           :value-mask xcb:ConfigWindow:StackMode
                           :stack-mode xcb:StackMode:Above))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--id
                           :value-mask (logior xcb:ConfigWindow:Sibling
                                               xcb:ConfigWindow:StackMode)
                           :sibling container
                           :stack-mode xcb:StackMode:Above))))))

(defun exwm-layout--placeholder-geometry-p (x y width height)
  "Non-nil when X Y WIDTH HEIGHT is the off-screen min-size floating frame.
`exwm-floating--set-floating' creates that frame before the real
geometry exists.  Copying it onto the client leaves a tiny window
inside a correctly sized container."
  (and (numberp x) (numberp y) (numberp width) (numberp height)
       (<= width 160) (<= height 120)
       (or (< x -1000) (< y -1000) (> x 30000) (> y 30000))))

(defvar exwm-layout--other-buffer-exclude-buffers nil
  "List of buffers that should not be selected by `other-buffer'.")

(defvar exwm-layout--other-buffer-exclude-exwm-mode-buffers nil
  "When non-nil, prevent EXWM buffers from being selected by `other-buffer'.")

(defvar exwm-layout--timer nil "Timer used to track echo area changes.")

(defvar exwm-workspace--current)
(defvar exwm-workspace--list)
(defvar exwm-workspace--showing-desktop)
(declare-function exwm-workspace--set-fullscreen "exwm-workspace.el" (frame))
(declare-function exwm-workspace--update-workareas "exwm-workspace.el" ())
(declare-function exwm-input--release-keyboard "exwm-input.el")
(declare-function exwm-input--grab-keyboard "exwm-input.el")
(declare-function exwm-input-grab-keyboard "exwm-input.el")
(declare-function exwm-workspace--active-p "exwm-workspace.el" (frame))
(declare-function exwm-workspace--get-geometry "exwm-workspace.el" (frame))
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace--workspace-p "exwm-workspace.el"
                  (workspace))
(declare-function exwm-workspace-move-window "exwm-workspace.el"
                  (frame-or-index &optional id))
(declare-function exwm-workspace--raise-child-frames "exwm-workspace.el" ())
(declare-function exwm-floating--refresh-emacs-frame "exwm-floating.el"
                  (frame))

(defun exwm-layout--set-state (id state)
  "Set WM_STATE of X window ID to STATE."
  (exwm--log "id=#x%x" id)
  (xcb:+request exwm--connection
      (make-instance 'xcb:icccm:set-WM_STATE
                     :window id :state state :icon xcb:Window:None))
  (with-current-buffer (exwm--id->buffer id)
    (setq exwm-state state)))

(defun exwm-layout--iconic-state-p (&optional id)
  "Check whether X window ID is in iconic state."
  (= xcb:icccm:WM_STATE:IconicState
     (if id
         (buffer-local-value 'exwm-state (exwm--id->buffer id))
       exwm-state)))

(defun exwm-layout--set-ewmh-state (id)
  "Set _NET_WM_STATE of X window ID to the value of variable `exwm--ewmh-state'."
  (with-current-buffer (exwm--id->buffer id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data exwm--ewmh-state))))

(defun exwm-layout--fullscreen-p ()
  "Check whether current `exwm-mode' buffer is in fullscreen state."
  (when (derived-mode-p 'exwm-mode)
    (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)))

(defun exwm-layout--auto-iconify ()
  "Helper function to iconify unused X windows.
See variable `exwm-layout-auto-iconify'."
  (when (and exwm-layout-auto-iconify
             (not exwm-transient-for))
    (let ((xwin exwm--id)
          (state exwm-state))
      (dolist (pair exwm--id-buffer-alist)
        (with-current-buffer (cdr pair)
          (when (and exwm--floating-frame
                     (eq exwm-transient-for xwin)
                     (not (eq exwm-state state)))
            (if (eq state xcb:icccm:WM_STATE:NormalState)
                (exwm-layout--refresh-floating exwm--floating-frame)
              (exwm-layout--hide exwm--id))))))))

(cl-defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (when exwm-workspace--showing-desktop
    (exwm--log "Show #x%x deferred; the desktop is showing" id)
    (cl-return-from exwm-layout--show))
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (exwm--window-inside-absolute-pixel-edges window))
         (x (pop edges))
         (y (pop edges))
         (width (- (pop edges) x))
         (height (- (pop edges) y)))
    (with-current-buffer (exwm--id->buffer id)
      (when exwm--floating-frame
        (let ((container (frame-parameter exwm--floating-frame 'exwm-container))
              (inset (frame-parameter exwm--floating-frame 'exwm-floating-inset)))
          ;; Restore a parked container.  Do not add its origin to the
          ;; Emacs window edges: those edges are already absolute, and
          ;; adding the origin again is the 0.35 mis-position
          ;; (dde5e7a).
          (when (and container exwm--floating-frame-geometry)
            (with-slots ((frame-x x) (frame-y y)
                         (frame-width width) (frame-height height))
                exwm--floating-frame-geometry
              (when (and (numberp frame-width) (numberp frame-height)
                         (> frame-width 1) (> frame-height 1))
                (exwm--set-geometry container
                                    frame-x frame-y
                                    frame-width frame-height))))
          (setq exwm--floating-frame-geometry nil)
          ;; Emacs learns the frame position from its own X connection.
          ;; EXWM moves the frame on the XELB connection, so the cached
          ;; edges can still describe the initial off-screen frame
          ;; (about 70x38).  The container geometry is the one EXWM
          ;; just applied.
          (when (and container inset)
            (when-let* ((geometry
                         (xcb:+request-unchecked+reply
                             exwm--connection
                             (make-instance 'xcb:GetGeometry
                                            :drawable container))))
              (with-slots ((cx x) (cy y) (cw width) (ch height)
                           (cborder border-width))
                  geometry
                (when (and (> cw 1) (> ch 1))
                  (setq x (+ cx (or cborder 0) (nth 0 inset))
                        y (+ cy (or cborder 0) (nth 1 inset))
                        width (max 1 (- cw (nth 0 inset) (nth 2 inset)))
                        height (max 1 (- ch (nth 1 inset) (nth 3 inset))))))))))
      (when (exwm-layout--fullscreen-p)
        (with-slots ((x* x)
                     (y* y)
                     (width* width)
                     (height* height))
            (exwm-workspace--get-geometry exwm--frame)
          (setq x x*
                y y*
                width width*
                height height*)))
      (unless (exwm-layout--placeholder-geometry-p x y width height)
        (exwm--set-geometry id x y width height))
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  ;; The client was just mapped above the workspace frame.  Raise child
  ;; frames after it so a popup stays visible over that client.
  (exwm-workspace--raise-child-frames)
  (xcb:flush exwm--connection))

(defun exwm-layout--hide (id)
  "Hide window ID."
  (with-current-buffer (exwm--id->buffer id)
    (unless (or (exwm-layout--iconic-state-p)
                (and exwm--floating-frame
                     exwm--desktop
                     (= #xffffffff exwm--desktop)))
      (exwm--log "Hide #x%x" id)
      (when exwm--floating-frame
        (let* ((container (frame-parameter exwm--floating-frame
                                           'exwm-container))
               (geometry (xcb:+request-unchecked+reply exwm--connection
                             (make-instance 'xcb:GetGeometry
                                            :drawable container))))
          ;; A second hide sees the parked 1x1 window.  Keep the
          ;; geometry saved by the first hide.
          (when (and geometry
                     (> (slot-value geometry 'width) 1)
                     (> (slot-value geometry 'height) 1))
            (setq exwm--floating-frame-geometry geometry))
          (exwm--set-geometry container exwm-layout--floating-hidden-position
                              exwm-layout--floating-hidden-position
                              1
                              1)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:NoEvent))
      (xcb:+request exwm--connection
          (make-instance 'xcb:UnmapWindow :window id))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask (exwm--get-client-event-mask)))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:IconicState)
      (cl-pushnew xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state)
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)
      (xcb:flush exwm--connection))))

(defvar-local exwm--window-dedicated-before-fullscreen nil
  "Value of `window-dedicated-p' saved by `exwm-layout-set-fullscreen'.")

(defvar-local exwm--fullscreen-hold nil
  "Non-nil when the user left fullscreen and the client must not restore it.")

(cl-defun exwm-layout-set-fullscreen (&optional id user)
  "Make window ID fullscreen.
When USER is non-nil, this is a user request and a previous choice to
stay out of fullscreen is cleared.  A client request is ignored while
that choice is in effect."
  (interactive)
  (when (called-interactively-p 'any)
    (setq user t))
  (exwm--log "id=#x%x" (or id 0))
  (unless (or id (derived-mode-p 'exwm-mode))
    (cl-return-from exwm-layout-set-fullscreen))
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; Fullscreen is a property of this buffer.  The selected buffer
    ;; may be a different one when ID is given.
    (when (exwm-layout--fullscreen-p)
      (cl-return-from exwm-layout-set-fullscreen))
    (when (and exwm--fullscreen-hold (not user))
      (cl-return-from exwm-layout-set-fullscreen))
    (when user
      (setq exwm--fullscreen-for-minibuffer nil))
    (when (and exwm--fullscreen-for-minibuffer (not user))
      (cl-return-from exwm-layout-set-fullscreen))
    (setq exwm--fullscreen-hold nil)
    ;; Expand the X window to fill the whole screen.
    (with-slots (x y width height) (exwm-workspace--get-geometry exwm--frame)
      (exwm--set-geometry exwm--id x y width height))
    ;; Raise the X window.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--id
                       :value-mask (logior xcb:ConfigWindow:BorderWidth
                                           xcb:ConfigWindow:StackMode)
                       :border-width 0
                       :stack-mode xcb:StackMode:Above))
    (cl-pushnew xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
    (exwm-layout--set-ewmh-state exwm--id)
    (xcb:flush exwm--connection)
    (let ((window (get-buffer-window nil t)))
      (when (window-live-p window)
        (setq exwm--window-dedicated-before-fullscreen
              (window-dedicated-p window))
        (set-window-dedicated-p window t)))
    (when exwm-layout-fullscreen-release-keyboard
      (exwm-input--release-keyboard exwm--id))))

(cl-defun exwm-layout-unset-fullscreen (&optional id user)
  "Restore X window ID from fullscreen state.
When USER is non-nil, later client requests to enter fullscreen are
ignored until the user enters fullscreen again."
  (interactive)
  (when (called-interactively-p 'any)
    (setq user t))
  (exwm--log "id=#x%x" (or id 0))
  (unless (or id (derived-mode-p 'exwm-mode))
    (cl-return-from exwm-layout-unset-fullscreen))
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; The selected buffer is not the target when ID names another
    ;; X window.  Leave a window that is not fullscreen alone.
    (unless (exwm-layout--fullscreen-p)
      (cl-return-from exwm-layout-unset-fullscreen))
    (when user
      (setq exwm--fullscreen-hold t
            exwm--fullscreen-for-minibuffer nil))
    ;; `exwm-layout--show' relies on `exwm--ewmh-state' to decide whether to
    ;; fullscreen the window.
    (setq exwm--ewmh-state
          (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
    (exwm-layout--set-ewmh-state exwm--id)
    (if exwm--floating-frame
        (exwm-layout--show exwm--id (frame-root-window exwm--floating-frame))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window exwm--id
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling exwm--guide-window
                         :stack-mode xcb:StackMode:Above))
      (let ((window (get-buffer-window nil t)))
        (when window
          (exwm-layout--show exwm--id window))))
    (xcb:flush exwm--connection)
    (let ((window (get-buffer-window nil t)))
      (when (window-live-p window)
        (set-window-dedicated-p window
                                exwm--window-dedicated-before-fullscreen))
      (setq exwm--window-dedicated-before-fullscreen nil))
    (when (eq 'line-mode exwm--selected-input-mode)
      (exwm-input--grab-keyboard exwm--id))))

(defun exwm-layout-toggle-fullscreen (&optional id)
  "Toggle fullscreen mode of X window ID.
If ID is non-nil, default to ID of `window-buffer'."
  (interactive)
  (setq id (or id (exwm--buffer->id (current-buffer))
               (user-error "Current buffer has no X window ID")))
  (exwm--log "id=#x%x" id)
  (let ((user (called-interactively-p 'any)))
    (with-current-buffer (exwm--id->buffer id)
      (if (exwm-layout--fullscreen-p)
          (exwm-layout-unset-fullscreen id user)
        (exwm-layout-set-fullscreen id user)))))

(defun exwm-layout--other-buffer-predicate (buffer)
  "Return non-nil when the BUFFER may be displayed in selected frame.

Prevents EXWM-mode buffers already being displayed on some other window from
being selected.

Should be set as `buffer-predicate' frame parameter for all
frames.  Used by `other-buffer'.

When variable `exwm-layout--other-buffer-exclude-exwm-mode-buffers'
is t EXWM buffers are never selected by `other-buffer'.

When variable `exwm-layout--other-buffer-exclude-buffers' is a
list of buffers, EXWM buffers belonging to that list are never
selected by `other-buffer'."
  (or (not (with-current-buffer buffer (derived-mode-p 'exwm-mode)))
      (and (not exwm-layout--other-buffer-exclude-exwm-mode-buffers)
           (not (memq buffer exwm-layout--other-buffer-exclude-buffers))
           ;; Do not select if already shown in some window.
           (not (get-buffer-window buffer t)))))

(defun exwm-layout--raise-fullscreen (frame)
  "Raise fullscreen X clients on workspace FRAME above other clients.
Mapping the other clients on that workspace stacks them above a
client that was already fullscreen.  Child frames are raised again
by the caller so a popup stays visible."
  (dolist (pair exwm--id-buffer-alist)
    (with-current-buffer (cdr pair)
      (when (and (eq exwm--frame frame)
                 (exwm-layout--fullscreen-p)
                 (not (exwm-layout--iconic-state-p)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--id
                           :value-mask (logior xcb:ConfigWindow:BorderWidth
                                               xcb:ConfigWindow:StackMode)
                           :border-width 0
                           :stack-mode xcb:StackMode:Above))))))

(defun exwm-layout--set-client-list-stacking ()
  "Set _NET_CLIENT_LIST_STACKING."
  (exwm--log)
  (let (clients-floating clients clients-iconic clients-other)
    (pcase-dolist (`(,id . ,buffer) exwm--id-buffer-alist)
      (with-current-buffer buffer
        (cond
         ;; X window on other workspaces.
         ((not (eq exwm--frame exwm-workspace--current))
          (push id clients-other))
         ;; A floating X window on the current workspace.
         (exwm--floating-frame (push id clients-floating))
         ;; A normal tilling X window on the current workspace.
         ((get-buffer-window buffer exwm-workspace--current)
          (push id clients))
         ;; An iconic tilling X window on the current workspace.
         (t (push id clients-iconic)))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_CLIENT_LIST_STACKING
                       :window exwm--root
                       :data (vconcat clients-other clients-iconic
                                      clients clients-floating)))))

(defun exwm-layout--refresh (&optional frame)
  "Refresh layout of FRAME.
If FRAME is nil, refresh layout of selected frame."
  ;; `window-size-change-functions' sets this argument while
  ;; `window-configuration-change-hook' makes the frame selected.
  (unless frame
    (setq frame (selected-frame)))
  (exwm--log "frame=%s" frame)
  (if (not (exwm-workspace--workspace-p frame))
      (if (frame-parameter frame 'exwm-outer-id)
          (exwm-layout--refresh-floating frame)
        (exwm-layout--refresh-other frame))
    (exwm-layout--refresh-workspace frame)))

(defun exwm-layout--refresh-floating (frame)
  "Refresh floating frame FRAME."
  (exwm--log "Refresh floating %s" frame)
  (if (frame-parameter frame 'exwm-floating-emacs)
      ;; An ordinary Emacs buffer has no X client to show.  Fit the
      ;; container to the frame, or park it with its workspace.
      (exwm-floating--refresh-emacs-frame frame)
    (let ((window (frame-first-window frame)))
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'exwm-mode)
                   ;; It may be a buffer waiting to be killed.
                   (exwm--id->buffer exwm--id))
          (exwm--log "Refresh floating window #x%x" exwm--id)
          (if (and (exwm-workspace--active-p exwm--frame)
                   (not (exwm-layout--iconic-state-p)))
              (exwm-layout--show exwm--id window)
            (exwm-layout--hide exwm--id)))))))

(defun exwm-layout--refresh-other (frame)
  "Refresh client or nox frame FRAME."
  ;; Other frames (e.g. terminal/graphical frame of emacsclient)
  ;; We shall bury all `exwm-mode' buffers in this case
  (exwm--log "Refresh other %s" frame)
  (let ((windows (window-list frame 'nomini)) ;exclude minibuffer
        (exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
    (dolist (window windows)
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'exwm-mode)
          (if (window-prev-buffers window)
              (switch-to-prev-buffer window)
            (switch-to-next-buffer window)))))))

(defun exwm-layout--refresh-workspace (frame)
  "Refresh workspace frame FRAME."
  (exwm--log "Refresh workspace %s" frame)
  ;; Workspaces other than the active one can also be refreshed (RandR)
  (let (covered-buffers   ;EXWM-buffers covered by a new X window.
        vacated-windows)  ;Windows previously displaying EXWM-buffers.
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (when (and (not exwm--floating-frame) ;exclude floating X windows
                   (or exwm-layout-show-all-buffers
                       ;; Exclude X windows on other workspaces
                       (eq frame exwm--frame)))
          (let (;; List of windows in current frame displaying the `exwm-mode'
                ;; buffers.
                (windows (get-buffer-window-list (current-buffer) 'nomini
                                                 frame)))
            (if (not windows)
                (when (eq frame exwm--frame)
                  ;; Hide it if it was being shown in this workspace.
                  (exwm-layout--hide exwm--id))
              (let ((window (car windows)))
                (if (eq frame exwm--frame)
                    ;; Show it if `frame' is active, hide otherwise.
                    ;; Hiding a covered buffer also sets IconicState.
                    ;; A buffer that is on screen must be mapped again;
                    ;; leaving it iconic paints a black window.  A
                    ;; minimized client has no window here, so it stays
                    ;; hidden.
                    (if (exwm-workspace--active-p frame)
                        (exwm-layout--show exwm--id window)
                      (exwm-layout--hide exwm--id))
                  ;; It was last shown in other workspace; move it here.
                  (exwm-workspace-move-window frame exwm--id))
                ;; Vacate any other windows (in any workspace) showing this
                ;; `exwm-mode' buffer.
                (setq vacated-windows
                      (append vacated-windows (remove
                                               window
                                               (get-buffer-window-list
                                                (current-buffer) 'nomini t))))
                ;; Note any `exwm-mode' buffer is being covered by another
                ;; `exwm-mode' buffer.  We want to avoid that `exwm-mode'
                ;; buffer to be reappear in any of the vacated windows.
                (let ((prev-buffer (car-safe
                                    (car-safe (window-prev-buffers window)))))
                  (and
                   prev-buffer
                   (buffer-live-p prev-buffer)
                   (with-current-buffer prev-buffer
                     (derived-mode-p 'exwm-mode))
                   (push prev-buffer covered-buffers)))))))))
    ;; Set some sensible buffer to vacated windows.
    (let ((exwm-layout--other-buffer-exclude-buffers covered-buffers))
      (dolist (window vacated-windows)
        (if (window-prev-buffers window)
            (switch-to-prev-buffer window)
          (switch-to-next-buffer window))))
    ;; Make sure windows floating / on other workspaces are excluded
    (let ((exwm-layout--other-buffer-exclude-exwm-mode-buffers t))
      (dolist (window (window-list frame 'nomini))
        (with-current-buffer (window-buffer window)
          (when (and (derived-mode-p 'exwm-mode)
                     (or exwm--floating-frame (not (eq frame exwm--frame))))
            (if (window-prev-buffers window)
                (switch-to-prev-buffer window)
              (switch-to-next-buffer window))))))
    (exwm-layout--set-client-list-stacking)
    (exwm-layout--raise-fullscreen frame)
    (exwm-workspace--raise-child-frames)
    (xcb:flush exwm--connection)))

(defun exwm-layout--on-minibuffer-setup ()
  "Refresh layout when minibuffer grows."
  (exwm--log)
  ;; Only when active minibuffer's frame is an EXWM frame.
  (let* ((mini-window (active-minibuffer-window))
         (frame (window-frame mini-window)))
    (when (exwm-workspace--workspace-p frame)
      (exwm--defer 0 (lambda ()
                       (when (< 1 (window-height mini-window))
                         (exwm-layout--refresh frame)))))))

(defun exwm-layout--on-echo-area-change (&optional dirty)
  "Run when message arrives or in `echo-area-clear-hook' to refresh layout.
If DIRTY is non-nil, refresh layout immediately."
  (let ((frame (window-frame (active-minibuffer-window)))
        (msg (current-message)))
    ;; Check whether the frame where current window's minibuffer resides (not
    ;; current window's frame for floating windows!) must be adjusted.
    (when (and msg
               (exwm-workspace--workspace-p frame)
               (or (cl-position ?\n msg)
                   (> (length msg) (frame-width frame))))
      (exwm--log)
      (if dirty
          (exwm-layout--refresh exwm-workspace--current)
        (exwm--defer 0 #'exwm-layout--refresh exwm-workspace--current)))))

(defun exwm-layout-enlarge-window (delta &optional horizontal)
  "Make the selected window DELTA pixels taller.

If no argument is given, make the selected window one pixel taller.  If the
optional argument HORIZONTAL is non-nil, make selected window DELTA pixels
wider.  If DELTA is negative, shrink selected window by -DELTA pixels.

Normal hints are checked and regarded if the selected window is displaying an
`exwm-mode' buffer.  However, this may violate the normal hints set on other X
windows."
  (interactive "p")
  (exwm--log)
  (cond
   ((zerop delta))                     ;no operation
   ((window-minibuffer-p))             ;avoid resize minibuffer-window
   ((not (and (derived-mode-p 'exwm-mode) exwm--floating-frame))
    ;; Resize on tiling layout
    (unless (= 0 (window-resizable nil delta horizontal nil t)) ;not resizable
      (let ((window-resize-pixelwise t))
        (window-resize nil delta horizontal nil t))))
   ;; Resize on floating layout
   (exwm--fixed-size)                   ;fixed size
   (horizontal
    (let* ((width (frame-outer-width))
           (edges (exwm--window-inside-pixel-edges))
           (inner-width (- (elt edges 2) (elt edges 0)))
           (margin (- width inner-width)))
      (if (> delta 0)
          (if (not exwm--normal-hints-max-width)
              (incf width delta)
            (if (>= inner-width exwm--normal-hints-max-width)
                (setq width nil)
              (setq width (min (+ exwm--normal-hints-max-width margin)
                               (+ width delta)))))
        (if (not exwm--normal-hints-min-width)
            (incf width delta)
          (if (<= inner-width exwm--normal-hints-min-width)
              (setq width nil)
            (setq width (max (+ exwm--normal-hints-min-width margin)
                             (+ width delta))))))
      (when (and width (> width 0))
        (setf (slot-value exwm--geometry 'width) width)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-outer-id)
                           :value-mask xcb:ConfigWindow:Width
                           :width width))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-container)
                           :value-mask xcb:ConfigWindow:Width
                           :width width))
        (xcb:flush exwm--connection))))
   (t
    (let* ((height (frame-outer-height))
           (edges (exwm--window-inside-pixel-edges))
           (inner-height (- (elt edges 3) (elt edges 1)))
           (margin (- height inner-height)))
      (if (> delta 0)
          (if (not exwm--normal-hints-max-height)
              (incf height delta)
            (if (>= inner-height exwm--normal-hints-max-height)
                (setq height nil)
              (setq height (min (+ exwm--normal-hints-max-height margin)
                                (+ height delta)))))
        (if (not exwm--normal-hints-min-height)
            (incf height delta)
          (if (<= inner-height exwm--normal-hints-min-height)
              (setq height nil)
            (setq height (max (+ exwm--normal-hints-min-height margin)
                              (+ height delta))))))
      (when (and height (> height 0))
        (setf (slot-value exwm--geometry 'height) height)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-outer-id)
                           :value-mask xcb:ConfigWindow:Height
                           :height height))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (frame-parameter exwm--floating-frame
                                                    'exwm-container)
                           :value-mask xcb:ConfigWindow:Height
                           :height height))
        (xcb:flush exwm--connection))))))

(defun exwm-layout-enlarge-window-horizontally (delta)
  "Make the selected window DELTA pixels wider.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window delta t))

(defun exwm-layout-shrink-window (delta)
  "Make the selected window DELTA pixels lower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window (- delta)))

(defun exwm-layout-shrink-window-horizontally (delta)
  "Make the selected window DELTA pixels narrower.

See also `exwm-layout-enlarge-window'."
  (interactive "p")
  (exwm--log "%s" delta)
  (exwm-layout-enlarge-window (- delta) t))

(defun exwm-layout--window-bottom-offset (window)
  "Compute the distance from the bottom of WINDOW to the bottom of its frame."
  (- (elt (frame-edges (window-frame window) 'outer-edges) 3)
     (elt (exwm--window-inside-pixel-edges ) 3)))

(defun exwm-layout-hide-mode-line ()
  "Hide the mode-line.
See `exwm-layout-toggle-mode-line' for more details."
  (interactive)
  (exwm--log)
  (exwm-layout-toggle-mode-line -1))

(defun exwm-layout-show-mode-line ()
  "Show the mode-line.
See `exwm-layout-toggle-mode-line' for more details."
  (interactive)
  (exwm--log)
  (exwm-layout-toggle-mode-line 1))

;; You can do this with by let-binding places with `gv-ref' and
;; `gv-deref' instead of a macro, but the macro is cleaner.
(defmacro exwm-layout--toggle-mode-line-1
    (hide active-place saved-place none)
  "Macro implementing the core logic behind mode-line toggling.

If HIDE, the modeline is hidden. Otherwise, it is shown.

ACTIVE-PLACE is the generalized variable where the active mode-line
is stored.

SAVED-PLACE is the generalized variable where the saved mode-line is
stored when hidden.

NONE is the symbol stored in ACTIVE-PLACE to hide the mode-line."
  `(if ,hide
       (ignore (cl-shiftf ,saved-place ,active-place ',none))
     (setf ,active-place (or (cl-shiftf ,saved-place nil)
                          mode-line-format
                          (default-value 'mode-line-format)
                          (error "No sane mode-line to show")))))

(defsubst exwm-layout--window-specific-mode-line (window)
  "Return non-nil if WINDOW has a WINDOW-specific mode-line."
  (or (window-parameter window 'mode-line-format)
      (window-parameter window 'exwm--saved-mode-line-format)))

(defsubst exwm-layout--mode-line (&optional window)
  "Return the effective mode-line for WINDOW or the current buffer.
Return nil if the buffer/window has no mode-line."
  (pcase (and window (window-parameter window 'mode-line-format))
    ('none nil)
    ('nil mode-line-format)
    (wml wml)))

(defun exwm-layout-toggle-mode-line (&optional arg)
  "Toggle the display of mode-line.

If ARG is a positive number, show the mode-line.
If ARG is a negative number, hide the mode-line.
Otherwise, toggle the mode-line.

If the mode-line format is specific to the current window (e.g., an
undecorated floating window), the mode-line is toggled in that window
only. Otherwise, it's toggled globally."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (exwm--log)
  (let* ((target-window
          (cond (exwm--floating-frame
                 (frame-first-window exwm--floating-frame))
                ((eq (window-buffer) (current-buffer))
                 (selected-window))))
         (is-visible
          (not (null (exwm-layout--mode-line target-window)))))
    (when (or (not (numberp arg)) (eq (< arg 0) is-visible))
      (let ((old-bottom-offset
             (and exwm--floating-frame
                  target-window
                  (exwm-layout--window-bottom-offset target-window))))
        (if (and target-window
                 (exwm-layout--window-specific-mode-line target-window))
            (exwm-layout--toggle-mode-line-1
             is-visible
             (window-parameter target-window
                               'mode-line-format)
             (window-parameter target-window
                               'exwm--saved-mode-line-format)
             none)
          (exwm-layout--toggle-mode-line-1
           is-visible
           mode-line-format
           exwm--saved-mode-line-format
           nil))
        (when old-bottom-offset
          (exwm-layout-enlarge-window
           (- (exwm-layout--window-bottom-offset target-window)
              old-bottom-offset))))
      (force-mode-line-update))))

(defun exwm-layout--minibuffer-leave-fullscreen ()
  "Leave fullscreen while the minibuffer is in use.
See `exwm-layout-minibuffer-unfullscreen'."
  (setq exwm-layout--minibuffer-fullscreen nil)
  (when exwm-layout-minibuffer-unfullscreen
    (dolist (pair exwm--id-buffer-alist)
      (with-current-buffer (cdr pair)
        (when (and (eq exwm--frame exwm-workspace--current)
                   (exwm-layout--fullscreen-p))
          (setq exwm--fullscreen-for-minibuffer t)
          (push exwm--id exwm-layout--minibuffer-fullscreen)
          (exwm-layout-unset-fullscreen exwm--id))))))

(defun exwm-layout--minibuffer-restore-fullscreen ()
  "Restore fullscreen left for the minibuffer."
  (dolist (id exwm-layout--minibuffer-fullscreen)
    (let ((buffer (exwm--id->buffer id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq exwm--fullscreen-for-minibuffer nil)
          (unless exwm--fullscreen-hold
            (exwm-layout-set-fullscreen id))))))
  (setq exwm-layout--minibuffer-fullscreen nil))

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (exwm--log)
  (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  (add-hook 'window-size-change-functions #'exwm-layout--refresh)
  (add-hook 'minibuffer-setup-hook #'exwm-layout--minibuffer-leave-fullscreen)
  (add-hook 'minibuffer-exit-hook #'exwm-layout--minibuffer-restore-fullscreen)
  (unless (exwm-workspace--minibuffer-own-frame-p)
    ;; Refresh when minibuffer grows
    (add-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup t)
    (setq exwm-layout--timer
          (run-with-idle-timer 0 t #'exwm-layout--on-echo-area-change t))
    (add-hook 'echo-area-clear-hook #'exwm-layout--on-echo-area-change)))

(defun exwm-layout--exit ()
  "Exit the layout module."
  (exwm--log)
  (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
  (remove-hook 'window-size-change-functions #'exwm-layout--refresh)
  (remove-hook 'minibuffer-setup-hook #'exwm-layout--minibuffer-leave-fullscreen)
  (remove-hook 'minibuffer-exit-hook #'exwm-layout--minibuffer-restore-fullscreen)
  (setq exwm-layout--minibuffer-fullscreen nil)
  (remove-hook 'minibuffer-setup-hook #'exwm-layout--on-minibuffer-setup)
  (when exwm-layout--timer
    (cancel-timer exwm-layout--timer)
    (setq exwm-layout--timer nil))
  (remove-hook 'echo-area-clear-hook #'exwm-layout--on-echo-area-change))

(provide 'exwm-layout)
;;; exwm-layout.el ends here
