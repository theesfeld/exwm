;;; exwm-systemtray.el --- System Tray Module for  -*- lexical-binding: t -*-
;;;                        EXWM

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

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

;; This module adds system tray support for EXWM.

;; To use this module, enable it as follows:
;;
;;   (exwm-systemtray-mode 1)

;;; Code:

(require 'xcb-ewmh)
(require 'xcb-icccm)
(require 'xcb-xembed)
(require 'xcb-systemtray)

(require 'exwm-core)
(require 'exwm-workspace)

(declare-function exwm-workspace--workarea "exwm-workspace.el" (frame))

(defclass exwm-systemtray--icon ()
  ((width :initarg :width)
   (height :initarg :height)
   (visible :initarg :visible))
  :documentation "Attributes of a system tray icon.")

(defgroup exwm-systemtray nil
  "System tray."
  :group 'exwm)

;;;###autoload
(define-minor-mode exwm-systemtray-mode
  "Toggle EXWM systemtray support."
  :global t
  :group 'exwm-systemtray
  (exwm--global-minor-mode-body systemtray))

(defcustom exwm-systemtray-height nil
  "System tray height.

You shall use the default value if using auto-hide minibuffer."
  :type 'integer)

(defcustom exwm-systemtray-icon-gap 2
  "Gap between icons."
  :type 'integer)

(defcustom exwm-systemtray-position 'bottom
  "Position of the systemtray at the top or bottom.
The position can be set to an integer.  In this case a
positive (negative) integer specifies the offset from the top (bottom)
of the screen."
  :type '(choice (const top) (const bottom) integer))

(defvar exwm-systemtray--connection nil "The X connection.")

(defvar exwm-systemtray--embedder-window nil "The embedder window.")
(defvar exwm-systemtray--embedder-window-depth nil
  "The embedder window's depth.")

(defcustom exwm-systemtray-workspace nil
  "Where to show the one system tray.
Nil follows the selected workspace.  An integer is a workspace
index.  A string is a RandR output name: the tray stays on the
visible workspace of that output.  XEmbed allows one tray, so
icons cannot be embedded on every workspace at once.  A separate
minibuffer frame keeps the tray on that frame either way."
  :type '(choice (const :tag "Follow the selected workspace" nil)
                 (integer :tag "Workspace index")
                 (string :tag "Monitor name"))
  :initialize #'custom-initialize-default
  :set #'exwm-systemtray--set-workspace)

(defcustom exwm-systemtray-background-color 'default
  "Background color of the system tray.
This should be a color, the symbol `transparent', or a face such
as `default' or `tab-bar'.  `workspace-background' is accepted
and treated as `default'.

`transparent' uses a ParentRelative background.  That is valid
only when the embedder's depth matches Emacs, as reported by
`x-display-planes'.  It shows the parent frame through the tray.
It is not alpha blending, and a 32-bit visual does not make the
icons transparent when the depths differ.  EXWM does not
composite.  The value is kept until both depths are known.  A
known mismatch falls back to `default'."
  :type '(choice (const :tag "ParentRelative" transparent)
                 (const :tag "Frame background" default)
                 (const :tag "Tab-bar background" tab-bar)
                 (color :tag "Color"))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (when (eq value 'workspace-background)
           (display-warning 'exwm-systemtray "Use `default' instead\
 of `workspace-background' for `exwm-systemtray-background-color'.")
           (setq value 'default))
         (when (and (eq value 'transparent)
                    (exwm-systemtray--transparency-depths)
                    (not (exwm-systemtray--transparency-supported-p)))
           (display-warning 'exwm-systemtray
                            "ParentRelative background needs the tray and \
Emacs to use the same visual depth.  Using `default' instead.")
           (setq value 'default))
         (set-default symbol value)
         (when (and exwm-systemtray-mode
                    exwm-systemtray--connection
                    exwm-systemtray--embedder-window)
           ;; Change the background color for embedder.
           (exwm-systemtray--set-background-color)
           ;; Unmap & map to take effect immediately.
           (xcb:+request exwm-systemtray--connection
                         (make-instance 'xcb:UnmapWindow
                                        :window exwm-systemtray--embedder-window))
           (xcb:+request exwm-systemtray--connection
                         (make-instance 'xcb:MapWindow
                                        :window exwm-systemtray--embedder-window))
           (xcb:flush exwm-systemtray--connection))))

(defvar exwm-systemtray-width 0
  "Current width of systemtray, which is computed when the icons are refreshed.
The width is useful to adjust the tab-bar alignment when
`exwm-systemtray-position' is set to `top'.")

;; GTK icons require at least 16 pixels to show normally.
(defconst exwm-systemtray--icon-min-size 16 "Minimum icon size.")

(defvar exwm-systemtray--list nil "The icon list.")

(defvar exwm-systemtray--selection-owner-window nil
  "The selection owner window.")

(defvar xcb:Atom:_NET_SYSTEM_TRAY_S0)

(defun exwm-systemtray--get-geometry (icon)
  "Return the geometry (width . height) of ICON.
The returned geometry will be scaled to fit the systemtray."
  (let (width* height*)
    (with-slots (width height)
        (xcb:+request-unchecked+reply exwm-systemtray--connection
            (make-instance 'xcb:GetGeometry :drawable icon))
      (setq height* exwm-systemtray-height
            width* (round (* width (/ (float height*) height))))
      (when (< width* exwm-systemtray--icon-min-size)
        (setq width* exwm-systemtray--icon-min-size
              height* (round (* height (/ (float width*) width)))))
      (exwm--log "Resize from %dx%d to %dx%d"
                 width height width* height*))
    (cons width* height*)))

(defun exwm-systemtray--embed (icon)
  "Embed an ICON."
  (exwm--log "Try to embed #x%x" icon)
  (when-let* ((info (xcb:+request-unchecked+reply
                        exwm-systemtray--connection
                        (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                       :window icon))))
    (exwm--log "Embed #x%x" icon)
    (pcase-let
        ((visible (if-let* ((flags (slot-value info 'flags)))
                      (/= 0 (logand flags xcb:xembed:MAPPED))
                    t)) ; default to visible.
         (`(,width . ,height)
          (exwm-systemtray--get-geometry icon)))
      ;; Add this icon to save-set.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ChangeSaveSet
                         :mode xcb:SetMode:Insert
                         :window icon))
      ;; Reparent to the embedder.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ReparentWindow
                         :window icon
                         :parent exwm-systemtray--embedder-window
                         :x 0
                         ;; Vertically centered.
                         :y (/ (- exwm-systemtray-height height) 2)))
      ;; Resize the icon.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ConfigureWindow
                         :window icon
                         :value-mask (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height
                                             xcb:ConfigWindow:BorderWidth)
                         :width width
                         :height height
                         :border-width 0))
      ;; Set event mask.
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window icon
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:ResizeRedirect
                                             xcb:EventMask:KeyPress
                                             xcb:EventMask:PropertyChange)))
      ;; Grab all keys and forward them to Emacs frame.
      (unless (exwm-workspace--minibuffer-own-frame-p)
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:GrabKey
                           :owner-events 0
                           :grab-window icon
                           :modifiers xcb:ModMask:Any
                           :key xcb:Grab:Any
                           :pointer-mode xcb:GrabMode:Async
                           :keyboard-mode xcb:GrabMode:Async)))
      (when visible
        (exwm--log "Map the window")
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:MapWindow :window icon)))
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:xembed:SendEvent
                         :destination icon
                         :event
                         (xcb:marshal
                          (make-instance 'xcb:xembed:EMBEDDED-NOTIFY
                                         :window icon
                                         :time xcb:Time:CurrentTime
                                         :embedder
                                         exwm-systemtray--embedder-window
                                         :version 0)
                          exwm-systemtray--connection)))
      (push `(,icon . ,(make-instance 'exwm-systemtray--icon
                                      :width width
                                      :height height
                                      :visible visible))
            exwm-systemtray--list)
      (exwm-systemtray--refresh))))

(defun exwm-systemtray--unembed (icon)
  "Unembed an ICON."
  (exwm--log "Unembed #x%x" icon)
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:UnmapWindow :window icon))
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:ReparentWindow
                     :window icon
                     :parent exwm--root
                     :x 0 :y 0))
  (setq exwm-systemtray--list
        (assq-delete-all icon exwm-systemtray--list))
  (exwm-systemtray--refresh))

(defun exwm-systemtray--refresh ()
  "Refresh the system tray."
  (exwm--log)
  ;; Make sure to redraw the embedder.
  (xcb:+request exwm-systemtray--connection
      (make-instance 'xcb:UnmapWindow
                     :window exwm-systemtray--embedder-window))
  (let ((x exwm-systemtray-icon-gap)
        map)
    (dolist (pair exwm-systemtray--list)
      (when (slot-value (cdr pair) 'visible)
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (car pair)
                           :value-mask xcb:ConfigWindow:X
                           :x x))
        (setq x (+ x (slot-value (cdr pair) 'width)
                   exwm-systemtray-icon-gap))
        (setq map t)))
    (let ((workarea (exwm-workspace--workarea exwm-workspace-current-index)))
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ConfigureWindow
                         :window exwm-systemtray--embedder-window
                         :value-mask (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Width)
                         :x (- (slot-value workarea 'width) x)
                         :width x)))
    (setq exwm-systemtray-width x)
    (when map
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:MapWindow
                         :window exwm-systemtray--embedder-window))))
  (xcb:flush exwm-systemtray--connection))

(defun exwm-systemtray--refresh-background-color (&optional remap)
  "Refresh background color after theme change or workspace switch.
If REMAP is not nil, map and unmap the embedder window so that the background is
redrawn."
  (when (facep exwm-systemtray-background-color)
    (exwm-systemtray--set-background-color)
    (when remap
      (xcb:+request exwm-systemtray--connection
                    (make-instance 'xcb:UnmapWindow
                                   :window exwm-systemtray--embedder-window))
      (xcb:+request exwm-systemtray--connection
                    (make-instance 'xcb:MapWindow
                                   :window exwm-systemtray--embedder-window))
      (xcb:flush exwm-systemtray--connection))))

(defun exwm-systemtray--set-background-color ()
  "Change the background color of the embedder.
The color is set according to `exwm-systemtray-background-color'.

Note that this function does not change the current contents of the embedder
window; unmap & map are necessary for the background color to take effect."
  (when (and exwm-systemtray--connection
             exwm-systemtray--embedder-window)
    (let* ((color (pcase exwm-systemtray-background-color
                    ((or `transparent `nil) ; nil means transparent as well
                     (cond
                      ((exwm-systemtray--transparency-supported-p) nil)
                      ((exwm-systemtray--transparency-depths)
                       (message "%s" "[EXWM] system tray ParentRelative background \
needs matching visual depths; using `default' instead")
                       (face-background 'default exwm-workspace--current))
                      (t
                       (face-background 'default exwm-workspace--current))))
                    ((pred facep)
                     (face-background exwm-systemtray-background-color
                                      exwm-workspace--current))
                    (_ exwm-systemtray-background-color)))
           (background-pixel (exwm--color->pixel color)))
      (xcb:+request exwm-systemtray--connection
                    (make-instance 'xcb:ChangeWindowAttributes
                                   :window exwm-systemtray--embedder-window
                                   ;; Either-or.  A `background-pixel' of nil
                                   ;; means simulate transparency.  We use
                                   ;; `xcb:CW:BackPixmap' together with
                                   ;; `xcb:BackPixmap:ParentRelative' do that,
                                   ;; but this only works when the parent
                                   ;; window's visual (Emacs') has the same
                                   ;; visual depth.
                                   :value-mask (if background-pixel
                                                   xcb:CW:BackPixel
                                                 xcb:CW:BackPixmap)
                                   ;; Due to the :value-mask above,
                                   ;; :background-pixmap only takes effect when
                                   ;; `transparent' is requested and supported
                                   ;; (visual depth of Emacs and of system tray
                                   ;; are equal).  Setting
                                   ;; `xcb:BackPixmap:ParentRelative' when
                                   ;; that's not the case would produce an
                                   ;; `xcb:Match' error.
                                   :background-pixmap xcb:BackPixmap:ParentRelative
                                   :background-pixel background-pixel)))))

(defun exwm-systemtray--transparency-depths (&optional emacs-planes
                                                       embedder-depth)
  "Return (PLANES . DEPTH) when both depths are known.
EMACS-PLANES defaults to `x-display-planes'.  EMBEDDER-DEPTH
defaults to `exwm-systemtray--embedder-window-depth'.  Return nil
when either value is missing or not a positive integer."
  (let ((planes (or emacs-planes
                    (and (display-graphic-p) (x-display-planes))))
        (depth (or embedder-depth exwm-systemtray--embedder-window-depth)))
    (and (integerp planes) (integerp depth)
         (> planes 0) (> depth 0)
         (cons planes depth))))

(defun exwm-systemtray--transparency-supported-p (&optional emacs-planes
                                                            embedder-depth)
  "Return non-nil when a ParentRelative tray background is valid.
EMACS-PLANES and EMBEDDER-DEPTH are as in
`exwm-systemtray--transparency-depths'.  The depths must be equal.
ParentRelative then shows the parent frame.  It does not blend
alpha, so a 32-bit visual is not transparency by itself.

Sections \"Visual and background pixmap handling\" and
\"_NET_SYSTEM_TRAY_VISUAL\" of the System Tray Protocol
Specification describe a real alpha visual.  This embedder is
created with the root visual, and EXWM does not composite."
  (let ((depths (exwm-systemtray--transparency-depths emacs-planes
                                                       embedder-depth)))
    (and depths (= (car depths) (cdr depths)))))

(defun exwm-systemtray--choose-frame (pin frames current)
  "Return the workspace frame that should parent the tray.
FRAMES is a list of (FRAME MONITOR ACTIVE) in workspace order.
PIN nil returns CURRENT.  An integer is that index, limited to
the existing frames.  A string is the active frame on that
monitor, or the first frame there, or else CURRENT."
  (cond
   ((null frames) current)
   ((integerp pin)
    (car (nth (max 0 (min pin (1- (length frames)))) frames)))
   ((and (stringp pin) (not (string-empty-p pin)))
    (or (car (cl-find-if (lambda (row)
                           (and (equal (nth 1 row) pin)
                                (nth 2 row)))
                         frames))
        (car (cl-find-if (lambda (row)
                           (equal (nth 1 row) pin))
                         frames))
        current))
   (t current)))

(defun exwm-systemtray--embedder-frame ()
  "Return the frame that currently parents the system tray."
  (if (exwm-workspace--minibuffer-own-frame-p)
      exwm-workspace--minibuffer
    (exwm-systemtray--choose-frame
     exwm-systemtray-workspace
     (mapcar (lambda (frame)
               (list frame
                     (frame-parameter frame 'exwm-randr-monitor)
                     (exwm-workspace--active-p frame)))
             exwm-workspace--list)
     exwm-workspace--current)))

(defun exwm-systemtray--set-workspace (symbol value)
  "Set SYMBOL to VALUE and move the tray if it is running."
  (set-default symbol value)
  (when (and exwm-systemtray--connection
             exwm-systemtray--embedder-window)
    (exwm-systemtray--on-workspace-switch)))

(defun exwm-systemtray--on-DestroyNotify (data _synthetic)
  "Unembed icons on DestroyNotify.
Argument DATA contains the raw event data."
  (exwm--log)
  (with-slots (window)
      (xcb:unmarshal-new 'xcb:DestroyNotify data)
    (when (assoc window exwm-systemtray--list)
      (exwm-systemtray--unembed window))))

(defun exwm-systemtray--on-ReparentNotify (data _synthetic)
  "Unembed icons on ReparentNotify.
Argument DATA contains the raw event data."
  (exwm--log)
  (with-slots (window parent)
      (xcb:unmarshal-new 'xcb:ReparentNotify data)
    (when (and (/= parent exwm-systemtray--embedder-window)
               (assoc window exwm-systemtray--list))
      (exwm-systemtray--unembed window))))

(defun exwm-systemtray--on-ResizeRequest (data _synthetic)
  "Resize the tray icon on ResizeRequest.
Argument DATA contains the raw event data."
  (exwm--log)
  (with-slots (window width height)
      (xcb:unmarshal-new 'xcb:ResizeRequest data)
    (when-let* ((attr (alist-get window exwm-systemtray--list)))
      (with-slots ((width* width) (height* height)) attr
        (setq height* exwm-systemtray-height
              width* (round (* width (/ (float height*) height))))
        (when (< width* exwm-systemtray--icon-min-size)
          (setq width* exwm-systemtray--icon-min-size
                height* (round (* height (/ (float width*) width)))))
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:ConfigureWindow
                           :window window
                           :value-mask (logior xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           ;; Vertically centered.
                           :y (/ (- exwm-systemtray-height height*) 2)
                           :width width*
                           :height height*)))
      (exwm-systemtray--refresh))))

(defun exwm-systemtray--on-PropertyNotify (data _synthetic)
  "Map/Unmap the tray icon on PropertyNotify.
Argument DATA contains the raw event data."
  (exwm--log)
  (with-slots (window atom state)
      (xcb:unmarshal-new 'xcb:PropertyNotify data)
    (when-let* ((_(eq state xcb:Property:NewValue))
                (_(eq atom xcb:Atom:_XEMBED_INFO))
                (attr (alist-get window exwm-systemtray--list))
                (info (xcb:+request-unchecked+reply
                          exwm-systemtray--connection
                          (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                         :window window))))
      (let ((visible (/= 0 (logand (slot-value info 'flags)
                                   xcb:xembed:MAPPED))))
        (exwm--log "#x%x visible? %s" window visible)
        (xcb:+request exwm-systemtray--connection
            (make-instance (if visible 'xcb:MapWindow 'xcb:UnmapWindow)
                           :window window))
        (setf (slot-value attr 'visible) visible)
        (exwm-systemtray--refresh)))))

(defun exwm-systemtray--on-ClientMessage (data _synthetic)
  "Handle client messages.
Argument DATA contains the raw event data."
  (let ((obj (xcb:unmarshal-new 'xcb:ClientMessage data)))
    (with-slots (window type data) obj
      (when (eq type xcb:Atom:_NET_SYSTEM_TRAY_OPCODE)
        (let* ((data32 (slot-value data 'data32))
               (opcode (elt data32 1)))
          (exwm--log "opcode: %s" opcode)
          (cond ((= opcode xcb:systemtray:opcode:REQUEST-DOCK)
                 (unless (assoc (elt data32 2) exwm-systemtray--list)
                   (exwm-systemtray--embed (elt data32 2))))
                ;; Not implemented (rarely used nowadays).
                ((or (= opcode xcb:systemtray:opcode:BEGIN-MESSAGE)
                     (= opcode xcb:systemtray:opcode:CANCEL-MESSAGE)))
                (t
                 (exwm--log "Unknown opcode message: %s" obj))))))))

(defun exwm-systemtray--on-KeyPress (data _synthetic)
  "Forward all KeyPress events to Emacs frame.
Argument DATA contains the raw event data."
  (exwm--log)
  ;; This function is only executed when there's no autohide minibuffer,
  ;; a workspace frame has the input focus and the pointer is over a
  ;; tray icon.
  (let ((dest (frame-parameter (selected-frame) 'exwm-outer-id))
        (obj (xcb:unmarshal-new 'xcb:KeyPress data)))
    (setf (slot-value obj 'event) dest)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination dest
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal obj exwm-systemtray--connection))))
  (xcb:flush exwm-systemtray--connection))

(defun exwm-systemtray--y-position (&optional frame)
  "Y position of the system tray on FRAME.
FRAME defaults to the frame that parents the tray."
  (let ((pos exwm-systemtray-position)
        (frame (or frame (exwm-systemtray--embedder-frame)
                   exwm-workspace--current)))
    (cond
     ((or (eq pos 'bottom) (and (fixnump pos) (<= pos 0)))
      ;; On GTK, pixel-height != outer-height. The frame-pixel-height is the height of the inner
      ;; frame window (corresponding to window-id) while the frame-outer-height corresponds to the
      ;; frame itself (outer-id). We embed relative to "window-id", so we need the pixel-height.
      (- (frame-pixel-height frame)
         exwm-systemtray-height
         (if (fixnump pos) (- pos) 0)))
     ((fixnump pos) pos)
     (t 0))))

(defun exwm-systemtray--on-workspace-switch ()
  "Reparent and refresh the system tray after a workspace change.
The parent is `exwm-systemtray-workspace' when that names a
workspace or monitor.  Otherwise it is the selected workspace.
A separate minibuffer frame keeps the parent it was given."
  (exwm--log)
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (let ((frame (exwm-systemtray--embedder-frame)))
      (when (and frame (frame-live-p frame)
                 (frame-parameter frame 'window-id))
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:ReparentWindow
                           :window exwm-systemtray--embedder-window
                           :parent (string-to-number
                                    (frame-parameter frame 'window-id))
                           :x 0
                           :y (exwm-systemtray--y-position frame))))))
  (exwm-systemtray--refresh-background-color)
  (exwm-systemtray--refresh))

(defun exwm-systemtray--on-theme-change (_theme)
  "Refresh system tray upon theme change."
  (exwm-systemtray--refresh-background-color 'remap))

(defun exwm-systemtray--refresh-all ()
  "Reposition/Refresh the system tray."
  (exwm--log)
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (let ((frame (exwm-systemtray--embedder-frame)))
      (when (and frame (frame-live-p frame))
        (xcb:+request exwm-systemtray--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm-systemtray--embedder-window
                           :value-mask xcb:ConfigWindow:Y
                           :y (exwm-systemtray--y-position frame))))))
  (exwm-systemtray--refresh))

(cl-defun exwm-systemtray--init ()
  "Initialize system tray module."
  (exwm--log)
  ;; idempotent initialization
  (when exwm-systemtray--connection
    (cl-return-from exwm-systemtray--init))
  (cl-assert (not exwm-systemtray--list))
  (cl-assert (not exwm-systemtray--selection-owner-window))
  (cl-assert (not exwm-systemtray--embedder-window))
  (unless exwm-systemtray-height
    (setq exwm-systemtray-height (max exwm-systemtray--icon-min-size
                                      (with-selected-window (minibuffer-window)
                                        (line-pixel-height)))))
  ;; Create a new connection.
  (setq exwm-systemtray--connection (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-systemtray--connection
                                              'process)
                                  nil)
  ;; Initialize XELB modules.
  (xcb:xembed:init exwm-systemtray--connection t)
  (xcb:systemtray:init exwm-systemtray--connection t)
  ;; Acquire the manager selection _NET_SYSTEM_TRAY_S0.
  (with-slots (owner)
      (xcb:+request-unchecked+reply exwm-systemtray--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:_NET_SYSTEM_TRAY_S0))
    (when (/= owner xcb:Window:None)
      (xcb:disconnect exwm-systemtray--connection)
      (setq exwm-systemtray--connection nil)
      (warn "[EXWM] Other system tray detected")
      (cl-return-from exwm-systemtray--init)))
  (let ((id (xcb:generate-id exwm-systemtray--connection)))
    (setq exwm-systemtray--selection-owner-window id)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid id
                       :parent exwm--root
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Get the selection ownership.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SetSelectionOwner
                       :owner id
                       :selection xcb:Atom:_NET_SYSTEM_TRAY_S0
                       :time xcb:Time:CurrentTime))
    ;; Send a client message to announce the selection.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination exwm--root
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:icccm:-ManagerSelection
                                              :window exwm--root
                                              :time xcb:Time:CurrentTime
                                              :selection
                                              xcb:Atom:_NET_SYSTEM_TRAY_S0
                                              :owner id)
                               exwm-systemtray--connection)))
    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "EXWM: exwm-systemtray--selection-owner-window"))
    ;; Set the _NET_SYSTEM_TRAY_ORIENTATION property.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:xembed:set-_NET_SYSTEM_TRAY_ORIENTATION
                       :window id
                       :data xcb:systemtray:ORIENTATION:HORZ)))
  ;; Create the embedder.
  (let ((id (xcb:generate-id exwm-systemtray--connection))
        frame parent embedder-depth embedder-visual embedder-colormap y)
    (setq exwm-systemtray--embedder-window id)
    (if (exwm-workspace--minibuffer-own-frame-p)
        (setq frame exwm-workspace--minibuffer
              y (if (>= (line-pixel-height) exwm-systemtray-height)
                    ;; Bottom aligned.
                    (- (line-pixel-height) exwm-systemtray-height)
                  ;; Vertically centered.
                  (/ (- (line-pixel-height) exwm-systemtray-height) 2)))
      (setq frame (or (exwm-systemtray--embedder-frame)
                      exwm-workspace--current)
            ;; Bottom aligned.
            y (exwm-systemtray--y-position frame)))
    (setq parent (string-to-number (frame-parameter frame 'window-id)))
    ;; Use default depth, visual and colormap (from root window), instead of
    ;; Emacs frame's.  See Section "Visual and background pixmap handling" in
    ;; "System Tray Protocol Specification 0.3".
    (let* ((vdc (exwm--get-visual-depth-colormap exwm-systemtray--connection
                                                 exwm--root)))
      (setq embedder-visual (car vdc))
      (setq embedder-depth (cadr vdc))
      (setq embedder-colormap (caddr vdc)))
    ;; Note down the embedder window's depth.  It will be used to check whether
    ;; we can use xcb:BackPixmap:ParentRelative to emulate transparency.
    (setq exwm-systemtray--embedder-window-depth embedder-depth)
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:CreateWindow
                       :depth embedder-depth
                       :wid id
                       :parent parent
                       :x 0
                       :y y
                       :width 1
                       :height exwm-systemtray-height
                       :border-width 0
                       :class xcb:WindowClass:InputOutput
                       :visual embedder-visual
                       :colormap embedder-colormap
                       :value-mask (logior xcb:CW:BorderPixel
                                           xcb:CW:Colormap
                                           xcb:CW:EventMask)
                       :border-pixel 0
                       :event-mask xcb:EventMask:SubstructureNotify))
    (exwm-systemtray--set-background-color)
    ;; Set _NET_WM_NAME.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "EXWM: exwm-systemtray--embedder-window"))
    ;; Set _NET_WM_WINDOW_TYPE.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                       :window id
                       :data (vector xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK)))
    ;; Set _NET_SYSTEM_TRAY_VISUAL.
    (xcb:+request exwm-systemtray--connection
        (make-instance 'xcb:xembed:set-_NET_SYSTEM_TRAY_VISUAL
                       :window exwm-systemtray--selection-owner-window
                       :data embedder-visual)))
  (xcb:flush exwm-systemtray--connection)
  ;; Attach event listeners.
  (xcb:+event exwm-systemtray--connection 'xcb:DestroyNotify
              #'exwm-systemtray--on-DestroyNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ReparentNotify
              #'exwm-systemtray--on-ReparentNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ResizeRequest
              #'exwm-systemtray--on-ResizeRequest)
  (xcb:+event exwm-systemtray--connection 'xcb:PropertyNotify
              #'exwm-systemtray--on-PropertyNotify)
  (xcb:+event exwm-systemtray--connection 'xcb:ClientMessage
              #'exwm-systemtray--on-ClientMessage)
  (unless (exwm-workspace--minibuffer-own-frame-p)
    (xcb:+event exwm-systemtray--connection 'xcb:KeyPress
                #'exwm-systemtray--on-KeyPress))
  ;; Add hook to move/reparent the embedder.
  (add-hook 'exwm-workspace-switch-hook #'exwm-systemtray--on-workspace-switch)
  (add-hook 'exwm-workspace--update-workareas-hook
            #'exwm-systemtray--refresh-all)
  ;; Add hook to update background colors.
  (add-hook 'enable-theme-functions #'exwm-systemtray--on-theme-change)
  (add-hook 'disable-theme-functions #'exwm-systemtray--on-theme-change)
  (add-hook 'menu-bar-mode-hook #'exwm-systemtray--refresh-all)
  (add-hook 'tool-bar-mode-hook #'exwm-systemtray--refresh-all)
  (when (boundp 'exwm-randr-refresh-hook)
    (add-hook 'exwm-randr-refresh-hook #'exwm-systemtray--on-workspace-switch))
  ;; The struts can be updated already.
  (when exwm-workspace--workareas
    (exwm-systemtray--refresh-all)))

(defun exwm-systemtray--exit ()
  "Exit the systemtray module."
  (exwm--log)
  (when exwm-systemtray--connection
    (when (slot-value exwm-systemtray--connection 'connected)
      ;; Hide & reparent out the embedder before disconnection to prevent
      ;; embedded icons from being reparented to an Emacs frame (which is the
      ;; parent of the embedder).
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:UnmapWindow
                         :window exwm-systemtray--embedder-window))
      (xcb:+request exwm-systemtray--connection
          (make-instance 'xcb:ReparentWindow
                         :window exwm-systemtray--embedder-window
                         :parent exwm--root
                         :x 0
                         :y 0))
      (xcb:disconnect exwm-systemtray--connection))
    (setq exwm-systemtray--connection nil
          exwm-systemtray--list nil
          exwm-systemtray--selection-owner-window nil
          exwm-systemtray--embedder-window nil
          exwm-systemtray--embedder-window-depth nil)
    (remove-hook 'exwm-workspace-switch-hook
                 #'exwm-systemtray--on-workspace-switch)
    (remove-hook 'exwm-workspace--update-workareas-hook
                 #'exwm-systemtray--refresh-all)
    (remove-hook 'enable-theme-functions #'exwm-systemtray--on-theme-change)
    (remove-hook 'disable-theme-functions #'exwm-systemtray--on-theme-change)
    (remove-hook 'menu-bar-mode-hook #'exwm-systemtray--refresh-all)
    (remove-hook 'tool-bar-mode-hook #'exwm-systemtray--refresh-all)
    (when (boundp 'exwm-randr-refresh-hook)
      (remove-hook 'exwm-randr-refresh-hook
                   #'exwm-systemtray--on-workspace-switch))))

(provide 'exwm-systemtray)
;;; exwm-systemtray.el ends here
