;;; exwm-floating.el --- Floating Module for EXWM  -*- lexical-binding: t -*-

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

;; This module deals with the conversion between floating and non-floating
;; states and implements moving/resizing operations on floating windows.
;; An ordinary Emacs buffer floats in its own frame.  That path does not
;; set `exwm--floating-frame', which means an X client.

;;; Code:

(require 'xcb-cursor)
(require 'exwm-core)

(defvar exwm-manage--display-window)

(defvar exwm-floating--emacs-frames nil
  "Floating frames that display ordinary Emacs buffers.
Each frame is reparented into an override-redirect container, the
same arrangement as a floating X window.  The frame parameter
`exwm-floating-emacs' is t.  `exwm--floating-frame' stays nil:
that buffer-local variable means a managed X client.")

(defconst exwm-floating--emacs-park -10000
  "Root position used to park a floating Emacs frame.
The frame is not resized.  The floating X window park of -101
would leave most of a large frame on screen.")

(defgroup exwm-floating nil
  "Floating."
  :group 'exwm)

(defcustom exwm-floating-setup-hook nil
  "Normal hook run when an X window has been made floating.
This hook runs in the context of the corresponding buffer."
  :type 'hook)

(defcustom exwm-floating-exit-hook nil
  "Normal hook run when an X window has exited floating state.
This hook runs in the context of the corresponding buffer."
  :type 'hook)

(defcustom exwm-floating-border-color "navy"
  "Border color of floating windows."
  :type 'color
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (when exwm--connection
           (exwm-floating-refresh-borders))))

(defcustom exwm-floating-border-color-focused nil
  "Border color of the focused floating window.
When nil, every floating window uses `exwm-floating-border-color'."
  :type '(choice (const :tag "Same as other windows" nil)
                 (color :tag "Focused window"))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (when exwm--connection
           (exwm-floating-refresh-borders))))

(defun exwm-floating-refresh-borders ()
  "Paint floating-window borders for the current focus.
With `exwm-floating-border-color-focused' nil, every window uses
`exwm-floating-border-color'."
  (when exwm--connection
    (let* ((focus-color exwm-floating-border-color-focused)
           (normal (exwm--color->pixel exwm-floating-border-color))
           (focus (exwm--color->pixel
                   (or focus-color exwm-floating-border-color))))
      (dolist (pair exwm--id-buffer-alist)
        (with-current-buffer (cdr pair)
          (when exwm--floating-frame
            (let* ((window (get-buffer-window nil t))
                   (pixel (if (and focus-color
                                   window
                                   (eq window (selected-window)))
                              focus
                            normal))
                   (container (frame-parameter exwm--floating-frame
                                               'exwm-container)))
              (when (and pixel container)
                (xcb:+request exwm--connection
                    (make-instance 'xcb:ChangeWindowAttributes
                                   :window container
                                   :value-mask xcb:CW:BorderPixel
                                   :border-pixel pixel)))))))
      (dolist (frame exwm-floating--emacs-frames)
        (when (and (frame-live-p frame)
                   (frame-parameter frame 'exwm-floating-emacs))
          (let ((pixel (if (and focus-color
                                (eq frame (selected-frame)))
                           focus
                         normal))
                (container (frame-parameter frame 'exwm-container)))
            (when (and pixel container)
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ChangeWindowAttributes
                                 :window container
                                 :value-mask xcb:CW:BorderPixel
                                 :border-pixel pixel))))))
      (xcb:flush exwm--connection))))

(defcustom exwm-floating-border-width 1
  "Border width of floating windows."
  :type `(integer
          :validate ,(lambda (widget)
                       (when (< (widget-value widget) 0)
                         (widget-put widget :error "Border width is at least 0")
                         widget)))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (let ((delta (- value exwm-floating-border-width))
               container)
           (set-default symbol value)
           ;; Change border width for all floating X windows.
           (dolist (pair exwm--id-buffer-alist)
             (with-current-buffer (cdr pair)
               (when exwm--floating-frame
                 (setq container (frame-parameter exwm--floating-frame
                                                  'exwm-container))
                 (with-slots (x y)
                     (xcb:+request-unchecked+reply exwm--connection
                         (make-instance 'xcb:GetGeometry
                                        :drawable container))
                   (xcb:+request exwm--connection
                       (make-instance 'xcb:ConfigureWindow
                                      :window container
                                      :value-mask
                                      (logior xcb:ConfigWindow:X
                                              xcb:ConfigWindow:Y
                                              xcb:ConfigWindow:BorderWidth)
                                      :border-width value
                                      :x (- x delta)
                                      :y (- y delta)))))))
           (dolist (frame exwm-floating--emacs-frames)
             (when (and exwm--connection
                        (frame-live-p frame)
                        (setq container (frame-parameter frame 'exwm-container)))
               (let ((reply (xcb:+request-unchecked+reply
                                exwm--connection
                                (make-instance 'xcb:GetGeometry
                                               :drawable container))))
                 (when reply
                   (with-slots (x y) reply
                     (xcb:+request exwm--connection
                         (make-instance 'xcb:ConfigureWindow
                                        :window container
                                        :value-mask (logior xcb:ConfigWindow:X
                                                            xcb:ConfigWindow:Y
                                                            xcb:ConfigWindow:BorderWidth)
                                        :border-width value
                                        :x (- x delta)
                                        :y (- y delta))))))))
           (when exwm--connection
             (xcb:flush exwm--connection)))))

(defcustom exwm-floating-keyboard-step 16
  "Pixels to move or resize a floating window from the keyboard."
  :type 'integer)

;; Cursors for moving/resizing a window
(defvar exwm-floating--cursor-move nil)
(defvar exwm-floating--cursor-top-left nil)
(defvar exwm-floating--cursor-top nil)
(defvar exwm-floating--cursor-top-right nil)
(defvar exwm-floating--cursor-right nil)
(defvar exwm-floating--cursor-bottom-right nil)
(defvar exwm-floating--cursor-bottom nil)
(defvar exwm-floating--cursor-bottom-left nil)
(defvar exwm-floating--cursor-left nil)

(defvar exwm-floating--moveresize-calculate nil
  "Calculate move/resize parameters [buffer event-mask x y width height].")

(defvar exwm-workspace--current)
(declare-function exwm-layout--hide "exwm-layout.el" (id))
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--refresh "exwm-layout.el" ())
(declare-function exwm-layout--show "exwm-layout.el" (id &optional window))
(declare-function exwm-workspace--active-p "exwm-workspace.el" (frame))
(declare-function exwm-workspace--position "exwm-workspace.el" (frame))
(declare-function exwm-workspace--raise-child-frames "exwm-workspace.el" ())
(declare-function exwm-workspace--workarea "exwm-workspace.el" (frame))
(declare-function exwm-workspace--workspace-p "exwm-workspace.el" (frame))

(defun exwm-floating--set-allowed-actions (id tiled-p)
  "Set _NET_WM_ALLOWED_ACTIONS for window with ID.
If TILED-P is non-nil, set actions for tiled window."
  (exwm--log "#x%x" id)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_ALLOWED_ACTIONS
                     :window id
                     :data (if tiled-p
                               (vector xcb:Atom:_NET_WM_ACTION_MINIMIZE
                                       xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                                       xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                                       xcb:Atom:_NET_WM_ACTION_CLOSE)
                             (vector xcb:Atom:_NET_WM_ACTION_MOVE
                                     xcb:Atom:_NET_WM_ACTION_RESIZE
                                     xcb:Atom:_NET_WM_ACTION_MINIMIZE
                                     xcb:Atom:_NET_WM_ACTION_FULLSCREEN
                                     xcb:Atom:_NET_WM_ACTION_CHANGE_DESKTOP
                                     xcb:Atom:_NET_WM_ACTION_CLOSE)))))

(defun exwm-floating--configured-dimension (dimension relative-to)
  "Return the user's configured floating DIMENSION.
The dimension is scaled relative to RELATIVE-TO if it's a floating point
value between 0 and 1 inclusive.

DIMENSION must be one of `x', `y', `width', or `height'.

Return nil if the user hasn't configured the dimension and/or if the
configured dimension is invalid."
  (when-let* ((val (plist-get exwm--configurations dimension)))
    (cond
     ((integerp val) val)
     ((and (floatp val) (>= 1 val 0))
      (round (* val relative-to)))
     (t (warn "EXWM: window configuration for `%s' has an invalid `%S' value `%S'"
              (buffer-name) dimension val)
        nil))))

(defvar exwm-floating--user nil
  "Non-nil while the user, rather than a client, asks to float.")

(defvar-local exwm--saved-tab-line-format nil
  "Value of `tab-line-format' saved while this buffer floats.")

(defvar-local exwm--tab-line-hidden-for-float nil
  "Non-nil when floating cleared `tab-line-format' for this buffer.")

(cl-defun exwm-floating--set-floating (id)
  "Make window ID floating.
A buffer with `exwm--stay-tiled' set stays tiled unless
`exwm-floating--user' is non-nil."
  (when (with-current-buffer (exwm--id->buffer id)
          (and exwm--stay-tiled (not exwm-floating--user)))
    (exwm--log "#x%x stays tiled" id)
    (cl-return-from exwm-floating--set-floating))
  ;; `select-frame-set-input-focus' warps the pointer when either of
  ;; these is set.  The frame is still off-screen until the XELB
  ;; geometry requests are flushed, so the warp lands on the edge of
  ;; the screen.
  (let ((focus-follows-mouse nil)
        (mouse-autoselect-window nil))
    ;; Hide the non-floating X window first.
    (replace-buffer-in-windows (exwm--id->buffer id))
    (with-current-buffer (exwm--id->buffer id)
      (let* ((frame (make-frame
                     `((minibuffer . ,(minibuffer-window exwm--frame))
                       (tab-bar-lines . 0)
                       (tab-bar-lines-keep-state . t)
                       ;; We move the frame off-screen to prevent "flashes" of
                       ;; visibility. No amount of inhibiting refresh, redisplay,
                       ;; etc. seems to prevent that.
                       ;;
                       ;; Additionally, we need the frame visible to correctly
                       ;; adjust its size.
                       (left . ,(* window-min-width -10000))
                       (top . ,(* window-min-height -10000))
                       (width . ,window-min-width)
                       (height . ,window-min-height)
                       (unsplittable . t))))
             (outer-id (string-to-number (frame-parameter frame 'outer-window-id)))
             (window-id (string-to-number (frame-parameter frame 'window-id)))
             (frame-container (xcb:generate-id exwm--connection))
             ; The floating frame has only one window.
             (window (frame-first-window frame))
             (border-pixel (exwm--color->pixel exwm-floating-border-color))
             (border-width (let ((border-width (plist-get exwm--configurations
                                                          'border-width)))
                             (if (and (integerp border-width)
                                      (>= border-width 0))
                                 border-width
                               exwm-floating-border-width)))
             (x (slot-value exwm--geometry 'x))
             (y (slot-value exwm--geometry 'y))
             (width (slot-value exwm--geometry 'width))
             (height (slot-value exwm--geometry 'height)))
        (exwm--log "Floating geometry (requested): %dx%d%+d%+d" width height x y)

        ;; Save frame parameters.
        (modify-frame-parameters frame `((exwm-outer-id . ,outer-id)
                                         (exwm-id . ,window-id)
                                         (exwm-container . ,frame-container)))

        ;; Configure the new window. The X window's buffer will already be
        ;; displayed in this window as it was current when we created the
        ;; floating frame.
        (set-window-dedicated-p window t)
        (set-window-parameter window 'split-window
                              (lambda (&rest _) (user-error "Floating window cannot be split")))
        (setq window-size-fixed exwm--fixed-size
              exwm--floating-frame frame)

        ;; Adjust the header & mode line before calculating sizes.
        (pcase-dolist (`(,prop . ,setting) '((mode-line-format . floating-mode-line)
                                             (header-line-format . floating-header-line)))
          (cond
           ((plist-member exwm--configurations setting)
            (set-window-parameter
             window prop
             (or (plist-get exwm--configurations setting) 'none)))
           ((not exwm--mwm-hints-decorations)
            (set-window-parameter window prop 'none))))

        ;; A tab line is not part of the floating inset.  Leaving it
        ;; up covers the client and shifts the border.  The tiling
        ;; window is a different window, so restore the variable when
        ;; this buffer leaves the floating frame.
        (unless exwm--tab-line-hidden-for-float
          (setq exwm--saved-tab-line-format tab-line-format
                exwm--tab-line-hidden-for-float t)
          (setq-local tab-line-format nil))

        ;; We MUST redisplay with the frame visible here in order to correctly calculate the sizes.
        (redisplay)

        ;; Adjust to container to fit the screen, centering dialogs, adjusting for the frame/borders,
        ;; and applying user size adjustments.
        ;; FIXME: check normal hints restrictions
        (with-slots ((screen-x x) (screen-y y) (screen-width width) (screen-height height))
            (exwm-workspace--workarea exwm--frame)

          ;; Fix invalid width/height.
          (when (= 0 width) (setq width (/ screen-width 2)))
          (when (= 0 height) (setq height (/ screen-height 2)))

          ;; Center floating windows unless they have explicit positions.
          (when (and (or (= x 0) (= x screen-x))
                     (or (= y 0) (= y screen-y)))
            (if-let* ((parent-buffer (exwm--id->buffer exwm-transient-for))
                      (parent-window (get-buffer-window parent-buffer))
                      (parent-edges (exwm--window-inside-absolute-pixel-edges parent-window))
                      (parent-x (elt parent-edges 0))
                      (parent-y (elt parent-edges 1))
                      (parent-width (- (elt parent-edges 2) parent-x))
                      (parent-height (- (elt parent-edges 3) parent-y))
                      ((and (<= width parent-width) (<= height parent-height))))
                ;; Put at the center of leading window
                (setq x (+ parent-x (/ (- parent-width  width) 2))
                      y (+ parent-y (/ (- parent-height height) 2)))
              ;; Put at the center of screen
              (setq x (+ screen-x (/ (- screen-width width) 2))
                    y (+ screen-y (/ (- screen-height height) 2)))))

          ;; Translate the window size hints into the correct container size.
          ;; But avoid moving the window border off-screen in the process.
          (let* ((outer-edges (frame-edges frame 'outer-edges))
                 (window-edges (exwm--window-inside-absolute-pixel-edges window))
                 (offset-left (- (elt window-edges 0) (elt outer-edges 0)))
                 (offset-right (- (elt outer-edges 2) (elt window-edges 2)))
                 (offset-top (- (elt window-edges 1) (elt outer-edges 1)))
                 (offset-bottom (- (elt outer-edges 3) (elt window-edges 3)))
                 (new-x (- x offset-left border-width))
                 (new-y (- y offset-top border-width)))
            (set-frame-parameter frame 'exwm-floating-inset
                                 (list offset-left offset-top
                                       offset-right offset-bottom))
            ;; Update the x/y but avoid moving the frame off-screen if it was previously on-screen.
            (when (or (<= screen-x new-x) (< x screen-x)) (setq x new-x))
            (when (or (<= screen-y new-y) (< y screen-y)) (setq y new-y))
            ;; Always update the width/height.
            (setq height (+ height offset-top offset-bottom)
                  width (+ width offset-left offset-right)))

          ;; Make it fit on the screen.
          (cond
           ;; Too wide
           ((> width screen-width)
            (setq x screen-x width screen-width))
           ;; Make sure at least half of the window is visible
           ((not (< screen-x (+ x (/ width 2)) (+ screen-x screen-width)))
            (setq x (+ screen-x (/ (- screen-width width) 2)))))
          (cond
           ;; Too tall
           ((> height screen-height)
            (setq y screen-y height screen-height))
           ;; Make sure at least half of the window is visible
           ((not (< screen-y (+ y (/ height 2)) (+ screen-y screen-height)))
            (setq y (+ screen-y (/ (- screen-height height) 2)))))

          ;; Apply user configuration.
          (when-let* ((user-x (exwm-floating--configured-dimension 'x screen-x)))
            (setq x (+ screen-x user-x)))
          (when-let* ((user-y (exwm-floating--configured-dimension 'y screen-y)))
            (setq y (+ screen-y user-y)))
          (when-let* ((user-width (exwm-floating--configured-dimension 'width screen-width)))
            (setq width (max 1 user-width)))
          (when-let* ((user-height (exwm-floating--configured-dimension 'height screen-height)))
            (setq height (max 1 user-height))))

        (exwm--log "Floating geometry (final): %dx%d%+d%+d" width height x y)

        ;; DO NOT USE set-frame-size. Emacs will mess up the size.
        (exwm--set-geometry outer-id x y width height)

        ;; Create the frame container as the parent of the frame.
        (xcb:+request exwm--connection
            (make-instance 'xcb:CreateWindow
                           :depth 0
                           :wid frame-container
                           :parent exwm--root
                           :x x
                           :y y
                           :width width
                           :height height
                           :border-width border-width
                           :class xcb:WindowClass:InputOutput
                           :visual 0
                           :value-mask (logior xcb:CW:BackPixmap
                                               (if border-pixel
                                                   xcb:CW:BorderPixel 0)
                                               xcb:CW:OverrideRedirect)
                           :background-pixmap xcb:BackPixmap:ParentRelative
                           :border-pixel border-pixel
                           :override-redirect 1))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                           :window frame-container
                           :data
                           (format "EXWM floating frame container for 0x%x" id)))
        ;; Map it.
        (xcb:+request exwm--connection
            (make-instance 'xcb:MapWindow :window frame-container))
        ;; Put the X window right above this frame container.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window id
                           :value-mask (logior xcb:ConfigWindow:Sibling
                                               xcb:ConfigWindow:StackMode)
                           :sibling frame-container
                           :stack-mode xcb:StackMode:Above))
        ;; Reparent this frame to its container.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ReparentWindow
                           :window outer-id :parent frame-container :x 0 :y 0))
        ;; Switch from tiling to floating actions.
        (exwm-floating--set-allowed-actions id nil)
        ;; Finally, focus the frame.  A startup notification for a
        ;; workspace the user has left must not pull focus back.
        (unless exwm-manage--display-window
          (select-frame-set-input-focus frame))
        ;; Flush everything.
        (xcb:flush exwm--connection)
        ;; Update the layout.
        (exwm-layout--show id window))
      (run-hooks 'exwm-floating-setup-hook))))

(defun exwm-floating--unset-floating (id)
  "Make window ID non-floating."
  (exwm--log "#x%x" id)
  (let ((buffer (exwm--id->buffer id)))
    (with-current-buffer buffer
      (when exwm--floating-frame
        (when exwm--tab-line-hidden-for-float
          (setq-local tab-line-format exwm--saved-tab-line-format)
          (setq exwm--tab-line-hidden-for-float nil
                exwm--saved-tab-line-format nil))
        ;; The X window is already mapped.
        ;; Unmap the X window.
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
        ;; Reparent the floating frame back to the root window.
        (let ((frame-id (frame-parameter exwm--floating-frame 'exwm-outer-id))
              (frame-container (frame-parameter exwm--floating-frame
                                                'exwm-container)))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UnmapWindow :window frame-id))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window frame-id
                             :parent exwm--root
                             :x 0 :y 0))
          ;; Also destroy its container.
          (xcb:+request exwm--connection
              (make-instance 'xcb:DestroyWindow :window frame-container))))
      ;; Place the X window just above the reference X window.
      ;; (the stacking order won't change from now on).
      ;; Also hide the possible floating border.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:BorderWidth
                                             xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :border-width 0
                         :sibling exwm--guide-window
                         :stack-mode xcb:StackMode:Above)))
    (exwm-floating--set-allowed-actions id t)
    (xcb:flush exwm--connection)
    (with-current-buffer buffer
      (when exwm--floating-frame        ;from floating to non-floating
        (set-window-dedicated-p (frame-first-window exwm--floating-frame) nil)
        ;; Select a tiling window and delete the old frame.
        (unless exwm-manage--display-window
          (select-window (frame-selected-window exwm-workspace--current)))
        (with-current-buffer buffer
          (delete-frame exwm--floating-frame))))
    (with-current-buffer buffer
      (setq window-size-fixed nil
            exwm--floating-frame nil))
    ;; Only show X windows in normal state.
    (unless (exwm-layout--iconic-state-p)
      (if (and exwm-manage--display-window
               (window-live-p exwm-manage--display-window))
          (set-window-buffer exwm-manage--display-window buffer)
        (pop-to-buffer-same-window buffer))))
  (with-current-buffer (exwm--id->buffer id)
    (run-hooks 'exwm-floating-exit-hook)))

(defun exwm-floating--emacs-container-geometry
    (source-x source-y chrome-left chrome-top outer-width outer-height
     area-x area-y area-width area-height)
  "Return (X Y WIDTH HEIGHT) for a floating Emacs frame container.
SOURCE-X and SOURCE-Y are the source window origin in root pixels.
CHROME-LEFT and CHROME-TOP are the pixels between the outer frame
and that window, so the window stays on the source.  OUTER-WIDTH
and OUTER-HEIGHT are the frame's outer size.  When the area
arguments are numbers, the origin moves so at least half of the
container stays inside that workarea.  A container larger than
the workarea is placed on the workarea origin."
  (let ((x (- source-x (or chrome-left 0)))
        (y (- source-y (or chrome-top 0)))
        (width (max 1 (or outer-width 1)))
        (height (max 1 (or outer-height 1))))
    (when (and (numberp area-x) (numberp area-y)
               (numberp area-width) (numberp area-height)
               (> area-width 0) (> area-height 0))
      (if (> width area-width)
          (setq x area-x)
        (unless (< area-x (+ x (/ width 2)) (+ area-x area-width))
          (setq x (+ area-x (/ (- area-width width) 2)))))
      (if (> height area-height)
          (setq y area-y)
        (unless (< area-y (+ y (/ height 2)) (+ area-y area-height))
          (setq y (+ area-y (/ (- area-height height) 2))))))
    (list x y width height)))

(defun exwm-floating--emacs-park-geometry (x y width height saved)
  "Return the geometry list to keep when parking a frame.
SAVED is an existing (X Y WIDTH HEIGHT) list and wins, so a park
echo cannot replace the on-screen position.  A 1x1 rectangle is
the floating X window park size and is not a frame position.
Return nil when there is nothing to store."
  (or saved
      (and (numberp x) (numberp y) (numberp width) (numberp height)
           (> width 1) (> height 1)
           (not (and (= x exwm-floating--emacs-park)
                     (= y exwm-floating--emacs-park)))
           (list x y width height))))

(defun exwm-floating--emacs-frame-showing (buffer)
  "Return a floating Emacs frame that shows BUFFER, or nil."
  (catch 'found
    (dolist (frame exwm-floating--emacs-frames)
      (when (and (frame-live-p frame)
                 (frame-parameter frame 'exwm-floating-emacs)
                 (get-buffer-window buffer frame))
        (throw 'found frame)))))

(defun exwm-floating--buffer-workspace ()
  "Return the workspace that should own a new floating Emacs frame."
  (let ((frame (window-frame (selected-window))))
    (cond
     ((exwm-workspace--workspace-p frame) frame)
     ((frame-parameter frame 'exwm-floating-workspace))
     (t exwm-workspace--current))))

(defun exwm-floating--measure-frame (frame)
  "Return (CHROME-LEFT CHROME-TOP OUTER-WIDTH OUTER-HEIGHT) for FRAME.
Call this before the frame is reparented into its container.
After that reparent, `frame-edges' walks up to the container and
no longer describes the Emacs window."
  (let ((outer (frame-edges frame 'outer-edges))
        (text (window-absolute-pixel-edges (frame-first-window frame))))
    (when (and (consp outer) (consp text))
      (list (max 0 (- (nth 0 text) (nth 0 outer)))
            (max 0 (- (nth 1 text) (nth 1 outer)))
            (max 1 (- (nth 2 outer) (nth 0 outer)))
            (max 1 (- (nth 3 outer) (nth 1 outer)))))))

(defun exwm-floating--fit-emacs-container (frame)
  "Give FRAME's container the same size as FRAME's outer X window.
The container position is left alone.  The size is read from the
Emacs window, not from `frame-outer-width': after the reparent,
that walks up to the container and would not track a resize."
  (when (and exwm--connection (frame-live-p frame))
    (let ((container (frame-parameter frame 'exwm-container))
          (outer (frame-parameter frame 'exwm-outer-id)))
      (when (and container outer)
        (let ((child (xcb:+request-unchecked+reply
                         exwm--connection
                         (make-instance 'xcb:GetGeometry :drawable outer)))
              (parent (xcb:+request-unchecked+reply
                          exwm--connection
                          (make-instance 'xcb:GetGeometry
                                         :drawable container))))
          (when (and child parent
                     (or (/= (slot-value child 'width)
                             (slot-value parent 'width))
                         (/= (slot-value child 'height)
                             (slot-value parent 'height))))
            (exwm--set-geometry container nil nil
                                (max 1 (slot-value child 'width))
                                (max 1 (slot-value child 'height)))
            (xcb:flush exwm--connection)))))))

(defun exwm-floating--park-emacs-frame (frame)
  "Move FRAME's container off screen without resizing it.
The on-screen geometry is kept in `exwm-floating-emacs-geometry'."
  (when (and exwm--connection (frame-live-p frame))
    (let ((container (frame-parameter frame 'exwm-container)))
      (when container
        (let* ((reply (xcb:+request-unchecked+reply
                          exwm--connection
                          (make-instance 'xcb:GetGeometry
                                         :drawable container)))
               (saved (and reply
                           (exwm-floating--emacs-park-geometry
                            (slot-value reply 'x)
                            (slot-value reply 'y)
                            (slot-value reply 'width)
                            (slot-value reply 'height)
                            (frame-parameter frame
                                             'exwm-floating-emacs-geometry)))))
          (when saved
            (set-frame-parameter frame 'exwm-floating-emacs-geometry saved)
            (exwm--set-geometry container
                                exwm-floating--emacs-park
                                exwm-floating--emacs-park
                                nil nil)))))))

(defun exwm-floating--unpark-emacs-frame (frame)
  "Restore FRAME from `exwm-floating-emacs-geometry' and fit it."
  (when (and exwm--connection (frame-live-p frame))
    (let ((container (frame-parameter frame 'exwm-container))
          (saved (frame-parameter frame 'exwm-floating-emacs-geometry)))
      (when (and container (consp saved))
        (exwm--set-geometry container
                            (nth 0 saved) (nth 1 saved)
                            (nth 2 saved) (nth 3 saved))
        (set-frame-parameter frame 'exwm-floating-emacs-geometry nil))
      (exwm-floating--fit-emacs-container frame))))

(defun exwm-floating--sync-workspace (workspace active)
  "Park or restore floating Emacs frames that belong to WORKSPACE.
ACTIVE non-nil restores them.  A frame on another workspace is
left where it is, including one whose monitor stays active."
  (when exwm--connection
    (dolist (frame (copy-sequence exwm-floating--emacs-frames))
      (cond
       ((not (frame-live-p frame))
        (setq exwm-floating--emacs-frames
              (delq frame exwm-floating--emacs-frames)))
       ((eq (frame-parameter frame 'exwm-floating-workspace) workspace)
        (if active
            (exwm-floating--unpark-emacs-frame frame)
          (exwm-floating--park-emacs-frame frame)))))
    (xcb:flush exwm--connection)))

(defvar exwm-workspace--showing-desktop)

(defun exwm-floating--refresh-emacs-frame (frame)
  "Fit FRAME, or park it when its workspace is hidden.
`exwm-layout--refresh-floating' calls this.  There is no X
client to show."
  (when (and (frame-live-p frame)
             (frame-parameter frame 'exwm-floating-emacs))
    (let ((workspace (frame-parameter frame 'exwm-floating-workspace)))
      (if (or exwm-workspace--showing-desktop
              (and (frame-live-p workspace)
                   (not (exwm-workspace--active-p workspace))))
          (exwm-floating--park-emacs-frame frame)
        (exwm-floating--unpark-emacs-frame frame))
      (when exwm--connection
        (xcb:flush exwm--connection)))))

(defun exwm-floating--raise-emacs-frame (frame)
  "Raise FRAME's container, then raise child frames above it."
  (when (and exwm--connection
             (frame-live-p frame)
             (frame-parameter frame 'exwm-floating-emacs))
    (let ((container (frame-parameter frame 'exwm-container)))
      (when container
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window container
                           :value-mask xcb:ConfigWindow:StackMode
                           :stack-mode xcb:StackMode:Above))
        (exwm-workspace--raise-child-frames)))))

(defun exwm-floating--teardown-emacs-frame (frame &optional release)
  "Destroy FRAME's floating container and forget FRAME.
The frame itself is not deleted.  When RELEASE is non-nil, EXWM
is exiting: reparent the frame to the root and clear
override-redirect.  A user unfloat leaves the window unmapped
and then deletes the frame.  Clearing `exwm-floating-emacs'
first makes a nested `delete-frame' hook a no-op."
  (setq exwm-floating--emacs-frames
        (delq frame exwm-floating--emacs-frames))
  (when (frame-live-p frame)
    (let ((container (frame-parameter frame 'exwm-container))
          (outer (or (frame-parameter frame 'exwm-outer-id)
                     (let ((id (frame-parameter frame 'outer-window-id)))
                       (and (stringp id) (string-to-number id))))))
      (set-frame-parameter frame 'exwm-floating-emacs nil)
      (set-frame-parameter frame 'exwm-container nil)
      (set-frame-parameter frame 'exwm-outer-id nil)
      (set-frame-parameter frame 'exwm-floating-workspace nil)
      (set-frame-parameter frame 'exwm-floating-emacs-geometry nil)
      (when (and container outer exwm--connection
                 (slot-value exwm--connection 'connected))
        (xcb:+request exwm--connection
            (make-instance 'xcb:UnmapWindow :window outer))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ReparentWindow
                           :window outer
                           :parent exwm--root
                           :x 0
                           :y 0))
        (when release
          (xcb:+request exwm--connection
              (make-instance 'xcb:ChangeWindowAttributes
                             :window outer
                             :value-mask xcb:CW:OverrideRedirect
                             :override-redirect 0))
          (xcb:+request exwm--connection
              (make-instance 'xcb:MapWindow :window outer)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:DestroyWindow :window container))
        (xcb:flush exwm--connection)))))

(defun exwm-floating--on-delete-frame (frame)
  "Destroy FRAME's container when the user deletes the frame."
  (when (and (frame-live-p frame)
             (frame-parameter frame 'exwm-floating-emacs))
    (exwm-floating--teardown-emacs-frame frame)))

(defun exwm-floating--move-emacs-frame (frame delta-x delta-y)
  "Move floating Emacs FRAME by DELTA-X and DELTA-Y pixels.
Only the container moves.  The frame is its child at 0, 0."
  (when (and exwm--connection
             (not (and (= 0 delta-x) (= 0 delta-y))))
    (let* ((container (frame-parameter frame 'exwm-container))
           (geometry (and container
                          (xcb:+request-unchecked+reply
                              exwm--connection
                              (make-instance 'xcb:GetGeometry
                                             :drawable container)))))
      (when geometry
        (let ((saved (frame-parameter frame 'exwm-floating-emacs-geometry)))
          (if (consp saved)
              ;; The container is parked.  Remember the new on-screen
              ;; position and leave the parked window where it is.
              (set-frame-parameter
               frame 'exwm-floating-emacs-geometry
               (list (+ (nth 0 saved) delta-x)
                     (+ (nth 1 saved) delta-y)
                     (nth 2 saved)
                     (nth 3 saved)))
            (with-slots (x y) geometry
              (exwm--set-geometry container
                                  (+ x delta-x) (+ y delta-y) nil nil)
              (exwm-workspace--raise-child-frames))))))))

(defun exwm-floating--resize-emacs-frame (frame delta-width delta-height)
  "Resize floating Emacs FRAME by DELTA-WIDTH and DELTA-HEIGHT.
`set-frame-size' pixelwise changes the text area, so the delta is
applied to the text size.  The container then follows the Emacs
window."
  (unless (and (= 0 delta-width) (= 0 delta-height))
    (set-frame-size frame
                    (max 1 (+ (frame-text-width frame) delta-width))
                    (max 1 (+ (frame-text-height frame) delta-height))
                    t)
    (exwm-floating--fit-emacs-container frame)))

(defun exwm-floating--set-emacs-frame ()
  "Show the current buffer in a floating frame.
The frame is the content.  It is reparented into a container that
provides the floating border.  This does not call
`exwm-floating--set-floating', which requires an X window id."
  (unless (and exwm--connection
               (slot-value exwm--connection 'connected))
    (user-error "[EXWM] Not running as a window manager"))
  (when (minibufferp)
    (user-error "[EXWM] Cannot float the minibuffer"))
  (let* ((buffer (current-buffer))
         (source (selected-window))
         (edges (window-absolute-pixel-edges source))
         (source-x (nth 0 edges))
         (source-y (nth 1 edges))
         (text-width (max 1 (window-body-width source t)))
         (text-height (max 1 (window-body-height source t)))
         (workspace (exwm-floating--buffer-workspace))
         (minibuffer-frame (or workspace (selected-frame)))
         (frame (make-frame
                 `((minibuffer . ,(minibuffer-window minibuffer-frame))
                   (tab-bar-lines . 0)
                   (tab-bar-lines-keep-state . t)
                   ;; Off-screen until the container exists, same as
                   ;; `exwm-floating--set-floating'.  Do not apply this
                   ;; position again after the reparent: it would then
                   ;; be relative to the container.
                   (left . ,(* window-min-width -10000))
                   (top . ,(* window-min-height -10000))
                   (width . (text-pixels . ,text-width))
                   (height . (text-pixels . ,text-height))
                   (unsplittable . t)
                   (exwm-floating-emacs . t)
                   (exwm-floating-workspace . ,workspace))))
         (window (frame-first-window frame))
         (outer-id (string-to-number
                    (frame-parameter frame 'outer-window-id)))
         (window-id (string-to-number
                     (frame-parameter frame 'window-id)))
         (container (xcb:generate-id exwm--connection))
         (border-pixel (exwm--color->pixel exwm-floating-border-color))
         (border-width exwm-floating-border-width)
         success)
    (set-window-buffer window buffer)
    (set-window-parameter
     window 'split-window
     (lambda (&rest _) (user-error "Floating window cannot be split")))
    (when (and (window-live-p source)
               (not (eq source window))
               (eq (window-buffer source) buffer))
      (with-selected-window source
        (if (window-prev-buffers source)
            (switch-to-prev-buffer source)
          (switch-to-next-buffer source))))
    ;; The frame must be realized before its edges are meaningful.
    (redisplay)
    (modify-frame-parameters
     frame `((exwm-outer-id . ,outer-id)
             (exwm-id . ,window-id)))
    (setq exwm-floating--emacs-frames
          (cons frame exwm-floating--emacs-frames))
    (unwind-protect
        (let* ((measured (or (exwm-floating--measure-frame frame)
                             (list 0 0 text-width text-height)))
               (workarea (and (frame-live-p workspace)
                              (exwm-workspace--position workspace)
                              (exwm-workspace--workarea workspace)))
               (geometry (exwm-floating--emacs-container-geometry
                          source-x source-y
                          (nth 0 measured) (nth 1 measured)
                          (nth 2 measured) (nth 3 measured)
                          (and workarea (slot-value workarea 'x))
                          (and workarea (slot-value workarea 'y))
                          (and workarea (slot-value workarea 'width))
                          (and workarea (slot-value workarea 'height))))
               (x (nth 0 geometry))
               (y (nth 1 geometry))
               (width (nth 2 geometry))
               (height (nth 3 geometry))
               (hidden (and (frame-live-p workspace)
                            (not (exwm-workspace--active-p workspace)))))
          (xcb:+request exwm--connection
              (make-instance 'xcb:CreateWindow
                             :depth 0
                             :wid container
                             :parent exwm--root
                             :x x
                             :y y
                             :width width
                             :height height
                             :border-width border-width
                             :class xcb:WindowClass:InputOutput
                             :visual 0
                             :value-mask (logior xcb:CW:BackPixmap
                                                 (if border-pixel
                                                     xcb:CW:BorderPixel 0)
                                                 xcb:CW:OverrideRedirect)
                             :background-pixmap xcb:BackPixmap:ParentRelative
                             :border-pixel border-pixel
                             :override-redirect 1))
          ;; Set this only after the CreateWindow is queued, so a
          ;; failure earlier does not destroy an id that was never used.
          (set-frame-parameter frame 'exwm-container container)
          (xcb:+request exwm--connection
              (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                             :window container
                             :data "EXWM floating Emacs frame"))
          (xcb:+request exwm--connection
              (make-instance 'xcb:MapWindow :window container))
          (xcb:+request exwm--connection
              (make-instance 'xcb:ReparentWindow
                             :window outer-id
                             :parent container
                             :x 0
                             :y 0))
          (when hidden
            (exwm-floating--park-emacs-frame frame))
          (xcb:flush exwm--connection)
          (unless hidden
            (exwm-floating--raise-emacs-frame frame)
            (unless exwm-manage--display-window
              (select-frame-set-input-focus frame)))
          (when exwm-floating-border-color-focused
            (exwm-floating-refresh-borders))
          (setq success t))
      (unless success
        (exwm-floating--teardown-emacs-frame frame)
        (when (frame-live-p frame)
          (delete-frame frame))))))

(defun exwm-floating--unset-emacs-frame (frame)
  "Put FRAME's buffer back on its workspace and delete FRAME."
  (let* ((window (and (frame-live-p frame)
                      (frame-selected-window frame)))
         (buffer (and window (window-buffer window)))
         (workspace (or (and (frame-live-p frame)
                             (frame-parameter frame 'exwm-floating-workspace))
                        exwm-workspace--current)))
    (when (and (not exwm-manage--display-window)
               (frame-live-p workspace))
      (select-frame workspace))
    (exwm-floating--teardown-emacs-frame frame)
    (when (frame-live-p frame)
      (delete-frame frame))
    (when (and (not exwm-manage--display-window)
               (buffer-live-p buffer))
      (pop-to-buffer buffer))))

;;;###autoload
(defun exwm-floating-set-floating ()
  "Float the current X window.
This is the user request, so it floats a window that is configured
to stay tiled."
  (interactive)
  (unless (and (derived-mode-p 'exwm-mode) exwm--id)
    (user-error "[EXWM] No managed X window to float"))
  (unless exwm--floating-frame
    (let ((exwm-floating--user t))
      (exwm-floating--set-floating exwm--id))))

;;;###autoload
(defun exwm-floating-unset-floating ()
  "Tile the current X window."
  (interactive)
  (unless (and (derived-mode-p 'exwm-mode) exwm--id)
    (user-error "[EXWM] No managed X window to tile"))
  (when exwm--floating-frame
    (exwm-floating--unset-floating exwm--id)))

;;;###autoload
(defun exwm-floating-toggle-floating ()
  "Toggle floating for the current X window or Emacs buffer.
An X window uses its managed floating frame.  Any other buffer is
shown in a floating frame with the same border as a floating X
window.  Calling this from that frame, or on a buffer already
shown there, puts the buffer back on its workspace.

The key binding is on `exwm-mode-map' only.  From an ordinary
buffer, use \\[exwm-floating-toggle-floating]."
  (interactive)
  (exwm--log)
  (cond
   ((derived-mode-p 'exwm-mode)
    (if exwm--floating-frame
        (exwm-floating-unset-floating)
      (exwm-floating-set-floating)))
   ((and (frame-live-p (selected-frame))
         (frame-parameter (selected-frame) 'exwm-floating-emacs))
    (exwm-floating--unset-emacs-frame (selected-frame)))
   (t
    (let ((existing (exwm-floating--emacs-frame-showing (current-buffer))))
      (if existing
          (exwm-floating--unset-emacs-frame existing)
        (exwm-floating--set-emacs-frame))))))

;;;###autoload
(defun exwm-floating-hide ()
  "Hide the current floating X window (which would show again when selected)."
  (interactive)
  (exwm--log)
  (when (and (derived-mode-p 'exwm-mode)
             exwm--floating-frame)
    (exwm-layout--hide exwm--id)
    (select-frame-set-input-focus exwm-workspace--current)))

(defun exwm-floating--start-moveresize (id &optional type)
  "Start move/resize for window with ID.
When non-nil, TYPE indicates the type of move/resize.
Float resizing is stopped when TYPE is nil."
  (exwm--log "#x%x" id)
  (let ((buffer-or-id (or (exwm--id->buffer id) id))
        frame container-or-id x y width height cursor)
    (if (bufferp buffer-or-id)
        ;; Managed.
        (with-current-buffer buffer-or-id
          (setq frame exwm--floating-frame
                container-or-id (frame-parameter exwm--floating-frame
                                                 'exwm-container)))
      ;; Unmanaged.
      (setq container-or-id id))
    (when (and container-or-id
               ;; Test if the pointer can be grabbed
               (= xcb:GrabStatus:Success
                  (slot-value
                   (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GrabPointer
                                      :owner-events 0
                                      :grab-window container-or-id
                                      :event-mask xcb:EventMask:NoEvent
                                      :pointer-mode xcb:GrabMode:Async
                                      :keyboard-mode xcb:GrabMode:Async
                                      :confine-to xcb:Window:None
                                      :cursor xcb:Cursor:None
                                      :time xcb:Time:CurrentTime))
                   'status)))
      (with-slots (root-x root-y win-x win-y)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:QueryPointer :window id))
        (if (not (bufferp buffer-or-id))
            ;; Unmanaged.
            (unless (eq type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE)
              (with-slots ((width* width)
                           (height* height))
                  (xcb:+request-unchecked+reply exwm--connection
                      (make-instance 'xcb:GetGeometry :drawable id))
                (setq width width*
                      height height*)))
          ;; Managed.
          (select-window (frame-first-window frame)) ;transfer input focus
          (setq width (frame-outer-width frame)
                height (frame-outer-height frame))
          (unless type
            ;; Determine the resize type according to the pointer position
            ;; Clicking the center 1/3 part to resize has no effect
            (setq x (/ (* 3 win-x) (float width))
                  y (/ (* 3 win-y) (float height))
                  type (cond ((and (< x 1) (< y 1))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT)
                             ((and (> x 2) (< y 1))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
                             ((and (> x 2) (> y 2))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
                             ((and (< x 1) (> y 2))
                              xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
                             ((> x 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT)
                             ((> y 2) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                             ((< x 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT)
                             ((< y 1) xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP)))))
        (when (and type
                   (not (eq type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
                   (bufferp buffer-or-id)
                   (buffer-local-value 'exwm--fixed-size buffer-or-id))
          (exwm-floating--stop-moveresize)
          (user-error "[EXWM] Window size is fixed"))
        (if (not type)
            (exwm-floating--stop-moveresize)
          (cond ((= type xcb:ewmh:_NET_WM_MOVERESIZE_MOVE)
                 (setq cursor exwm-floating--cursor-move
                       exwm-floating--moveresize-calculate
                       (lambda (x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y))
                                 (- x win-x) (- y win-y) 0 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT)
                 (setq cursor exwm-floating--cursor-top-left
                       exwm-floating--moveresize-calculate
                       (lambda (x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height))
                                 (- x win-x) (- y win-y)
                                 (- (+ root-x width) x)
                                 (- (+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP)
                 (setq cursor exwm-floating--cursor-top
                       exwm-floating--moveresize-calculate
                       (lambda (_x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Height))
                                 0 (- y win-y) 0 (- (+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
                 (setq cursor exwm-floating--cursor-top-right
                       exwm-floating--moveresize-calculate
                       (lambda (x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height))
                                 0 (- y win-y) (- x (- root-x width))
                                 (- (+ root-y height) y)))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT)
                 (setq cursor exwm-floating--cursor-right
                       exwm-floating--moveresize-calculate
                       (lambda (x _y)
                         (vector buffer-or-id
                                 xcb:ConfigWindow:Width
                                 0 0 (- x (- root-x width)) 0))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
                 (setq cursor exwm-floating--cursor-bottom-right
                       exwm-floating--moveresize-calculate
                       (lambda (x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height))
                                 0 0 (- x (- root-x width))
                                 (- y (- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM)
                 (setq cursor exwm-floating--cursor-bottom
                       exwm-floating--moveresize-calculate
                       (lambda (_x y)
                         (vector buffer-or-id
                                 xcb:ConfigWindow:Height
                                 0 0 0 (- y (- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
                 (setq cursor exwm-floating--cursor-bottom-left
                       exwm-floating--moveresize-calculate
                       (lambda (x y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height))
                                 (- x win-x)
                                 0
                                 (- (+ root-x width) x)
                                 (- y (- root-y height))))))
                ((= type xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT)
                 (setq cursor exwm-floating--cursor-left
                       exwm-floating--moveresize-calculate
                       (lambda (x _y)
                         (vector buffer-or-id
                                 (eval-when-compile
                                   (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Width))
                                 (- x win-x) 0 (- (+ root-x width) x) 0)))))
          ;; Select events and change cursor (should always succeed)
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GrabPointer
                             :owner-events 0 :grab-window container-or-id
                             :event-mask (eval-when-compile
                                           (logior xcb:EventMask:ButtonRelease
                                                   xcb:EventMask:ButtonMotion))
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Async
                             :confine-to xcb:Window:None
                             :cursor cursor
                             :time xcb:Time:CurrentTime)))))))

(defun exwm-floating--stop-moveresize (&rest _args)
  "Stop move/resize."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:UngrabPointer :time xcb:Time:CurrentTime))
  (when exwm-floating--moveresize-calculate
    (let (result buffer-or-id outer-id container-id)
      (setq result (funcall exwm-floating--moveresize-calculate 0 0)
            buffer-or-id (aref result 0))
      (when (bufferp buffer-or-id)
        (with-current-buffer buffer-or-id
          (setq outer-id (frame-parameter exwm--floating-frame 'exwm-outer-id)
                container-id (frame-parameter exwm--floating-frame
                                              'exwm-container))
          (with-slots (x y width height border-width)
              (xcb:+request-unchecked+reply exwm--connection
                  (make-instance 'xcb:GetGeometry
                                 :drawable container-id))
            ;; Notify Emacs frame about this the position change.
            (xcb:+request exwm--connection
                (make-instance 'xcb:SendEvent
                               :propagate 0
                               :destination outer-id
                               :event-mask xcb:EventMask:StructureNotify
                               :event
                               (xcb:marshal
                                (make-instance 'xcb:ConfigureNotify
                                               :event outer-id
                                               :window outer-id
                                               :above-sibling xcb:Window:None
                                               :x (+ x border-width)
                                               :y (+ y border-width)
                                               :width width
                                               :height height
                                               :border-width 0
                                               :override-redirect 0)
                                exwm--connection)))
            (xcb:flush exwm--connection))
          (exwm-layout--show exwm--id
                             (frame-root-window exwm--floating-frame)))))
    (setq exwm-floating--moveresize-calculate nil)))

(defun exwm-floating--do-moveresize (data _synthetic)
  "Perform move/resize on floating window with DATA."
  (when exwm-floating--moveresize-calculate
    (let* ((obj (xcb:unmarshal-new 'xcb:MotionNotify data))
           (result (funcall exwm-floating--moveresize-calculate
                            (slot-value obj 'root-x)
                            (slot-value obj 'root-y)))
           (buffer-or-id (aref result 0))
           (value-mask (aref result 1))
           (x (aref result 2))
           (y (aref result 3))
           (width (max 1 (aref result 4)))
           (height (max 1 (aref result 5)))
           left-offset top-offset container-or-id)
      (if (not (bufferp buffer-or-id))
          ;; Unmanaged.
          (setq container-or-id buffer-or-id
                left-offset 0
                top-offset 0)
        ;; Managed.
        (let* ((frame (buffer-local-value 'exwm--floating-frame buffer-or-id))
               (window (get-buffer-window buffer-or-id frame))
               (window-edges (exwm--window-inside-absolute-pixel-edges window))
               (outer-edges (frame-edges frame 'outer-edges)))
          (setq container-or-id (frame-parameter frame 'exwm-container)
                left-offset (- (elt window-edges 0) (elt outer-edges 0))
                top-offset (- (elt window-edges 1) (elt outer-edges 1)))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window container-or-id
                         :value-mask value-mask
                         :x (- x left-offset)
                         :y (- y top-offset)
                         :width width
                         :height height))
      (when (bufferp buffer-or-id)
        ;; Managed.
        (with-current-buffer buffer-or-id
          (let ((resize-value-mask
                 (logand value-mask (logior xcb:ConfigWindow:Width
                                            xcb:ConfigWindow:Height)))
                (move-value-mask
                 (logand value-mask (logior xcb:ConfigWindow:X
                                            xcb:ConfigWindow:Y))))
            (when (/= 0 resize-value-mask)
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window (frame-parameter exwm--floating-frame
                                                          'exwm-outer-id)
                                 :value-mask resize-value-mask
                                 :width width
                                 :height height)))
            (when (/= 0 move-value-mask)
              (xcb:+request exwm--connection
                  (make-instance 'xcb:ConfigureWindow
                                 :window exwm--id
                                 :value-mask move-value-mask
                                 :x x
                                 :y y))))))
      (xcb:flush exwm--connection))))

(defun exwm-floating-move (&optional delta-x delta-y)
  "Move a floating window right by DELTA-X pixels and down by DELTA-Y pixels.

Both DELTA-X and DELTA-Y default to 1.  This command should be bound locally."
  (exwm--log "delta-x: %s, delta-y: %s" delta-x delta-y)
  (unless delta-x (setq delta-x 1))
  (unless delta-y (setq delta-y 1))
  (cond
   ((and (derived-mode-p 'exwm-mode) exwm--floating-frame)
    (unless (and (= 0 delta-x) (= 0 delta-y))
      (let* ((floating-container (frame-parameter exwm--floating-frame
                                                  'exwm-container))
             (geometry (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:GetGeometry
                                          :drawable floating-container)))
             (edges (exwm--window-inside-absolute-pixel-edges)))
        (with-slots (x y) geometry
          (exwm--set-geometry floating-container
                              (+ x delta-x) (+ y delta-y) nil nil))
        (exwm--set-geometry exwm--id
                            (+ (pop edges) delta-x)
                            (+ (pop edges) delta-y)
                            nil nil)
        (xcb:flush exwm--connection))))
   ((and (frame-live-p (selected-frame))
         (frame-parameter (selected-frame) 'exwm-floating-emacs))
    (exwm-floating--move-emacs-frame (selected-frame) delta-x delta-y))
   (t
    (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))))

(defun exwm-floating--keyboard-delta (n)
  "Return N steps of `exwm-floating-keyboard-step', in pixels."
  (* (prefix-numeric-value n) exwm-floating-keyboard-step))

(defun exwm-floating-move-right (&optional n)
  "Move the floating window right by N steps."
  (interactive "p")
  (exwm-floating-move (exwm-floating--keyboard-delta n) 0))

(defun exwm-floating-move-left (&optional n)
  "Move the floating window left by N steps."
  (interactive "p")
  (exwm-floating-move (- (exwm-floating--keyboard-delta n)) 0))

(defun exwm-floating-move-down (&optional n)
  "Move the floating window down by N steps."
  (interactive "p")
  (exwm-floating-move 0 (exwm-floating--keyboard-delta n)))

(defun exwm-floating-move-up (&optional n)
  "Move the floating window up by N steps."
  (interactive "p")
  (exwm-floating-move 0 (- (exwm-floating--keyboard-delta n))))

(defun exwm-floating-resize (delta-width delta-height)
  "Grow the floating window by DELTA-WIDTH and DELTA-HEIGHT pixels."
  (cond
   ((and (derived-mode-p 'exwm-mode) exwm--floating-frame)
    (when exwm--fixed-size
      (user-error "[EXWM] Window size is fixed"))
    (unless (and (= 0 delta-width) (= 0 delta-height))
      (set-frame-size exwm--floating-frame
                      (max 1 (+ (frame-pixel-width exwm--floating-frame)
                                delta-width))
                      (max 1 (+ (frame-pixel-height exwm--floating-frame)
                                delta-height))
                      t)))
   ((and (frame-live-p (selected-frame))
         (frame-parameter (selected-frame) 'exwm-floating-emacs))
    (exwm-floating--resize-emacs-frame
     (selected-frame) delta-width delta-height))
   (t
    (user-error "[EXWM] `exwm-floating-resize' is only for floating X windows"))))

(defun exwm-floating-grow-width (&optional n)
  "Grow the floating window width by N steps."
  (interactive "p")
  (exwm-floating-resize (exwm-floating--keyboard-delta n) 0))

(defun exwm-floating-shrink-width (&optional n)
  "Shrink the floating window width by N steps."
  (interactive "p")
  (exwm-floating-resize (- (exwm-floating--keyboard-delta n)) 0))

(defun exwm-floating-grow-height (&optional n)
  "Grow the floating window height by N steps."
  (interactive "p")
  (exwm-floating-resize 0 (exwm-floating--keyboard-delta n)))

(defun exwm-floating-shrink-height (&optional n)
  "Shrink the floating window height by N steps."
  (interactive "p")
  (exwm-floating-resize 0 (- (exwm-floating--keyboard-delta n))))

(defun exwm-floating--init ()
  "Initialize floating module."
  (exwm--log)
  (add-hook 'delete-frame-functions #'exwm-floating--on-delete-frame)
  (dolist (param '(exwm-floating-emacs
                   exwm-floating-workspace
                   exwm-floating-emacs-geometry))
    (unless (assq param frameset-filter-alist)
      (push (cons param :never) frameset-filter-alist)))
  ;; Initialize cursors for moving/resizing a window
  (setq exwm-floating--cursor-move
        (xcb:cursor:load-cursor exwm--connection "fleur")
        exwm-floating--cursor-top-left
        (xcb:cursor:load-cursor exwm--connection "top_left_corner")
        exwm-floating--cursor-top
        (xcb:cursor:load-cursor exwm--connection "top_side")
        exwm-floating--cursor-top-right
        (xcb:cursor:load-cursor exwm--connection "top_right_corner")
        exwm-floating--cursor-right
        (xcb:cursor:load-cursor exwm--connection "right_side")
        exwm-floating--cursor-bottom-right
        (xcb:cursor:load-cursor exwm--connection "bottom_right_corner")
        exwm-floating--cursor-bottom
        (xcb:cursor:load-cursor exwm--connection "bottom_side")
        exwm-floating--cursor-bottom-left
        (xcb:cursor:load-cursor exwm--connection "bottom_left_corner")
        exwm-floating--cursor-left
        (xcb:cursor:load-cursor exwm--connection "left_side")))

(defun exwm-floating--exit ()
  "Exit the floating module."
  (exwm--log)
  (remove-hook 'delete-frame-functions #'exwm-floating--on-delete-frame)
  ;; The connection is still open here.  Drop the containers and
  ;; leave the frames as ordinary X windows.
  (dolist (frame (copy-sequence exwm-floating--emacs-frames))
    (exwm-floating--teardown-emacs-frame frame 'release))
  (setq exwm-floating--emacs-frames nil))

(provide 'exwm-floating)
;;; exwm-floating.el ends here
