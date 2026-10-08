;;; exwm-input.el --- Input Module for EXWM  -*- lexical-binding: t -*-

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

;; This module deals with key/mouse matters, including:
;; + Input focus,
;; + Key/Button event handling,
;; + Key events filtering and simulation.

;; Todo:
;; + Pointer simulation mode (e.g. 'C-c 1'/'C-c 2' for single/double click,
;;   move with arrow keys).
;; + Simulation keys to mimic Emacs key bindings for text edit (redo, select,
;;   cancel, clear, etc).  Some of them are not present on common keyboard
;;   (keycode = 0).  May need to use XKB extension.

;;; Code:

(require 'xcb-keysyms)
(require 'xcb-xtest)
(require 'windmove)
(require 'exwm-core)

(defgroup exwm-input nil
  "Input."
  :group 'exwm)

(defcustom exwm-input-prefix-keys
  '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)
  "List of prefix keys EXWM should forward to Emacs when in `line-mode'.

There is no need to add prefix keys for global/simulation keys or those
defined in `exwm-mode-map' here."
  :type '(repeat key-sequence)
  :get (lambda (symbol)
         (mapcar #'vector (default-value symbol)))
  :set (lambda (symbol value)
         (set symbol (mapcar (lambda (i)
                               (if (sequencep i)
                                   (aref i 0)
                                 i))
                             value))))

(defcustom exwm-input-move-event 's-down-mouse-1
  "Emacs event to start moving a window."
  :type 'key-sequence
  :get (lambda (symbol)
         (let ((value (default-value symbol)))
           (if (mouse-event-p value)
               value
             (vector value))))
  :set (lambda (symbol value)
         (set symbol (if (sequencep value)
                         (aref value 0)
                       value))))

(defcustom exwm-input-resize-event 's-down-mouse-3
  "Emacs event to start resizing a window."
  :type 'key-sequence
  :get (lambda (symbol)
         (let ((value (default-value symbol)))
           (if (mouse-event-p value)
               value
             (vector value))))
  :set (lambda (symbol value)
         (set symbol (if (sequencep value)
                         (aref value 0)
                       value))))

(defcustom exwm-input-line-mode-passthrough nil
  "Non-nil makes `line-mode' forward all events to Emacs."
  :type 'boolean)

(defcustom exwm-input-mouse-follows-focus nil
  "When non-nil, warp the pointer to the focused window.
The warp runs when input focus is committed, to the center of
that Emacs window.  A focus change that came from the pointer
stays where it is, so this does not fight
`mouse-autoselect-window' or a click.  Nil does not move the
pointer.  `exwm-workspace-warp-cursor' is unchanged."
  :type 'boolean)

;; Declared here so the setter below can be defined before the option.
(defvar exwm-input-modifiers)
(defvar exwm-input--installed-modifier-masks nil
  "Modifier masks currently grabbed for `exwm-input-modifiers'.")

(defun exwm-input--set-modifiers (symbol value)
  "Setter for `exwm-input-modifiers'."
  (let (clean)
    (dolist (mod value)
      (if (memq mod '(super hyper meta control alt))
          (cl-pushnew mod clean)
        (warn "EXWM: %S is not a valid `exwm-input-modifiers' entry and was ignored"
              mod)))
    (set-default symbol (nreverse clean)))
  (when exwm--connection
    (exwm-input--grab-modifiers-on-all-windows)))

(defun exwm-input--lock-masks ()
  "Lock masks combined with a reserved modifier.
Caps Lock and Num Lock must not defeat the grab."
  (let ((masks (list 0)))
    (when (/= 0 xcb:keysyms:num-lock-mask)
      (push xcb:keysyms:num-lock-mask masks))
    (when (/= 0 xcb:keysyms:lock-mask)
      (push xcb:keysyms:lock-mask masks)
      (when (/= 0 xcb:keysyms:num-lock-mask)
        (push (logior xcb:keysyms:lock-mask xcb:keysyms:num-lock-mask)
              masks)))
    masks))

(defun exwm-input--modifier-masks ()
  "X masks for `exwm-input-modifiers', including lock combinations."
  (let* ((named `((super . ,xcb:keysyms:super-mask)
                  (hyper . ,xcb:keysyms:hyper-mask)
                  (meta . ,xcb:keysyms:meta-mask)
                  (control . ,xcb:keysyms:control-mask)
                  (alt . ,xcb:keysyms:alt-mask)))
         (masks nil))
    (dolist (mod exwm-input-modifiers)
      (let ((base (cdr (assq mod named))))
        (when (and base (/= 0 base))
          (dolist (lock (exwm-input--lock-masks))
            (cl-pushnew (logior base lock) masks)))))
    masks))

(defun exwm-input--modifier-event-p (event)
  "Return non-nil if EVENT uses a modifier in `exwm-input-modifiers'."
  (and exwm-input-modifiers
       (let ((mods (event-modifiers event)))
         (cl-some (lambda (mod) (memq mod mods)) exwm-input-modifiers))))

(defun exwm-input--ungrab-modifiers (&rest xwins)
  "Release reserved-modifier grabs on XWINS.
Leave `exwm-input--installed-modifier-masks' unchanged so the grabs
can be restored.  Line-mode grabs every key with AnyModifier, and
that request fails while a narrower modifier grab is installed."
  (when (and exwm--connection exwm-input--installed-modifier-masks)
    (let ((ungrab (make-instance 'xcb:UngrabKey
                                 :key xcb:Grab:Any
                                 :grab-window nil
                                 :modifiers nil)))
      (dolist (xwin xwins)
        (dolist (mask exwm-input--installed-modifier-masks)
          (setf (slot-value ungrab 'grab-window) xwin
                (slot-value ungrab 'modifiers) mask)
          (xcb:+request exwm--connection ungrab)))
      (xcb:flush exwm--connection))))

(defun exwm-input--grab-modifiers (&rest xwins)
  "Grab or release `exwm-input-modifiers' on XWINS."
  (when exwm--connection
    (let ((masks (exwm-input--modifier-masks))
          (grab (make-instance 'xcb:GrabKey
                               :owner-events 0
                               :grab-window nil
                               :modifiers nil
                               :key xcb:Grab:Any
                               :pointer-mode xcb:GrabMode:Async
                               :keyboard-mode xcb:GrabMode:Async))
          (ungrab (make-instance 'xcb:UngrabKey
                                 :key xcb:Grab:Any
                                 :grab-window nil
                                 :modifiers nil)))
      (dolist (xwin xwins)
        (dolist (mask exwm-input--installed-modifier-masks)
          (setf (slot-value ungrab 'grab-window) xwin
                (slot-value ungrab 'modifiers) mask)
          (xcb:+request exwm--connection ungrab))
        (dolist (mask masks)
          (setf (slot-value grab 'grab-window) xwin
                (slot-value grab 'modifiers) mask)
          (xcb:+request exwm--connection grab)))
      (setq exwm-input--installed-modifier-masks masks)
      (xcb:flush exwm--connection))))

(defun exwm-input--grab-modifiers-on-all-windows ()
  "Apply `exwm-input-modifiers' to every existing X window."
  (when-let* ((tree (xcb:+request-unchecked+reply exwm--connection
                        (make-instance 'xcb:QueryTree
                                       :window exwm--root))))
    (apply #'exwm-input--grab-modifiers
           (slot-value tree 'children))))

(defcustom exwm-input-modifiers nil
  "Modifiers whose chords always go to Emacs.

Each entry is one of `super', `hyper', `meta', `control', or `alt'.
A chord that uses one of these modifiers is handled by Emacs in both
`line-mode' and `char-mode'.  The application does not see it.  This
reserves a modifier for window management without listing every binding
in `exwm-input-global-keys'.

Super is the usual choice:

  (setopt exwm-input-modifiers \\='(super))

`shift' is not accepted.  Grabbing it would swallow ordinary capital
letters.  `control' and `meta' work, and they also stop applications
from seeing those chords while a window is in `char-mode'.

A modifier that is not present on the keyboard is skipped.  Set this
before EXWM starts.  `setopt' and Customize apply a new value
immediately, including after EXWM has started."
  :type '(set (const super) (const hyper) (const meta)
              (const control) (const alt))
  :set #'exwm-input--set-modifiers)

;; Input focus update requests should be accumulated for a short time
;; interval so that only the last one need to be processed.  This not
;; improves the overall performance, but avoids the problem of input
;; focus loop, which is a result of the interaction with Emacs frames.
;;
;; FIXME: The time interval is hard to decide and perhaps machine-dependent.
;;        A value too small can cause redundant updates of input focus,
;;        and even worse, dead loops.  OTOH a large value would bring
;;        laggy experience.
(defconst exwm-input--update-focus-interval 0.01
  "Time interval (in seconds) for accumulating input focus update requests.")

(defconst exwm-input--passthrough-functions '(read-char
                                              read-char-exclusive
                                              read-key-sequence-vector
                                              read-key-sequence
                                              read-event)
  "Low-level read functions that must be exempted from EXWM input handling.")

(defvar exwm-input--global-keys nil "Global key bindings.")

(defvar exwm-input--global-prefix-keys nil
  "List of prefix keys of global key bindings.")

(defvar exwm-input--line-mode-cache nil "Cache for incomplete key sequence.")

(defvar exwm-input--simulation-keys nil "Simulation keys in `line-mode'.")

(defvar exwm-input--skip-buffer-list-update nil
  "Skip the upcoming `buffer-list-update'.")

(defvar exwm-input--temp-line-mode nil
  "Non-nil indicates it's in temporary line-mode for `char-mode'.")

(defvar exwm-input--timestamp-atom nil)

(defvar exwm-input--timestamp-callback nil)

(defvar exwm-input--timestamp-window nil)

(defvar exwm-input--update-focus-timer nil
  "Timer for deferring the update of input focus.")

(defvar exwm-input--update-focus-lock nil
  "Lock for solving input focus update contention.")

(defvar exwm-input--update-focus-window nil "The (Emacs) window to be focused.
This value should always be overwritten.")

(defvar exwm-input--echo-area-timer nil "Timer for detecting echo area dirty.")

(defvar exwm-input--event-hook nil
  "Hook to run when EXWM receives an event.")

(defvar exwm-input-input-mode-change-hook nil
  "Hook to run when an input mode changes on an `exwm-mode' buffer.
Current buffer will be the `exwm-mode' buffer when this hook runs.")

(defvar exwm-workspace--current)
(defvar exwm-floating-border-color-focused)
(declare-function exwm-floating-refresh-borders "exwm-floating.el" ())
(declare-function exwm-floating--raise-emacs-frame "exwm-floating.el" (frame))
(declare-function exwm-floating--do-moveresize "exwm-floating.el"
                  (data _synthetic))
(declare-function exwm-floating--start-moveresize "exwm-floating.el"
                  (id &optional type))
(declare-function exwm-floating--stop-moveresize "exwm-floating.el"
                  (&rest _args))
(declare-function exwm-layout--iconic-state-p "exwm-layout.el" (&optional id))
(declare-function exwm-layout--raise-floating "exwm-layout.el" ())
(declare-function exwm-layout--show "exwm-layout.el" (id &optional window))

(defvar exwm-input--pointer-focus-until nil
  "Time until which a focus change came from pointer autoselection.
`mouse-autoselect-window' focuses the window under the pointer.
That must not raise it.  A click or a command still raises.")

(defun exwm-input--note-pointer-focus (&rest _args)
  "Remember that pointer autoselection is moving input focus."
  (setq exwm-input--pointer-focus-until (+ (float-time) 0.2)))

(defun exwm-input--clear-pointer-focus ()
  "Forget a pointer-autoselection focus once a real command starts."
  (setq exwm-input--pointer-focus-until nil))

(defun exwm-input--pointer-focus-p ()
  "Non-nil when the current focus change is from pointer movement."
  (and exwm-input--pointer-focus-until
       (< (float-time) exwm-input--pointer-focus-until)))

(defvar exwm-input--pointer-focus-window nil
  "Window the pointer just selected.
Input focus committed to this window does not warp the pointer.")

(defvar exwm-input--warp-until nil
  "Time until which EnterNotify from a warp is ignored.")

(defun exwm-input--warping-p ()
  "Non-nil while a pointer warp is still settling."
  (and exwm-input--warp-until
       (< (float-time) exwm-input--warp-until)))

(defun exwm-input--warp-blocked-p (window)
  "Return non-nil when WINDOW must not be warped to.
A pointer enter, a click, or a warp already in progress selected it."
  (or (and exwm-input--pointer-focus-window
           (eq window exwm-input--pointer-focus-window))
      (exwm-input--pointer-focus-p)
      (exwm-input--warping-p)))

(defun exwm-input--warp-decision (option blocked showing)
  "Return non-nil when a committed focus should move the pointer.
OPTION is `exwm-input-mouse-follows-focus'.  BLOCKED is non-nil
when the pointer already chose this window.  SHOWING is non-nil
while show-desktop has the windows hidden."
  (and option (not blocked) (not showing)))
(declare-function exwm-reset "exwm.el" ())
(declare-function exwm-workspace--minibuffer-own-frame-p "exwm-workspace.el")
(declare-function exwm-workspace--workspace-p "exwm-workspace.el" (workspace))
(declare-function exwm-workspace-switch "exwm-workspace.el"
                  (frame-or-index &optional force))

(defun exwm-input--set-focus (id)
  "Set input focus to window ID in a proper way."
  (let ((from (slot-value (xcb:+request-unchecked+reply exwm--connection
                              (make-instance 'xcb:GetInputFocus))
                          'focus))
        tree)
    (if (or (exwm--id->buffer from)
            (eq from id))
        (exwm--log "#x%x => #x%x" (or from 0) (or id 0))
      ;; Attempt to find the top-level X window for a 'focus proxy'.
      (unless (= from xcb:Window:None)
        (setq tree (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:QueryTree
                                      :window from)))
        (when tree
          (setq from (slot-value tree 'parent))))
      (exwm--log "#x%x (corrected) => #x%x" (or from 0) (or id 0)))
    (when (and (exwm--id->buffer id)
               ;; Avoid redundant input focus transfer.
               (not (eq from id)))
      (with-current-buffer (exwm--id->buffer id)
        (exwm-input--update-timestamp
         (lambda (timestamp id send-input-focus wm-take-focus)
           (when send-input-focus
             (xcb:+request exwm--connection
                 (make-instance 'xcb:SetInputFocus
                                :revert-to xcb:InputFocus:Parent
                                :focus id
                                :time timestamp)))
           (when wm-take-focus
             (let ((event (make-instance 'xcb:icccm:WM_TAKE_FOCUS
                                         :window id
                                         :time timestamp)))
               (setq event (xcb:marshal event exwm--connection))
               (xcb:+request exwm--connection
                   (make-instance 'xcb:icccm:SendEvent
                                  :destination id
                                  :event event))))
           (exwm-input--set-active-window id)
           (xcb:flush exwm--connection))
         id
         (or exwm--hints-input
             (not (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols)))
         (memq xcb:Atom:WM_TAKE_FOCUS exwm--protocols))))))

(defun exwm-input--update-timestamp (callback &rest args)
  "Fetch the latest timestamp from the server and feed it to CALLBACK.

ARGS are additional arguments to CALLBACK."
  (setq exwm-input--timestamp-callback (cons callback args))
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeProperty
                     :mode xcb:PropMode:Replace
                     :window exwm-input--timestamp-window
                     :property exwm-input--timestamp-atom
                     :type xcb:Atom:CARDINAL
                     :format 32
                     :data-len 0
                     :data nil))
  (xcb:flush exwm--connection))

(defun exwm-input--on-PropertyNotify (data _synthetic)
  "Handle PropertyNotify events with DATA."
  (exwm--log)
  (when exwm-input--timestamp-callback
    (with-slots (window time)
        (xcb:unmarshal-new 'xcb:PropertyNotify data)
      (when (= exwm-input--timestamp-window window)
        (apply (car exwm-input--timestamp-callback)
               time
               (cdr exwm-input--timestamp-callback))
        (setq exwm-input--timestamp-callback nil)))))

(defvar exwm-input--last-enter-notify-position nil)

(defun exwm-input--on-EnterNotify (data _synthetic)
  "Handle EnterNotify events with DATA."
  (with-slots (time root event root-x root-y event-x event-y state)
      (xcb:unmarshal-new 'xcb:EnterNotify data)
    (unless (exwm-input--warping-p)
      (when-let* ((_(not (equal exwm-input--last-enter-notify-position
                                (vector root-x root-y))))
                  (buffer (exwm--id->buffer event))
                  (window (get-buffer-window buffer t))
                  (_(not (eq window (selected-window))))
                  (frame (window-frame window))
                  (frame-xid (frame-parameter frame 'exwm-id)))
        (setq exwm-input--pointer-focus-window window)
        (exwm--log "buffer=%s; window=%s" buffer window)
      (unless (eq frame exwm-workspace--current)
        (if (exwm-workspace--workspace-p frame)
            ;; The X window is on another workspace.
            (exwm-workspace-switch frame)
          (with-current-buffer buffer
            (when (and (derived-mode-p 'exwm-mode)
                       (not (eq exwm--frame exwm-workspace--current)))
              ;; The floating X window is on another workspace.
              (exwm-workspace-switch exwm--frame)))))
      ;; Send a fake MotionNotify event to Emacs.
      (let* ((edges (exwm--window-inside-pixel-edges window))
             (x (+ event-x (elt edges 0)))
             (y (+ event-y (elt edges 1)))
             (fake-evt (make-instance 'xcb:MotionNotify
                                      :detail 0
                                      :time time
                                      :root root
                                      :event frame-xid
                                      :child xcb:Window:None
                                      :root-x root-x
                                      :root-y root-y
                                      :event-x x
                                      :event-y y
                                      :state state
                                      :same-screen 1)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0
                           :destination frame-xid
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal fake-evt exwm--connection))))
        (xcb:flush exwm--connection)))
    (setq exwm-input--last-enter-notify-position (vector root-x root-y))))

(defun exwm-input--on-keysyms-update ()
  "Update global prefix keys."
  (exwm--log)
  (let ((exwm-input--global-prefix-keys nil))
    (exwm-input--update-global-prefix-keys))
  (exwm-input--xtest-refresh-modifiers))

(defun exwm-input--on-buffer-list-update ()
  "Run in `buffer-list-update-hook' to track input focus."
  (when (and          ; this hook is called incesantly; place cheap tests on top
         (not exwm-input--skip-buffer-list-update)
         (exwm--terminal-p) ; skip other terminals, e.g. TTY client frames
         (not (frame-parameter nil 'no-accept-focus)))
    (exwm--log "current-buffer=%S selected-window=%S"
               (current-buffer) (selected-window))
    (redirect-frame-focus (selected-frame) nil)
    (setq exwm-input--update-focus-window (selected-window))
    (exwm-input--update-focus-defer)))

(defun exwm-input--update-focus-defer ()
  "Schedule a deferred update to input focus.
Instead of immediately focusing the current window, it defers the focus change
until the selected window stops changing (debouncing input focus updates)."
  (when exwm-input--update-focus-timer
    (cancel-timer exwm-input--update-focus-timer))
  (setq exwm-input--update-focus-timer
        ;; Attempt to accumulate successive events close enough.
        (run-with-timer exwm-input--update-focus-interval
                        nil
                        #'exwm-input--update-focus-commit)))

(defun exwm-input--other-window-x-id ()
  "Return the X window id in the other window, or nil.
Signal the same error as `scroll-other-window' when there is no other
window."
  (let ((window (other-window-for-scrolling)))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (and (derived-mode-p 'exwm-mode) exwm--id)))))

(defun exwm-input-scroll-other-window (&optional arg)
  "Scroll the other window, or page an X client shown there.
ARG is passed to `scroll-other-window' for an ordinary buffer.
A negative ARG sends Prior to an X client.  Any other ARG sends Next."
  (interactive "P")
  (let ((id (exwm-input--other-window-x-id)))
    (if id
        (exwm-input--fake-key
         (if (and arg (< (prefix-numeric-value arg) 0))
             'prior
           'next)
         id)
      (scroll-other-window arg))))

(defun exwm-input-scroll-other-window-down (&optional arg)
  "Scroll the other window down, or page an X client up.
ARG is passed to `scroll-other-window-down' for an ordinary buffer.
An X client receives Prior."
  (interactive "P")
  (let ((id (exwm-input--other-window-x-id)))
    (if id
        (exwm-input--fake-key 'prior id)
      (scroll-other-window-down arg))))

(defun exwm-input-refresh-focus ()
  "Schedule input focus for the selected window.
`exwm-workspace-switch' does this itself.  Call this after another
command selects an EXWM window without switching workspace, for
example a perspective switch.  The change is deferred in the same
way as `buffer-list-update-hook'."
  (interactive)
  (setq exwm-input--update-focus-window (selected-window))
  (exwm-input--update-focus-defer))

(defvar exwm-input--persp-after-load nil
  "Non-nil once perspective packages are hooked for a later load.")

(defun exwm-input--refresh-focus-after-persp (&rest _)
  "Schedule input focus after a perspective switch.
persp-mode and perspective.el restore a window configuration without
going through `exwm-workspace-switch'.  The selected window is the
one just restored."
  (when exwm--connection
    (exwm-input-refresh-focus)))

(defun exwm-input--persp-setup ()
  "Focus the restored window when a perspective package switches."
  (when (boundp 'persp-activated-functions)
    (add-hook 'persp-activated-functions
              #'exwm-input--refresh-focus-after-persp))
  (when (boundp 'persp-activated-hook)
    (add-hook 'persp-activated-hook
              #'exwm-input--refresh-focus-after-persp))
  (unless exwm-input--persp-after-load
    (setq exwm-input--persp-after-load t)
    (with-eval-after-load 'persp-mode
      (when exwm--connection
        (add-hook 'persp-activated-functions
                  #'exwm-input--refresh-focus-after-persp)))
    (with-eval-after-load 'perspective
      (when exwm--connection
        (add-hook 'persp-activated-hook
                  #'exwm-input--refresh-focus-after-persp)))))

(defun exwm-input--persp-exit ()
  "Remove the perspective focus hooks."
  (remove-hook 'persp-activated-functions
               #'exwm-input--refresh-focus-after-persp)
  (remove-hook 'persp-activated-hook
               #'exwm-input--refresh-focus-after-persp))

(defun exwm-input--update-focus-commit ()
  "Attempt to update the window focus.
If we're currently updating the window focus, re-schedule a focus update
attempt later."
  (if exwm-input--update-focus-lock
      (exwm-input--update-focus-defer)
    (let ((exwm-input--update-focus-lock t))
      (exwm-input--update-focus exwm-input--update-focus-window))))

(defun exwm-input--update-focus (window)
  "Update input focus to WINDOW."
  (when (window-live-p window)
    (exwm--log "focus-window=%s focus-buffer=%s" window (window-buffer window))
    (with-current-buffer (window-buffer window)
      (if (derived-mode-p 'exwm-mode)
          (if (not (eq exwm--frame exwm-workspace--current))
              (progn
                (set-frame-parameter exwm--frame 'exwm-selected-window window)
                (exwm--defer 0 #'exwm-workspace-switch exwm--frame))
            (exwm--log "Set focus on #x%x" exwm--id)
            (when exwm--floating-frame
              ;; Pointer movement focuses without raising, so a dialog
              ;; does not bury the one under the mouse.  A command
              ;; focus still raises.  Clicks raise in the button handler.
              (unless (exwm-input--pointer-focus-p)
                (exwm-layout--raise-floating))
              ;; This floating X window might be hide by `exwm-floating-hide'.
              (when (exwm-layout--iconic-state-p)
                (exwm-layout--show exwm--id window))
              (xcb:flush exwm--connection))
            (exwm-input--set-focus exwm--id))
        (when (eq (selected-window) window)
          (exwm--log "Focus on %s" window)
          (if (and (exwm-workspace--workspace-p (selected-frame))
                   (not (eq (selected-frame) exwm-workspace--current)))
              ;; The focus is on another workspace (e.g. it got clicked)
              ;; so switch to it.
              (progn
                (exwm--log "Switching to %s's workspace %s (%s)"
                           window
                           (window-frame window)
                           (selected-frame))
                (set-frame-parameter (selected-frame) 'exwm-selected-window
                                     window)
                (exwm--defer 0 #'exwm-workspace-switch (selected-frame)))
            ;; The focus is still on the current workspace.
            (let ((frame (if (not (and (exwm-workspace--minibuffer-own-frame-p)
                                       (minibufferp)))
                             (window-frame window)
                           ;; X input focus should be set on the previously
                           ;; selected frame.
                           (window-frame (minibuffer-window)))))
              (x-focus-frame frame)
              (when (frame-parameter frame 'exwm-floating-emacs)
                (let ((workspace (frame-parameter frame
                                                  'exwm-floating-workspace)))
                  (when (and (frame-live-p workspace)
                             (not (eq workspace exwm-workspace--current)))
                    (set-frame-parameter workspace 'exwm-selected-window
                                         window)
                    (exwm--defer 0 #'exwm-workspace-switch workspace)))
                (exwm-floating--raise-emacs-frame frame))
              (exwm-input--set-active-window
               (or (frame-parameter exwm-workspace--current 'exwm-outer-id)
                   xcb:Window:None)))
            (xcb:flush exwm--connection)))))
    (when exwm-floating-border-color-focused
      (exwm-floating-refresh-borders))
    (let ((warp (exwm-input--should-warp-p window)))
      ;; Consume a pointer-chosen window so the next command can warp.
      (setq exwm-input--pointer-focus-window nil)
      (when warp
        (exwm-input--warp-to-window window)))))

(defun exwm-input--should-warp-p (window)
  "Return non-nil when focus on WINDOW should warp the pointer."
  (and (exwm-input--warp-decision
        exwm-input-mouse-follows-focus
        (exwm-input--warp-blocked-p window)
        (bound-and-true-p exwm-workspace--showing-desktop))
       (window-live-p window)
       (let ((frame (window-frame window)))
         (or (eq frame exwm-workspace--current)
             (eq (frame-parameter frame 'exwm-floating-workspace)
                 exwm-workspace--current)))))

(defun exwm-input--warp-to-window (window)
  "Warp the pointer to the center of WINDOW.
The following EnterNotify is ignored briefly so the warp does not
move focus again."
  (when (and exwm--connection (window-live-p window))
    (let* ((frame (window-frame window))
           (xid (frame-parameter frame 'window-id))
           (edges (exwm--window-inside-pixel-edges window))
           (width (- (elt edges 2) (elt edges 0)))
           (height (- (elt edges 3) (elt edges 1))))
      (when (and (stringp xid) (> width 0) (> height 0))
        (setq exwm-input--warp-until (+ (float-time) 0.2)
              exwm-input--pointer-focus-window window)
        (xcb:+request exwm--connection
            (make-instance 'xcb:WarpPointer
                           :src-window xcb:Window:None
                           :dst-window (string-to-number xid)
                           :src-x 0
                           :src-y 0
                           :src-width 0
                           :src-height 0
                           :dst-x (+ (elt edges 0) (/ width 2))
                           :dst-y (+ (elt edges 1) (/ height 2))))
        (xcb:flush exwm--connection)))))

(defun exwm-input--output-in-direction-p (direction here out)
  "Return non-nil when OUT lies DIRECTION from HERE.
HERE and OUT are (NAME X Y WIDTH HEIGHT).  The rectangle must
sit past HERE's edge in DIRECTION, or overlap the other axis
and extend past that edge."
  (let* ((x (nth 1 here))
         (y (nth 2 here))
         (w (nth 3 here))
         (h (nth 4 here))
         (ox (nth 1 out))
         (oy (nth 2 out))
         (ow (nth 3 out))
         (oh (nth 4 out))
         (vertical (and (< oy (+ y h)) (< y (+ oy oh))))
         (horizontal (and (< ox (+ x w)) (< x (+ ox ow)))))
    (pcase direction
      ('right (or (>= ox (+ x w))
                  (and vertical
                       (> (+ ox (/ ow 2)) (+ x (/ w 2)))
                       (> (+ ox ow) (+ x w)))))
      ('left (or (<= (+ ox ow) x)
                 (and vertical
                      (< (+ ox (/ ow 2)) (+ x (/ w 2)))
                      (< ox x))))
      ('down (or (>= oy (+ y h))
                 (and horizontal
                      (> (+ oy (/ oh 2)) (+ y (/ h 2)))
                      (> (+ oy oh) (+ y h)))))
      ('up (or (<= (+ oy oh) y)
               (and horizontal
                    (< (+ oy (/ oh 2)) (+ y (/ h 2)))
                    (< oy y)))))))

(defun exwm-input--output-distance (direction here out)
  "Return (GAP PERP) from HERE to OUT in DIRECTION.
A smaller gap is closer.  A negative gap is an overlap."
  (let ((x (nth 1 here))
        (y (nth 2 here))
        (w (nth 3 here))
        (h (nth 4 here))
        (ox (nth 1 out))
        (oy (nth 2 out))
        (ow (nth 3 out))
        (oh (nth 4 out)))
    (pcase direction
      ('right (list (- ox (+ x w))
                    (abs (- (+ oy (/ oh 2)) (+ y (/ h 2))))))
      ('left (list (- x (+ ox ow))
                   (abs (- (+ oy (/ oh 2)) (+ y (/ h 2))))))
      ('down (list (- oy (+ y h))
                   (abs (- (+ ox (/ ow 2)) (+ x (/ w 2))))))
      ('up (list (- y (+ oy oh))
                 (abs (- (+ ox (/ ow 2)) (+ x (/ w 2)))))))))

(defun exwm-input--closer-p (a b)
  "Return non-nil when distance A is closer than distance B."
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cadr a) (cadr b)))))

(defun exwm-input--neighbor-output (direction current outputs)
  "Return the output in DIRECTION from CURRENT.
OUTPUTS is a list of (NAME X Y WIDTH HEIGHT).  CURRENT is a name.
An output that shares the perpendicular edge beats a diagonal
one.  Return nil when CURRENT is unknown or nothing lies that way."
  (let ((here (and (stringp current) (assoc current outputs))))
    (when (and here (memq direction '(left right up down)))
      (let ((best nil)
            (best-distance nil))
        (dolist (out outputs)
          (when (and (not (equal (car out) current))
                     (exwm-input--output-in-direction-p direction here out))
            (let ((distance (exwm-input--output-distance direction here out)))
              (when (or (null best-distance)
                        (exwm-input--closer-p distance best-distance))
                (setq best (car out)
                      best-distance distance)))))
        best))))

(defun exwm-input--frame-on-output (name rows)
  "Return the frame on output NAME.
ROWS is a list of (FRAME MONITOR ACTIVE) in workspace order.
The active frame wins.  Otherwise the first frame on NAME.
Nil when NAME is not among ROWS."
  (when (and (stringp name) (not (string-empty-p name)))
    (or (car (cl-find-if (lambda (row)
                           (and (equal (nth 1 row) name)
                                (nth 2 row)))
                         rows))
        (car (cl-find-if (lambda (row)
                           (equal (nth 1 row) name))
                         rows)))))

(defvar exwm-workspace--list)
(defvar exwm-workspace--showing-desktop)
(declare-function exwm-workspace--active-p "exwm-workspace.el" (frame))
(declare-function exwm-randr--get-monitors "exwm-randr.el" ())

(defun exwm-input--output-geometries ()
  "Return ((NAME X Y WIDTH HEIGHT) ...) from RandR, or nil.
Nil means RandR is not connected.  Callers then keep `windmove'."
  (when (and (bound-and-true-p exwm-randr--connection)
             (fboundp 'exwm-randr--get-monitors))
    (mapcar (lambda (cell)
              (let ((geometry (cdr cell)))
                (list (car cell)
                      (slot-value geometry 'x)
                      (slot-value geometry 'y)
                      (slot-value geometry 'width)
                      (slot-value geometry 'height))))
            (nth 1 (exwm-randr--get-monitors)))))

(defun exwm-input--windmove (direction)
  "Select the window in DIRECTION.  Return non-nil on success.
`windmove-wrap-around' still applies, so a wrap stays in the frame."
  (condition-case nil
      (progn
        (pcase direction
          ('left (windmove-left))
          ('right (windmove-right))
          ('up (windmove-up))
          ('down (windmove-down)))
        t)
    (error nil)))

(defun exwm-input--focus-output (direction outputs)
  "Switch to the workspace active on the monitor in DIRECTION.
OUTPUTS is the list from `exwm-input--output-geometries'."
  (let* ((current (and (frame-live-p exwm-workspace--current)
                       (frame-parameter exwm-workspace--current
                                        'exwm-randr-monitor)))
         (neighbor (exwm-input--neighbor-output direction current outputs)))
    (unless neighbor
      (user-error "[EXWM] No monitor to the %s" direction))
    (let ((frame (exwm-input--frame-on-output
                  neighbor
                  (mapcar (lambda (workspace)
                            (list workspace
                                  (frame-parameter workspace
                                                   'exwm-randr-monitor)
                                  (exwm-workspace--active-p workspace)))
                          exwm-workspace--list))))
      (unless (frame-live-p frame)
        (user-error "[EXWM] No workspace on %s" neighbor))
      (exwm-workspace-switch frame))))

(defun exwm-input--focus-direction (direction &optional output-only)
  "Focus DIRECTION, crossing to the next monitor when needed.
OUTPUT-ONLY skips `windmove'.  With RandR off, a window command
is `windmove' and a monitor command reports that RandR is off."
  (cond
   ((and (not output-only) (exwm-input--windmove direction)))
   (t
    (let ((outputs (exwm-input--output-geometries)))
      (cond
       (outputs
        (exwm-input--focus-output direction outputs))
       (output-only
        (user-error "[EXWM] RandR is not active"))
       (t (pcase direction
            ('left (windmove-left))
            ('right (windmove-right))
            ('up (windmove-up))
            ('down (windmove-down)))))))))

(defun exwm-focus-left ()
  "Focus the window on the left, then the monitor on the left.
With RandR off, this is `windmove-left'.  A wrap from
`windmove-wrap-around' stays on this monitor.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'left))

(defun exwm-focus-right ()
  "Focus the window on the right, then the monitor on the right.
With RandR off, this is `windmove-right'.  A wrap from
`windmove-wrap-around' stays on this monitor.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'right))

(defun exwm-focus-up ()
  "Focus the window above, then the monitor above.
With RandR off, this is `windmove-up'.  A wrap from
`windmove-wrap-around' stays on this monitor.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'up))

(defun exwm-focus-down ()
  "Focus the window below, then the monitor below.
With RandR off, this is `windmove-down'.  A wrap from
`windmove-wrap-around' stays on this monitor.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'down))

(defun exwm-output-focus-left ()
  "Focus the monitor on the left.
This skips `windmove'.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'left t))

(defun exwm-output-focus-right ()
  "Focus the monitor on the right.
This skips `windmove'.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'right t))

(defun exwm-output-focus-up ()
  "Focus the monitor above.
This skips `windmove'.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'up t))

(defun exwm-output-focus-down ()
  "Focus the monitor below.
This skips `windmove'.  No key is bound."
  (interactive)
  (exwm-input--focus-direction 'down t))

(defun exwm-input--set-active-window (id)
  "Set _NET_ACTIVE_WINDOW to ID."
  (exwm--log)
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_ACTIVE_WINDOW
                     :window exwm--root
                     :data id)))

(defun exwm-input--on-ButtonPress (data _synthetic)
  "Handle ButtonPress event with DATA."
  (exwm--log "major-mode=%s buffer=%s"
             major-mode (buffer-name (current-buffer)))
  (with-slots (detail event state)
      (xcb:unmarshal-new 'xcb:ButtonPress data)
    (let* ((mode xcb:Allow:SyncPointer)
           (button-event (xcb:keysyms:keysym->event exwm--connection
                                                    detail state))
           (buffer (exwm--id->buffer event))
           fake-last-command)
      (cond ((and (eq button-event exwm-input-move-event)
                  buffer
                  ;; Either an undecorated or a floating X window.
                  (with-current-buffer buffer
                    (or (not (derived-mode-p 'exwm-mode))
                        exwm--floating-frame)))
             ;; Move
             (exwm-floating--start-moveresize
              event xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
            ((and (eq button-event exwm-input-resize-event)
                  buffer
                  (with-current-buffer buffer
                    (or (not (derived-mode-p 'exwm-mode))
                        exwm--floating-frame)))
             ;; Resize
             (exwm-floating--start-moveresize event))
            (buffer
             ;; Click to focus, and raise.  Pointer entry does not.
             (setq exwm-input--pointer-focus-until nil
                   fake-last-command t)
             (when-let* ((clicked (get-buffer-window buffer t)))
               (setq exwm-input--pointer-focus-window clicked))
             (with-current-buffer buffer
               (when exwm--floating-frame
                 (exwm-layout--raise-floating)
                 (xcb:flush exwm--connection)))
             (when-let* ((window (get-buffer-window buffer t))
                         (_(not (eq window (selected-window)))))
               (when-let* ((frame (window-frame window))
                           (_(not (eq frame exwm-workspace--current))))
                 (if (exwm-workspace--workspace-p frame)
                     ;; The X window is on another workspace
                     (exwm-workspace-switch frame)
                   (with-current-buffer buffer
                     (when (and (derived-mode-p 'exwm-mode)
                                (not (eq exwm--frame
                                         exwm-workspace--current)))
                       ;; The floating X window is on another workspace
                       (exwm-workspace-switch exwm--frame)))))
               ;; It has been reported that the `window' may have be deleted
               (unless (window-live-p window)
                 (setq window (get-buffer-window buffer t)))
               (when window (select-window window)))
             ;; Also process keybindings.
             (with-current-buffer buffer
               (when (derived-mode-p 'exwm-mode)
                 (cl-case exwm--input-mode
                   (line-mode
                    (setq mode (exwm-input--on-ButtonPress-line-mode
                                buffer button-event)))
                   (char-mode
                    (setq mode (exwm-input--on-ButtonPress-char-mode)))))))
            (t
             ;; Replay this event by default.
             (setq fake-last-command t)
             (setq mode xcb:Allow:ReplayPointer)))
      (when fake-last-command
        (if buffer
            (with-current-buffer buffer
              (exwm-input--fake-last-command))
          (exwm-input--fake-last-command)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents :mode mode :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection)))
  (run-hooks 'exwm-input--event-hook))

(defun exwm-input--on-KeyPress (data _synthetic)
  "Handle KeyPress event with DATA."
  (with-current-buffer (window-buffer (selected-window))
    (let ((obj (xcb:unmarshal-new 'xcb:KeyPress data)))
      (exwm--log "major-mode=%s buffer=%s"
                 major-mode (buffer-name (current-buffer)))
      (if (derived-mode-p 'exwm-mode)
          (cl-case exwm--input-mode
            (line-mode
             (exwm-input--on-KeyPress-line-mode obj data))
            (char-mode
             (exwm-input--on-KeyPress-char-mode obj data)))
        (exwm-input--on-KeyPress-char-mode obj)))
    (run-hooks 'exwm-input--event-hook)))

(defun exwm-input--on-CreateNotify (data _synthetic)
  "Handle CreateNotify events with DATA."
  (exwm--log)
  (with-slots (window) (xcb:unmarshal-new 'xcb:CreateNotify data)
    (exwm-input--grab-global-prefix-keys window)))

(defun exwm-input--update-global-prefix-keys ()
  "Update `exwm-input--global-prefix-keys'."
  (exwm--log)
  (when exwm--connection
    (let ((original exwm-input--global-prefix-keys))
      (setq exwm-input--global-prefix-keys nil)
      (dolist (i exwm-input--global-keys)
        (cl-pushnew (exwm-input--canonicalize-event (elt i 0))
                    exwm-input--global-prefix-keys))
      (unless (equal original exwm-input--global-prefix-keys)
        (apply #'exwm-input--grab-global-prefix-keys
               (slot-value (xcb:+request-unchecked+reply exwm--connection
                               (make-instance 'xcb:QueryTree
                                              :window exwm--root))
                           'children))))))

(defun exwm-input--grab-global-prefix-keys (&rest xwins)
  "Grab global prefix keys in XWINS."
  (exwm--log)
  (let ((req (make-instance 'xcb:GrabKey
                            :owner-events 0
                            :grab-window nil
                            :modifiers nil
                            :key nil
                            :pointer-mode xcb:GrabMode:Async
                            :keyboard-mode xcb:GrabMode:Async))
        keysyms keycode alt-modifier)
    (dolist (k exwm-input--global-prefix-keys)
      (setq keysyms (xcb:keysyms:event->keysyms exwm--connection k))
      (if (= 0 (caar keysyms))
          (warn "Key unavailable: %s" (key-description (vector k)))
        (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                                   (caar keysyms)))
        (when (/= 0 keycode)
          (exwm--log "Grabbing key=%s (keysyms=%s keycode=%s)"
                     (single-key-description k) keysyms keycode)
          (dolist (keysym keysyms)
            (setf (slot-value req 'modifiers) (cdr keysym)
                  (slot-value req 'key) keycode)
            ;; Also grab this key with num-lock mask set.
            (when (and (/= 0 xcb:keysyms:num-lock-mask)
                       (= 0 (logand (cdr keysym) xcb:keysyms:num-lock-mask)))
              (setf alt-modifier (logior (cdr keysym)
                                         xcb:keysyms:num-lock-mask)))
            (dolist (xwin xwins)
              (setf (slot-value req 'grab-window) xwin)
              (xcb:+request exwm--connection req)
              (when alt-modifier
                (setf (slot-value req 'modifiers) alt-modifier)
                (xcb:+request exwm--connection req)))))))
    (apply #'exwm-input--grab-modifiers xwins)
    (xcb:flush exwm--connection)))

(defun exwm-input--canonicalize-event (event)
  "Return EVENT with modifiers in Emacs's canonical order.

Character events already ignore modifier order.  Symbol events such
as `s-C-left' and `C-s-left' do not, but the event EXWM receives from
X is canonical.  `event-convert-list' changes a character event, so
characters are returned unchanged."
  (if (or (symbolp event)
          (and (integerp event)
               (symbolp (event-basic-type event))))
      (or (event-convert-list
           (append (event-modifiers event)
                   (list (event-basic-type event))))
          event)
    event))

(defun exwm-input--canonicalize-key (key)
  "Return KEY with each event in canonical modifier order."
  (if (vectorp key)
      (apply #'vector (mapcar #'exwm-input--canonicalize-event key))
    key))

(defun exwm-input--set-key (key command)
  "Set KEY to COMMAND."
  (setq key (exwm-input--canonicalize-key key))
  (exwm--log "key: %s, command: %s" key command)
  (global-set-key key command)
  (cl-pushnew key exwm-input--global-keys))

(defcustom exwm-input-global-keys nil
  "Global keys.

It is an alist of the form (key . command), meaning giving KEY (a key
sequence) a global binding as COMMAND.

Notes:
* Setting the value directly (rather than customizing it) after EXWM
  finishes initialization has no effect."
  :type '(alist :key-type key-sequence :value-type function)
  :set (lambda (symbol value)
         (when (boundp symbol)
           (dolist (i (symbol-value symbol))
             (global-unset-key (car i))))
         (set symbol value)
         (setq exwm-input--global-keys nil)
         (dolist (i value)
           (exwm-input--set-key (car i) (cdr i)))
         (when exwm--connection
           (exwm-input--update-global-prefix-keys))))

(defun exwm-input-set-key (key command)
  "Set a global KEY binding to COMMAND.

The new binding only takes effect in real time when this command is
called interactively, and is lost when this session ends unless it's
specifically saved in the Customize interface for `exwm-input-global-keys'.

In configuration you should customize or set `exwm-input-global-keys'
instead."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (exwm--log)
  (setq exwm-input-global-keys (append exwm-input-global-keys
                                       (list (cons key command))))
  (exwm-input--set-key key command)
  (when (called-interactively-p 'any)
    (exwm-input--update-global-prefix-keys)))

(defsubst exwm-input--unread-event (event)
  "Append EVENT to `unread-command-events'."
  (declare (indent defun))
  (setq unread-command-events
        (append unread-command-events `((t . ,event)))))

(defun exwm-input--mimic-read-event (event)
  "Process EVENT as if it were returned by `read-event'."
  (exwm--log)
  (unless (eq 0 extra-keyboard-modifiers)
    (setq event (event-convert-list (append (event-modifiers
                                             extra-keyboard-modifiers)
                                            event))))
  (when (characterp event)
    (let ((event* (when keyboard-translate-table
                    (aref keyboard-translate-table event))))
      (when event*
        (setq event event*))))
  event)

(cl-defun exwm-input--translate (key)
  "Translate KEY."
  (let (translation)
    (dolist (map (list input-decode-map
                       local-function-key-map
                       key-translation-map))
      (setq translation (lookup-key map key))
      (if (functionp translation)
          (cl-return-from exwm-input--translate (funcall translation nil))
        (when (vectorp translation)
          (cl-return-from exwm-input--translate translation)))))
  key)

(defun exwm-input--cache-event (event &optional temp-line-mode)
  "Cache EVENT.
When non-nil, TEMP-LINE-MODE temporarily puts the window in line mode."
  (exwm--log "%s" event)
  (setq exwm-input--line-mode-cache
        (vconcat exwm-input--line-mode-cache (vector event)))
  ;; Attempt to translate this key sequence.
  (setq exwm-input--line-mode-cache
        (exwm-input--translate exwm-input--line-mode-cache))
  ;; When the key sequence is complete (not a keymap).
  ;; Note that `exwm-input--line-mode-cache' might get translated to nil, for
  ;; example 'mouse--down-1-maybe-follows-link' does this.
  (if (and exwm-input--line-mode-cache
           (keymapp (key-binding exwm-input--line-mode-cache)))
      ;; Grab keyboard temporarily to intercept the complete key sequence.
      (when temp-line-mode
        (setq exwm-input--temp-line-mode t)
        (exwm-input--grab-keyboard))
    (setq exwm-input--line-mode-cache nil)
    (when exwm-input--temp-line-mode
      (setq exwm-input--temp-line-mode nil)
      (exwm-input--release-keyboard))))

(defun exwm-input--event-passthrough-p (event)
  "Whether EVENT should be passed to Emacs.
Current buffer must be an `exwm-mode' buffer."
  (or exwm-input-line-mode-passthrough
      ;; Forward the event when there is an incomplete key
      ;; sequence or when the minibuffer is active.
      exwm-input--line-mode-cache
      (eq (active-minibuffer-window) (selected-window))
      ;;
      (memq (exwm-input--canonicalize-event event)
            exwm-input--global-prefix-keys)
      (memq event exwm-input-prefix-keys)
      (exwm-input--modifier-event-p event)
      ;; `C-g' is not a prefix key.  Putting it in
      ;; `exwm-input-prefix-keys' would also change simulation keys.
      (eq event ?\C-g)
      (when overriding-terminal-local-map
        (lookup-key overriding-terminal-local-map
                    (vector event)))
      (lookup-key (current-local-map) (vector event))
      (gethash event exwm-input--simulation-keys)))

(defun exwm-input--noop (&rest _args)
  "A placeholder command."
  (interactive))
(put #'exwm-input--noop 'completion-predicate #'ignore) ;; Move to declare in Emacs 28

(defun exwm-input--fake-last-command ()
  "Fool some packages into thinking there is a change in the buffer."
  (setq last-command #'exwm-input--noop)
  ;; The Emacs manual says:
  ;; > Quitting is suppressed while running pre-command-hook and
  ;; > post-command-hook. If an error happens while executing one of these
  ;; > hooks, it does not terminate execution of the hook; instead the error is
  ;; > silenced and the function in which the error occurred is removed from the
  ;; > hook.
  ;; We supress errors but neither continue execution nor we remove from the
  ;; hook.
  (condition-case err
      (run-hooks 'pre-command-hook)
    ((error)
     (exwm--log "Error occurred while running pre-command-hook: %s"
                (error-message-string err))
     (xcb-debug:backtrace)))
  (condition-case err
      (run-hooks 'post-command-hook)
    ((error)
     (exwm--log "Error occurred while running post-command-hook: %s"
                (error-message-string err))
     (xcb-debug:backtrace))))

(defun exwm-input--on-KeyPress-line-mode (keypress raw-data)
  "Feed parsed X KEYPRESS event with RAW-DATA to Emacs command loop."
  (with-slots (detail state) keypress
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event raw-event mode)
      (exwm--log "%s" keysym)
      (when (and (/= 0 (car keysym))
                 (setq raw-event (xcb:keysyms:keysym->event
                                  exwm--connection (car keysym)
                                  (logand state (lognot (cdr keysym)))))
                 (setq event (exwm-input--mimic-read-event raw-event))
                 (exwm-input--event-passthrough-p event))
        (setq mode xcb:Allow:AsyncKeyboard)
        (exwm-input--cache-event event)
        (exwm-input--unread-event raw-event))
      (unless mode
        (if (= 0 (logand #x6000 state)) ;Check the 13~14 bits.
            ;; Not an XKB state; just replay it.
            (setq mode xcb:Allow:ReplayKeyboard)
          ;; An XKB state; sent it with SendEvent.
          ;; FIXME: Can this also be replayed?
          ;; FIXME: KeyRelease events are lost.
          (setq mode xcb:Allow:AsyncKeyboard)
          (xcb:+request exwm--connection
              (make-instance 'xcb:SendEvent
                             :propagate 0
                             :destination (slot-value keypress 'event)
                             :event-mask xcb:EventMask:NoEvent
                             :event raw-data)))
        (when event
          (if (not defining-kbd-macro)
              (exwm-input--fake-last-command)
            ;; Make Emacs aware of this event when defining keyboard macros.
            (set-transient-map `(keymap (t . ,#'exwm-input--noop)))
            (exwm-input--unread-event event))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents
                         :mode mode
                         :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection))))

(defun exwm-input--on-KeyPress-char-mode (keypress &optional _raw-data)
  "Handle `char-mode' KEYPRESS event."
  (with-slots (detail state) keypress
    (let ((keysym (xcb:keysyms:keycode->keysym exwm--connection detail state))
          event raw-event)
      (exwm--log "%s" keysym)
      (when (and (/= 0 (car keysym))
                 (setq raw-event (xcb:keysyms:keysym->event
                                  exwm--connection (car keysym)
                                  (logand state (lognot (cdr keysym)))))
                 (setq event (exwm-input--mimic-read-event raw-event)))
        (if (not (derived-mode-p 'exwm-mode))
            (exwm-input--unread-event raw-event)
          (exwm-input--cache-event event t)
          (exwm-input--unread-event raw-event)))))
  (xcb:+request exwm--connection
      (make-instance 'xcb:AllowEvents
                     :mode xcb:Allow:AsyncKeyboard
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))

(defun exwm-input--on-ButtonPress-line-mode (buffer button-event)
  "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on.  BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
  (with-current-buffer buffer
    (let ((read-event (exwm-input--mimic-read-event button-event)))
      (exwm--log "%s" read-event)
      (if (and read-event
               (exwm-input--event-passthrough-p read-event))
          ;; The event should be forwarded to emacs
          (progn
            (exwm-input--cache-event read-event)
            (exwm-input--unread-event button-event)
            xcb:Allow:SyncPointer)
        ;; The event should be replayed
        xcb:Allow:ReplayPointer))))

(defun exwm-input--on-ButtonPress-char-mode ()
  "Handle button events in `char-mode'.
The return value is used as event_mode to release the original
button event."
  (exwm--log)
  xcb:Allow:ReplayPointer)

(defun exwm-input--update-mode-line (id)
  "Update the propertized `mode-line-process' for window ID."
  (exwm--log "#x%x" id)
  (let (help-echo cmd mode)
    (with-current-buffer (exwm--id->buffer id)
      (cl-case exwm--input-mode
        (line-mode
         (setq mode "line"
               help-echo "mouse-1: Switch to char-mode"
               cmd (lambda ()
                     (interactive)
                     (exwm-input-release-keyboard id))))
        (char-mode
         (setq mode "char"
               help-echo "mouse-1: Switch to line-mode"
               cmd (lambda ()
                     (interactive)
                     (exwm-input-grab-keyboard id)))))
      (setq mode-line-process
            `(": "
              (:propertize ,mode
                           help-echo ,help-echo
                           mouse-face mode-line-highlight
                           local-map
                           (keymap
                            (mode-line
                             keymap
                             (down-mouse-1 . ,cmd))))))
      (force-mode-line-update))))

(defun exwm-input--grab-keyboard (&optional id)
  "Grab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (exwm--log "id=#x%x" id)
    ;; AnyModifier cannot be grabbed while a reserved-modifier grab
    ;; is still installed on this window.
    (exwm-input--ungrab-modifiers id)
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:GrabKey
                             :owner-events 0
                             :grab-window id
                             :modifiers xcb:ModMask:Any
                             :key xcb:Grab:Any
                             :pointer-mode xcb:GrabMode:Async
                             :keyboard-mode xcb:GrabMode:Sync))
      (exwm--log "Failed to grab keyboard for #x%x" id))
    (let ((buffer (exwm--id->buffer id)))
      (when buffer
        (with-current-buffer buffer
          (setq exwm--input-mode 'line-mode)
          (run-hooks 'exwm-input-input-mode-change-hook))))))

(defun exwm-input--release-keyboard (&optional id)
  "Ungrab all key events on window ID."
  (unless id (setq id (exwm--buffer->id (window-buffer))))
  (when id
    (exwm--log "id=#x%x" id)
    (when (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:UngrabKey
                             :key xcb:Grab:Any
                             :grab-window id
                             :modifiers xcb:ModMask:Any))
      (exwm--log "Failed to release keyboard for #x%x" id))
    (exwm-input--grab-global-prefix-keys id)
    (let ((buffer (exwm--id->buffer id)))
      (when buffer
        (with-current-buffer buffer
          (setq exwm--input-mode 'char-mode)
          (run-hooks 'exwm-input-input-mode-change-hook))))))

(defun exwm-input-grab-keyboard (&optional id)
  "Switch to `line-mode`.
When ID is non-nil, grab key events on its corresponding window."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (setq exwm--selected-input-mode 'line-mode)
    (exwm-input--grab-keyboard id)
    (exwm-input--update-mode-line id)))

(defun exwm-input-release-keyboard (&optional id)
  "Switch to `char-mode`.
When ID is non-nil, release keyboard events on its corresponding window."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (setq exwm--selected-input-mode  'char-mode)
    (exwm-input--release-keyboard id)
    (exwm-input--update-mode-line id)))

(defun exwm-input-toggle-keyboard (&optional id)
  "Toggle between `line-mode' and `char-mode'.
When ID is non-nil, toggle in its correpsonding window."
  (interactive (list (when (derived-mode-p 'exwm-mode)
                       (exwm--buffer->id (window-buffer)))))
  (when id
    (exwm--log "id=#x%x" id)
    (with-current-buffer (exwm--id->buffer id)
      (cl-case exwm--input-mode
        (line-mode
         (exwm-input-release-keyboard id))
        (char-mode
         (exwm-reset))))))

(defvar exwm-input--xtest nil
  "Non-nil when the server's XTEST extension can inject key events.")

(defvar exwm-input--xtest-modifiers nil
  "Modifiers XTEST may press and release while injecting a key.
Each element is (MASK KEYCODE...).  Locking modifiers such as
Caps Lock and Num Lock are omitted: a press toggles them instead
of holding them.")

(defconst exwm-input--xtest-lock-keysyms
  '(#xffe5 #xffe6 #xff7f #xff14 #xfe01)
  "Keysyms that lock rather than hold: Caps, Shift Lock, Num, Scroll, ISO Lock.")

(defun exwm-input--xtest-sync ()
  "Wait until the X server has processed requests already sent."
  (xcb:+request-unchecked+reply exwm--connection
      (make-instance 'xcb:GetInputFocus)))

(defun exwm-input--xtest-refresh-modifiers ()
  "Cache keycodes of modifiers that XTEST can hold temporarily."
  (setq exwm-input--xtest-modifiers nil)
  (when exwm-input--xtest
    (let ((reply (xcb:+request-unchecked+reply exwm--connection
                     (make-instance 'xcb:GetModifierMapping))))
      (when reply
        (with-slots (keycodes-per-modifier keycodes) reply
          (when (> keycodes-per-modifier 0)
            (let ((masks (list xcb:ModMask:Shift
                               xcb:ModMask:Lock
                               xcb:ModMask:Control
                               xcb:ModMask:1
                               xcb:ModMask:2
                               xcb:ModMask:3
                               xcb:ModMask:4
                               xcb:ModMask:5))
                  entries)
              (dotimes (i 8)
                (let (codes lock)
                  (dotimes (j keycodes-per-modifier)
                    (let ((code (elt keycodes
                                     (+ (* i keycodes-per-modifier) j))))
                      (when (and code (> code 0))
                        (push code codes)
                        (when (memq (car (xcb:keysyms:keycode->keysym
                                          exwm--connection code 0))
                                    exwm-input--xtest-lock-keysyms)
                          (setq lock t)))))
                  (when (and codes (not lock))
                    (push (cons (nth i masks) (nreverse codes)) entries))))
              (setq exwm-input--xtest-modifiers (nreverse entries)))))))))

(defun exwm-input--xtest-key-down-p (keymap keycode)
  "Whether KEYMAP, a 32-byte `QueryKeymap' result, shows KEYCODE held."
  (and keymap
       (<= 0 keycode 255)
       (/= 0 (logand (elt keymap (/ keycode 8))
                     (ash 1 (% keycode 8))))))

(defun exwm-input--xtest-event (keycode press)
  "Send one XTEST key press or release of KEYCODE.
PRESS non-nil sends KeyPress.  The event is not marked SendEvent."
  (xcb:+request exwm--connection
      (make-instance 'xcb:xtest:FakeInput
                     ;; 2 is KeyPress and 3 is KeyRelease.
                     :type (if press 2 3)
                     :detail keycode
                     :time xcb:Time:CurrentTime
                     :root xcb:Window:None
                     :rootX 0
                     :rootY 0
                     :deviceid 0)))

(defun exwm-input--xtest-restore-grabs (id)
  "Put back the key grabs on window ID after an XTEST injection."
  (let* ((buffer (exwm--id->buffer id))
         (line (and buffer
                    (eq (buffer-local-value 'exwm--input-mode buffer)
                        'line-mode))))
    (if line
        (progn
          ;; AnyModifier cannot be grabbed while a reserved-modifier
          ;; grab is still installed.
          (exwm-input--ungrab-modifiers id)
          (when (xcb:+request-checked+request-check exwm--connection
                    (make-instance 'xcb:GrabKey
                                   :owner-events 0
                                   :grab-window id
                                   :modifiers xcb:ModMask:Any
                                   :key xcb:Grab:Any
                                   :pointer-mode xcb:GrabMode:Async
                                   :keyboard-mode xcb:GrabMode:Sync))
            (exwm--log "Failed to restore keyboard grab for #x%x" id)))
      (exwm-input--grab-global-prefix-keys id))
    (xcb:flush exwm--connection)))

(defun exwm-input--xtest-key (id keycode state)
  "Inject KEYCODE with modifier STATE into the client that has ID.
XTEST events are ordinary key events, so clients that ignore
SendEvent still receive them.  Modifiers in STATE that are not held
are pressed for the duration of the key, and held modifiers that are
not in STATE are released, then both are restored.  The grab is
lifted first: line-mode's grab would otherwise consume the keys."
  (let* ((reply (xcb:+request-unchecked+reply exwm--connection
                    (make-instance 'xcb:QueryKeymap)))
         (keymap (and reply (slot-value reply 'keys)))
         release press)
    (dolist (entry exwm-input--xtest-modifiers)
      (let* ((mask (car entry))
             (codes (cdr entry))
             (want (/= 0 (logand (or state 0) mask)))
             held)
        (dolist (code codes)
          (when (exwm-input--xtest-key-down-p keymap code)
            (setq held t)))
        (cond ((and held (not want))
               (dolist (code codes)
                 (when (exwm-input--xtest-key-down-p keymap code)
                   (push code release))))
              ((and want (not held))
               (push (car codes) press)))))
    (setq release (nreverse release)
          press (nreverse press))
    (unwind-protect
        (progn
          ;; The key that invoked simulation may still own an active grab.
          ;; UngrabKey does not release that; UngrabKeyboard does.
          (xcb:+request-checked+request-check exwm--connection
              (make-instance 'xcb:UngrabKeyboard
                             :time xcb:Time:CurrentTime))
          (xcb:+request exwm--connection
              (make-instance 'xcb:UngrabKey
                             :key xcb:Grab:Any
                             :grab-window id
                             :modifiers xcb:ModMask:Any))
          (exwm-input--xtest-sync)
          (dolist (code release)
            (exwm-input--xtest-event code nil))
          (dolist (code press)
            (exwm-input--xtest-event code t))
          (exwm-input--xtest-event keycode t)
          (exwm-input--xtest-event keycode nil)
          (dolist (code (reverse press))
            (exwm-input--xtest-event code nil))
          (dolist (code (reverse release))
            (exwm-input--xtest-event code t))
          (exwm-input--xtest-sync))
      (exwm-input--xtest-restore-grabs id))))

(defun exwm-input--fake-key (event &optional id)
  "Fake a key event equivalent to Emacs event EVENT.
XTEST is used when the server supports it, because clients such as
GTK 4 and Wine ignore events sent with SendEvent.  SendEvent remains
the fallback.  ID is the target X window; it defaults to the selected
window's client."
  (let* ((keysyms (xcb:keysyms:event->keysyms exwm--connection event))
         keycode)
    (when (= 0 (caar keysyms))
      (user-error "[EXWM] Invalid key: %s" (single-key-description event)))
    (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                               (caar keysyms)))
    (when (/= 0 keycode)
      (setq id (or id (exwm--buffer->id (window-buffer (selected-window)))))
      (exwm--log "id=#x%x event=%s keycode=%s xtest=%s"
                 id event keycode exwm-input--xtest)
      (if (and exwm-input--xtest id)
          (exwm-input--xtest-key id keycode (cdar keysyms))
        (dolist (class '(xcb:KeyPress xcb:KeyRelease))
          (xcb:+request exwm--connection
              (make-instance 'xcb:SendEvent
                             :propagate 0 :destination id
                             :event-mask xcb:EventMask:NoEvent
                             :event (xcb:marshal
                                     (make-instance class
                                                    :detail keycode
                                                    :time xcb:Time:CurrentTime
                                                    :root exwm--root :event id
                                                    :child 0
                                                    :root-x 0 :root-y 0
                                                    :event-x 0 :event-y 0
                                                    :state (cdar keysyms)
                                                    :same-screen 1)
                                     exwm--connection))))))
    (xcb:flush exwm--connection)))

(cl-defun exwm-input-send-next-key (n &optional end-key)
  "Send next N keys to client window.
N is currently capped at 12.
EXWM will prompt for the key to send.
If END-KEY is non-nil, stop sending keys if it's pressed."
  (interactive "p")
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode) (cl-return-from exwm-input-send-next-key))
  (setq n (min n 12))
  (let (key keys)
    (dotimes (i n)
      ;; Skip events not from keyboard
      (let ((exwm-input-line-mode-passthrough t))
        (catch 'break
          (while t
            (setq key (read-key (format "Send key: %s (%d/%d) %s"
                                        (key-description keys)
                                        (1+ i) n
                                        (if end-key
                                            (concat "To exit, press: "
                                                    (key-description
                                                     (list end-key)))
                                          ""))))
            (unless (listp key) (throw 'break nil)))))
      (setq keys (vconcat keys (vector key)))
      (when (eq key end-key) (cl-return-from exwm-input-send-next-key))
      (exwm-input--fake-key key))))

(defun exwm-input--set-simulation-keys (keys &optional cache local)
  "Set simulation KEYS.
If CACHE is non-nil, reuse `exwm-input--simulation-keys' cache.
If LOCAL is non-nil, bind the keys in the current buffer only."
  (exwm--log "%s" keys)
  (unless cache
    ;; Unbind simulation keys.
    (let ((hash (buffer-local-value 'exwm-input--simulation-keys
                                    (current-buffer))))
      (when (hash-table-p hash)
        (maphash (lambda (key _value)
                   (when (sequencep key)
                     (if local
                         (local-unset-key key)
                       (define-key exwm-mode-map key nil))))
                 hash)))
    ;; Abandon the old hash table.
    (setq exwm-input--simulation-keys (make-hash-table :test #'equal)))
  (dolist (i keys)
    (let ((original (vconcat (car i)))
          (simulated (cdr i)))
      (setq simulated (if (sequencep simulated)
                          (append simulated nil)
                        (list simulated)))
      ;; The key stored is a key sequence (vector).
      ;; The value stored is a list of key events.
      (puthash original simulated exwm-input--simulation-keys)
      ;; Also mark the prefix key as used.
      (puthash (aref original 0) t exwm-input--simulation-keys)))
  ;; Update keymaps.
  (maphash (lambda (key _value)
             (when (sequencep key)
               (if local
                   (local-set-key key #'exwm-input-send-simulation-key)
                 (define-key exwm-mode-map key
                             #'exwm-input-send-simulation-key))))
           exwm-input--simulation-keys))

(defcustom exwm-input-simulation-keys nil
  "Simulation keys.

It is an alist of the form (original-key . simulated-key), where both
original-key and simulated-key are key sequences.  Original-key is what you
type to an X window in `line-mode' which then gets translated to simulated-key
by EXWM and forwarded to the X window.

Notes:
* Setting the value directly (rather than customizing it) after EXWM
  finishes initialization has no effect.
* Original-keys consist of multiple key events are only supported in Emacs
  26.2 and later.
* When the X server provides the XTEST extension, simulated keys are
  injected with it so clients that ignore SendEvent still receive them.
  The chord's modifiers are held only for that key, and EXWM lifts its
  grab while the key is injected.  Otherwise EXWM uses SendEvent, and
  those clients need their own setting to accept synthetic events.
* The predefined examples in the Customize interface are not guaranteed to
  work for all applications.  This can be tweaked on a per application basis
  with `exwm-input-set-local-simulation-keys'."
  :type '(alist :key-type (key-sequence :tag "Original")
                :value-type (choice (key-sequence :tag "User-defined")
                                    (key-sequence :tag "Move left" [left])
                                    (key-sequence :tag "Move right" [right])
                                    (key-sequence :tag "Move up" [up])
                                    (key-sequence :tag "Move down" [down])
                                    (key-sequence :tag "Move to BOL" [home])
                                    (key-sequence :tag "Move to EOL" [end])
                                    (key-sequence :tag "Page up" [prior])
                                    (key-sequence :tag "Page down" [next])
                                    (key-sequence :tag "Copy" [C-c])
                                    (key-sequence :tag "Paste" [C-v])
                                    (key-sequence :tag "Delete" [delete])
                                    (key-sequence :tag "Delete to EOL"
                                                  [S-end delete])))
  :set (lambda (symbol value)
         (set symbol value)
         (exwm-input--set-simulation-keys value)))

(cl-defun exwm-input--read-keys (prompt stop-key)
  "Read keys with PROMPT until STOP-KEY pressed."
  (let ((cursor-in-echo-area t)
        keys key)
    (while (not (eq key stop-key))
      (setq key (read-key (format "%s (terminate with %s): %s"
                                  prompt
                                  (key-description (vector stop-key))
                                  (key-description keys)))
            keys (vconcat keys (vector key))))
    (when (> (length keys) 1)
      (substring keys 0 -1))))

(defun exwm-input-set-simulation-key (original-key simulated-key)
  "Set ORIGINAL-KEY to  SIMULATED-KEY.

The simulation key takes effect in real time, but is lost when this session
ends unless it's specifically saved in the Customize interface for
`exwm-input-simulation-keys'."
  (interactive
   (let (original simulated)
     (setq original (exwm-input--read-keys "Translate from" ?\C-g))
     (when original
       (setq simulated (exwm-input--read-keys
                        (format "Translate from %s to"
                                (key-description original))
                        ?\C-g)))
     (list original simulated)))
  (exwm--log "original: %s, simulated: %s" original-key simulated-key)
  (when (and original-key simulated-key)
    (let ((entry `((,original-key . ,simulated-key))))
      (setq exwm-input-simulation-keys (append exwm-input-simulation-keys
                                               entry))
      (exwm-input--set-simulation-keys entry 'cache))))

(defun exwm-input--unset-simulation-keys ()
  "Clear simulation keys and key bindings defined."
  (exwm--log)
  (when (hash-table-p exwm-input--simulation-keys)
    (maphash (lambda (key _value)
               (when (sequencep key)
                 (define-key exwm-mode-map key nil)))
             exwm-input--simulation-keys)
    (clrhash exwm-input--simulation-keys)))

(defun exwm-input-set-local-simulation-keys (simulation-keys)
  "Set buffer-local simulation keys.

SIMULATION-KEYS is an alist of the form (original-key . simulated-key),
where both ORIGINAL-KEY and SIMULATED-KEY are key sequences."
  (exwm--log)
  (make-local-variable 'exwm-input--simulation-keys)
  (use-local-map (copy-keymap exwm-mode-map))
  (exwm-input--set-simulation-keys simulation-keys nil 'local))

(cl-defun exwm-input-send-simulation-key (n)
  "Fake N key events according to the last input key sequence."
  (interactive "p")
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from exwm-input-send-simulation-key))
  (let ((keys (gethash (this-single-command-keys)
                       exwm-input--simulation-keys)))
    (dotimes (_ n)
      (dolist (key keys)
        (exwm-input--fake-key key)))))

(defmacro exwm-input-invoke-factory (keys)
  "Make a command that invokes KEYS when called.

One use is to access the keymap bound to KEYS (as prefix keys) in `char-mode'."
  (let* ((keys (kbd keys))
         (description (key-description keys)))
    `(defun ,(intern (concat "exwm-input--invoke--" description)) ()
       ,(format "Invoke `%s'." description)
       (interactive)
       (mapc (lambda (key)
               (exwm-input--cache-event key t)
               (exwm-input--unread-event key))
             ',(listify-key-sequence keys)))))

(defun exwm-input--on-minibuffer-setup ()
  "Run in `minibuffer-setup-hook' to grab keyboard if necessary."
  (let* ((window (or (minibuffer-selected-window) ; minibuffer-setup-hook
                     (selected-window)))          ; echo-area-clear-hook
         (frame (window-frame window)))
    (when (exwm--terminal-p frame)
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'exwm-mode)
                   (eq exwm--selected-input-mode 'char-mode))
          (exwm--log "Grab #x%x window=%s frame=%s" exwm--id window frame)
          (exwm-input--grab-keyboard exwm--id))))))

(defun exwm-input--on-minibuffer-exit ()
  "Run in `minibuffer-exit-hook' to release keyboard if necessary."
  (let* ((window (or (minibuffer-selected-window) ; minibuffer-setup-hook
                     (selected-window)))          ; echo-area-clear-hook
         (frame (window-frame window)))
    (when (exwm--terminal-p frame)
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'exwm-mode)
                   (eq exwm--selected-input-mode 'char-mode)
                   (eq exwm--input-mode 'line-mode))
          (exwm--log "Release #x%x window=%s frame=%s" exwm--id window frame)
          (exwm-input--release-keyboard exwm--id))))))

(defun exwm-input--on-echo-area-dirty ()
  "Run when new message arrives to grab keyboard if necessary."
  (when (and cursor-in-echo-area
             (not (active-minibuffer-window)))
    (exwm--log)
    (exwm-input--on-minibuffer-setup)))

(defun exwm-input--on-echo-area-clear ()
  "Run in `echo-area-clear-hook' to release keyboard if necessary."
  (unless (current-message)
    (exwm--log)
    (exwm-input--on-minibuffer-exit)))

(defun exwm-input--call-with-passthrough (function &rest args)
  "Bind `exwm-input-line-mode-passthrough' and call FUNCTION with ARGS."
  (let ((exwm-input-line-mode-passthrough t))
    (apply function args)))

(defun exwm-input--xtest-init ()
  "Enable XTEST key injection when the server has the extension."
  (setq exwm-input--xtest nil
        exwm-input--xtest-modifiers nil)
  (if (= 0 (slot-value (xcb:get-extension-data exwm--connection 'xcb:xtest)
                       'present))
      (exwm--log "XTEST is not available; simulation keys use SendEvent")
    (let ((reply (xcb:+request-unchecked+reply exwm--connection
                     (make-instance 'xcb:xtest:GetVersion
                                    :major-version 2
                                    :minor-version 2))))
      (when reply
        (with-slots (major-version minor-version) reply
          (when (and major-version (>= major-version 2))
            (setq exwm-input--xtest t)
            (exwm--log "XTEST %s.%s" major-version minor-version)))))))

(defun exwm-input--init ()
  "Initialize the keyboard module."
  (exwm--log)
  (exwm-input--xtest-init)
  ;; Refresh keyboard mapping.  Modifier keycodes need this mapping.
  (xcb:keysyms:init exwm--connection #'exwm-input--on-keysyms-update)
  (exwm-input--xtest-refresh-modifiers)
  ;; Create the X window and intern the atom used to fetch timestamp.
  (setq exwm-input--timestamp-window (xcb:generate-id exwm--connection))
  (xcb:+request exwm--connection
      (make-instance 'xcb:CreateWindow
                     :depth 0
                     :wid exwm-input--timestamp-window
                     :parent exwm--root
                     :x -1
                     :y -1
                     :width 1
                     :height 1
                     :border-width 0
                     :class xcb:WindowClass:CopyFromParent
                     :visual 0
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:PropertyChange))
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                     :window exwm-input--timestamp-window
                     :data "EXWM: exwm-input--timestamp-window"))
  (setq exwm-input--timestamp-atom (exwm--intern-atom "_TIME"))
  ;; Initialize global keys.
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  ;; Initialize simulation keys.
  (when exwm-input-simulation-keys
    (exwm-input--set-simulation-keys exwm-input-simulation-keys))
  ;; Attach event listeners
  (xcb:+event exwm--connection 'xcb:PropertyNotify
              #'exwm-input--on-PropertyNotify)
  (xcb:+event exwm--connection 'xcb:CreateNotify #'exwm-input--on-CreateNotify)
  (xcb:+event exwm--connection 'xcb:KeyPress #'exwm-input--on-KeyPress)
  (xcb:+event exwm--connection 'xcb:ButtonPress #'exwm-input--on-ButtonPress)
  (xcb:+event exwm--connection 'xcb:ButtonRelease
              #'exwm-floating--stop-moveresize)
  (xcb:+event exwm--connection 'xcb:MotionNotify
              #'exwm-floating--do-moveresize)
  (when mouse-autoselect-window
    (xcb:+event exwm--connection 'xcb:EnterNotify
                #'exwm-input--on-EnterNotify))
  ;; Grab/Release keyboard when minibuffer/echo becomes active/inactive.
  (add-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'exwm-input--on-minibuffer-exit)
  (setq exwm-input--echo-area-timer
        (run-with-idle-timer 0 t #'exwm-input--on-echo-area-dirty))
  (add-hook 'echo-area-clear-hook #'exwm-input--on-echo-area-clear)
  ;; Update focus when buffer list updates
  (add-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  (advice-add 'mouse-autoselect-window-select :before
              #'exwm-input--note-pointer-focus)
  (add-hook 'pre-command-hook #'exwm-input--clear-pointer-focus)

  (dolist (fun exwm-input--passthrough-functions)
    (advice-add fun :around #'exwm-input--call-with-passthrough))
  ;; C-M-v from an ordinary Emacs window.  The exwm-mode-map binding
  ;; covers line-mode, where an unbound key would go to the client.
  (define-key global-map [remap scroll-other-window]
              #'exwm-input-scroll-other-window)
  (define-key global-map [remap scroll-other-window-down]
              #'exwm-input-scroll-other-window-down)
  (exwm-input--persp-setup))

(defun exwm-input--post-init ()
  "The second stage in the initialization of the input module."
  (exwm--log)
  (exwm-input--update-global-prefix-keys))

(defun exwm-input--exit ()
  "Exit the input module."
  (exwm--log)
  (exwm-input--persp-exit)
  (setq exwm-input--xtest nil
        exwm-input--xtest-modifiers nil)
  (dolist (fun exwm-input--passthrough-functions)
    (advice-remove fun #'exwm-input--call-with-passthrough))
  (exwm-input--unset-simulation-keys)
  (remove-hook 'minibuffer-setup-hook #'exwm-input--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'exwm-input--on-minibuffer-exit)
  (when exwm-input--echo-area-timer
    (cancel-timer exwm-input--echo-area-timer)
    (setq exwm-input--echo-area-timer nil))
  (remove-hook 'echo-area-clear-hook #'exwm-input--on-echo-area-clear)
  (remove-hook 'buffer-list-update-hook #'exwm-input--on-buffer-list-update)
  (advice-remove 'mouse-autoselect-window-select
                 #'exwm-input--note-pointer-focus)
  (remove-hook 'pre-command-hook #'exwm-input--clear-pointer-focus)
  (define-key global-map [remap scroll-other-window] nil)
  (define-key global-map [remap scroll-other-window-down] nil)
  (setq exwm-input--pointer-focus-until nil
        exwm-input--pointer-focus-window nil
        exwm-input--warp-until nil)
  (when exwm-input--update-focus-timer
    (cancel-timer exwm-input--update-focus-timer))
  ;; Make input focus working even without a WM.
  (when (slot-value exwm--connection 'connected)
    (xcb:+request exwm--connection
        (make-instance 'xcb:SetInputFocus
                       :revert-to xcb:InputFocus:PointerRoot
                       :focus exwm--root
                       :time xcb:Time:CurrentTime))
    (xcb:flush exwm--connection)))

(provide 'exwm-input)
;;; exwm-input.el ends here
