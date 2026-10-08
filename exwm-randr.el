;;; exwm-randr.el --- RandR Module for EXWM  -*- lexical-binding: t -*-

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

;; This module adds RandR support for EXWM.  Currently it requires external
;; tools such as xrandr(1) to properly configure RandR first.  This
;; dependency may be removed in the future, but more work is needed before
;; that.

;; To use this module, load and enable it.  Workspaces are placed on
;; active monitors with no plist: the primary monitor first, then the
;; others from left to right and top to bottom.  A monitor that is
;; plugged in receives a workspace: a new one if every workspace already
;; sits alone on a monitor, otherwise one of the extras.  Unplugging
;; remembers the monitor so replugging restores that workspace.
;; `exwm-randr-workspace-monitor-plist' still overrides that for the
;; indexes it names.  `exwm-randr-screen-change-hook' is where a
;; user script configures outputs with xrandr(1), for example:
;;
;;   (setq exwm-randr-workspace-monitor-plist '(0 "VGA1"))
;;   (add-hook 'exwm-randr-screen-change-hook
;;             (lambda ()
;;               (start-process-shell-command
;;                "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;;   (exwm-randr-mode 1)
;;
;; With above lines, workspace 0 should be assigned to the output named "VGA1",
;; staying at the left of other workspaces on the output "LVDS1".  Please refer
;; to xrandr(1) for the configuration of RandR.

;; References:
;; + RandR (https://www.x.org/archive/X11R7.7/doc/randrproto/randrproto.txt)

;;; Code:

(require 'xcb-randr)

(require 'exwm-core)
(require 'exwm-workspace)

(declare-function x-get-atom-name "C source code" (VALUE &optional FRAME))

(defgroup exwm-randr nil
  "RandR."
  :group 'exwm)

(defvar exwm-randr--connection nil "The X connection.")

(defcustom exwm-randr-refresh-hook nil
  "Normal hook run when the RandR module just refreshed."
  :type 'hook)

(defcustom exwm-randr-screen-change-hook nil
  "Normal hook run when screen changes."
  :type 'hook)

(defcustom exwm-randr-auto-assign t
  "Assign workspaces to monitors when the plist does not name them.

The primary monitor is first.  The other active monitors follow,
left to right and top to bottom.  Mirrored outputs count as one
monitor.  Workspace 0 is the primary monitor, the next workspaces
take the remaining monitors, and any workspace beyond that stays on
the primary monitor.  A name in `exwm-randr-workspace-monitor-plist'
wins for that workspace.

When there are more monitors than workspaces, workspaces are added
so each monitor has one.  Unplugging a monitor moves the workspaces
that named it onto the primary monitor, remembering that monitor so
replugging restores them.  A still-empty monitor takes a workspace
that is stacked on another, or a new workspace.  The windows stay
on those workspaces.  With `exwm-workspace-strip' non-nil, a
workspace that already names a connected monitor stays there instead
of following its index, unless it is the extra copy used to fill an
empty monitor.  The plist still wins, and strip order is kept.

Set this to nil to keep every unnamed workspace on the primary
monitor, which was the behavior before this option existed."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when exwm-randr--connection
           (exwm-randr-refresh))))

(defvar exwm-randr--adding nil
  "Non-nil while auto-assign is creating a workspace.")

(defcustom exwm-randr-workspace-monitor-plist nil
  "Plist mapping workspaces to monitors.

In RandR 1.5 a monitor is a rectangle region decoupled from the physical
size of screens, and can be identified with `xrandr --listmonitors' (name of
the primary monitor is prefixed with an `*').  When no monitor is created it
automatically fallback to RandR 1.2 output which represents the physical
screen size.  RandR 1.5 monitors can be created with `xrandr --setmonitor'.
For example, to split an output (`LVDS-1') of size 1280x800 into two
side-by-side monitors one could invoke (the digits after `/' are size in mm)

    xrandr --setmonitor *LVDS-1-L 640/135x800/163+0+0 LVDS-1
    xrandr --setmonitor LVDS-1-R 640/135x800/163+640+0 none

If a monitor is not active, the workspaces mapped to it are displayed on the
primary monitor until it becomes active (if ever).  With
`exwm-randr-auto-assign' non-nil, a workspace this plist does not name is
placed by that option.  With `exwm-randr-auto-assign' nil, an unnamed
workspace is displayed on the primary monitor.  For example, with the
following setting and auto-assign turned off, workspaces other than 1 and 3
would always be displayed on the primary monitor, while workspaces 1 and 3
would be displayed on their corresponding monitors whenever those monitors
are active.

Changes to this variable only take immediate affect when set before
`exwm-randr-mode' is enabled, via `setopt', or when customized (see the
Info node `Customization'). Otherwise, the `exwm-randr-refresh' must be
called explicitly to assign the correct workspaces to the correct monitors.

  \\='(1 \"HDMI-1\" 3 \"DP-1\")"
  :type '(plist :key-type integer :value-type string)
  :initialize 'custom-initialize-changed
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when exwm-randr--connection
           (exwm-randr-refresh))))

(defvar exwm-randr--connection nil "The X connection.")

(defvar exwm-randr--last-timestamp 0 "Used for debouncing events.")

(defvar exwm-randr--prev-screen-change-timestamp 0
  "The most recent ScreenChangeNotify config change timestamp.")

;;;###autoload
(define-minor-mode exwm-randr-mode
  "Toggle EXWM randr support."
  :global t
  :group 'exwm-randr
  (exwm--global-minor-mode-body randr))

(defsubst exwm-randr--assert-connected ()
  "Assert that `exwm-randr-mode' is enabled and activated."
  (cond
   ((not exwm-randr-mode) (user-error "EXWM RandR mode not enabled"))
   ((not exwm-randr--connection) (user-error "EXWM RandR not connected, is EXWM running?"))))

(defun exwm-randr--get-monitors ()
  "Get RandR 1.5 monitors."
  (exwm--log)
  (let (monitor-name geometry monitor-geometry-alist primary-monitor)
    (with-slots (timestamp monitors)
        (xcb:+request-unchecked+reply exwm-randr--connection
            (make-instance 'xcb:randr:GetMonitors
                           :window exwm--root
                           :get-active 1))
      (when (> timestamp exwm-randr--last-timestamp)
        (setq exwm-randr--last-timestamp timestamp))
      (dolist (monitor monitors)
        (with-slots (name primary x y width height) monitor
          (setq monitor-name (x-get-atom-name name)
                geometry (make-instance 'xcb:RECTANGLE
                                        :x x
                                        :y y
                                        :width width
                                        :height height)
                monitor-geometry-alist (cons (cons monitor-name geometry)
                                             monitor-geometry-alist))
          (exwm--log "%s: %sx%s+%s+%s" monitor-name x y width height)
          ;; Save primary monitor when available (fallback to the first one).
          (when (or (/= 0 primary)
                    (not primary-monitor))
            (setq primary-monitor monitor-name)))))
    (exwm--log "Primary monitor: %s" primary-monitor)
    (list primary-monitor monitor-geometry-alist
          (exwm-randr--get-monitor-alias primary-monitor
                                         monitor-geometry-alist))))

(defun exwm-randr--get-monitor-alias (primary-monitor monitor-geometry-alist)
  "Generate monitor aliases using PRIMARY-MONITOR MONITOR-GEOMETRY-ALIST.

In a mirroring setup some monitors overlap and should be treated as one."
  (let (monitor-position-alist monitor-alias-alist monitor-name geometry)
    (setq monitor-position-alist (with-slots (x y)
                                     (cdr (assoc primary-monitor
                                                 monitor-geometry-alist))
                                   (list (cons primary-monitor (vector x y)))))
    (setq monitor-alias-alist (list (cons primary-monitor primary-monitor)))
    (dolist (pair monitor-geometry-alist)
      (setq monitor-name (car pair)
            geometry (cdr pair))
      (unless (assoc monitor-name monitor-alias-alist)
        (let* ((position (vector (slot-value geometry 'x)
                                 (slot-value geometry 'y)))
               (alias (car (rassoc position monitor-position-alist))))
          (if alias
              (setq monitor-alias-alist (cons (cons monitor-name alias)
                                              monitor-alias-alist))
            (setq monitor-position-alist (cons (cons monitor-name position)
                                               monitor-position-alist)
                  monitor-alias-alist (cons (cons monitor-name monitor-name)
                                            monitor-alias-alist))))))
    monitor-alias-alist))

(defun exwm-randr--monitor-order (primary monitor-geometry-alist
                                    monitor-alias-alist)
  "Return (NAME . GEOMETRY) for each distinct monitor.
PRIMARY is first.  The rest are ordered by Y, then X.  Mirrored
outputs share an alias and appear once."
  (let (unique)
    (dolist (pair monitor-geometry-alist)
      (let* ((alias (cdr (assoc (car pair) monitor-alias-alist)))
             (geometry (cdr (assoc alias monitor-geometry-alist))))
        (when (and alias geometry (not (assoc alias unique)))
          (push (cons alias geometry) unique))))
    (setq unique
          (sort unique
                (lambda (a b)
                  (let ((ga (cdr a))
                        (gb (cdr b)))
                    (or (< (slot-value ga 'y) (slot-value gb 'y))
                        (and (= (slot-value ga 'y) (slot-value gb 'y))
                             (< (slot-value ga 'x) (slot-value gb 'x))))))))
    (if-let* ((hit (assoc primary unique)))
        (cons hit (delq hit unique))
      unique)))

(defun exwm-randr--choose-monitors (primary order geometry-alist old-monitors)
  "Return a monitor name for each workspace.
PRIMARY is first in ORDER.  A plist entry wins.  A workspace that
was moved off a monitor that has come back is restored there.
`exwm-workspace-strip' keeps a workspace on a still-connected
monitor.  Every name in ORDER is used at least once: a duplicate
workspace is moved onto an empty monitor, or a workspace is added."
  (let* ((n (exwm-workspace--count))
         (chosen (make-vector n nil))
         (count (make-hash-table :test #'equal)))
    (dotimes (i n)
      (let* ((frame (elt exwm-workspace--list i))
             (configured (plist-get exwm-randr-workspace-monitor-plist i))
             (existing (frame-parameter frame 'exwm-randr-monitor))
             (last (frame-parameter frame 'exwm-randr-last-monitor))
             (monitor
              (cond
               ((and configured (assoc configured geometry-alist))
                configured)
               ((and last
                     (assoc last geometry-alist)
                     (zerop (gethash last count 0)))
                last)
               ((and exwm-workspace-strip
                     (assq frame old-monitors)
                     (stringp existing)
                     (assoc existing geometry-alist))
                existing))))
        (aset chosen i monitor)
        (when monitor
          (puthash monitor (1+ (gethash monitor count 0)) count))))
    (dotimes (i n)
      (unless (aref chosen i)
        (let ((monitor nil))
          (dolist (name order)
            (when (and (not monitor) (zerop (gethash name count 0)))
              (setq monitor name)))
          (setq monitor (or monitor
                            (and exwm-randr-auto-assign
                                 (if (< i (length order))
                                     (nth i order)
                                   primary))
                            primary))
          (aset chosen i monitor)
          (puthash monitor (1+ (gethash monitor count 0)) count))))
    (when exwm-randr-auto-assign
      (dolist (name order)
        (when (zerop (gethash name count 0))
          (let ((donor nil))
            (dotimes (i n)
              (let ((current (aref chosen i)))
                (when (and (not (plist-get exwm-randr-workspace-monitor-plist i))
                           current
                           (> (gethash current count 0) 1))
                  (setq donor i))))
            (if donor
                (let ((old (aref chosen donor)))
                  (puthash old (1- (gethash old count)) count)
                  (aset chosen donor name)
                  (puthash name 1 count))
              (unless exwm-randr--adding
                (let ((exwm-randr--adding t))
                  (exwm-workspace-add)
                  (setq chosen (vconcat chosen (vector name))
                        n (1+ n))
                  (puthash name 1 count))))))))
    (append chosen nil)))

(defun exwm-randr-refresh ()
  "Refresh workspaces according to the updated RandR info."
  (interactive)
  (exwm--log)
  (exwm-randr--assert-connected)
  (let* ((result (exwm-randr--get-monitors))
         (primary-monitor (elt result 0))
         (monitor-geometry-alist (elt result 1))
         (monitor-alias-alist (elt result 2))
         (order (mapcar #'car
                        (exwm-randr--monitor-order primary-monitor
                                                   monitor-geometry-alist
                                                   monitor-alias-alist)))
         container-monitor-alist container-frame-alist)
    (when (and primary-monitor monitor-geometry-alist)
      (let ((old-monitors
             (mapcar (lambda (frame)
                       (cons frame
                             (frame-parameter frame 'exwm-randr-monitor)))
                     exwm-workspace--list))
            (chosen nil))
      (setq chosen
            (exwm-randr--choose-monitors primary-monitor order
                                         monitor-geometry-alist
                                         old-monitors))
      (when exwm-workspace--fullscreen-frame-count
        ;; Not all workspaces are fullscreen; reset this counter.
        (setq exwm-workspace--fullscreen-frame-count 0))
      (dotimes (i (exwm-workspace--count))
        (let* ((frame (elt exwm-workspace--list i))
               (existing (frame-parameter frame 'exwm-randr-monitor))
               (monitor (or (nth i chosen) primary-monitor))
               (geometry (cdr (assoc monitor monitor-geometry-alist)))
               (container (frame-parameter frame 'exwm-container)))
          (if geometry
              ;; Unify monitor names in case it's a mirroring setup.
              (setq monitor (cdr (assoc monitor monitor-alias-alist)))
            ;; Missing monitors fallback to the primary one.
            (when (and (stringp existing)
                       (not (equal existing primary-monitor)))
              (set-frame-parameter frame 'exwm-randr-last-monitor existing))
            (setq monitor primary-monitor
                  geometry (cdr (assoc primary-monitor
                                       monitor-geometry-alist))))
          (when (and (stringp monitor)
                     (equal monitor
                            (frame-parameter frame 'exwm-randr-last-monitor)))
            (set-frame-parameter frame 'exwm-randr-last-monitor nil))
          (setq container-monitor-alist (nconc
                                         `((,container . ,(intern monitor)))
                                         container-monitor-alist)
                container-frame-alist (nconc `((,container . ,frame))
                                             container-frame-alist))
          (set-frame-parameter frame 'exwm-randr-monitor monitor)
          (set-frame-parameter frame 'exwm-geometry geometry)))
      ;; Update workareas.
      (exwm-workspace--update-workareas)
      ;; Resize workspace.
      (dolist (f exwm-workspace--list)
        (exwm-workspace--set-fullscreen f))
      (xcb:flush exwm-randr--connection)
      ;; Raise the minibuffer if it's active.
      (when (and (active-minibuffer-window)
                 (exwm-workspace--minibuffer-own-frame-p))
        (exwm-workspace--show-minibuffer))
      ;; Set _NET_DESKTOP_GEOMETRY.
      (exwm-workspace--set-desktop-geometry)
      ;; Update active/inactive workspaces.
      (dolist (w exwm-workspace--list)
        (exwm-workspace--set-active w nil))
      ;; Mark the workspace on the top of each monitor as active.
      (dolist (xwin
               (reverse
                (slot-value (xcb:+request-unchecked+reply exwm-randr--connection
                                (make-instance 'xcb:QueryTree
                                               :window exwm--root))
                            'children)))
        (let ((monitor (cdr (assq xwin container-monitor-alist))))
          (when monitor
            (setq container-monitor-alist
                  (rassq-delete-all monitor container-monitor-alist))
            (exwm-workspace--set-active (cdr (assq xwin container-frame-alist))
                                        t))))
      (xcb:flush exwm-randr--connection)
      (when exwm-workspace-strip
        (exwm-workspace--strip-note-orders
         (exwm-workspace--strip-rebuild
          (mapcar (lambda (frame)
                    (list frame
                          (frame-parameter frame 'exwm-randr-monitor)
                          (cdr (assq frame old-monitors))
                          (frame-parameter frame
                                           'exwm-workspace-strip-order)))
                  exwm-workspace--list))))
      (run-hooks 'exwm-randr-refresh-hook)))))

(defun exwm-randr--on-ScreenChangeNotify (data _synthetic)
  "Handle `ScreenChangeNotify' event with DATA.

Run `exwm-randr-screen-change-hook' (usually user scripts to configure RandR)."
  (exwm--log)
  (let* ((evt (xcb:unmarshal-new 'xcb:randr:ScreenChangeNotify data))
         (ts (slot-value evt 'config-timestamp)))
    (unless (equal ts exwm-randr--prev-screen-change-timestamp)
      (setq exwm-randr--prev-screen-change-timestamp ts)
      (run-hooks 'exwm-randr-screen-change-hook))))

(defun exwm-randr--on-Notify (data _synthetic)
  "Handle `CrtcChangeNotify' and `OutputChangeNotify' events with DATA.

Refresh when any CRTC/output changes."
  (exwm--log)
  (with-slots (subCode u)
      (xcb:unmarshal-new 'xcb:randr:Notify data)
    (when-let* ((notify
                 (with-slots (cc oc) u
                   (cond
                    ((= subCode xcb:randr:Notify:CrtcChange) cc)
                    ((= subCode xcb:randr:Notify:OutputChange) oc)))))
      (with-slots (timestamp) notify
        (when (> timestamp exwm-randr--last-timestamp)
          (exwm-randr-refresh)
          (setq exwm-randr--last-timestamp timestamp))))))

(defun exwm-randr--on-ConfigureNotify (data _synthetic)
  "Handle `ConfigureNotify' event with DATA.

Refresh when any RandR 1.5 monitor changes."
  (exwm--log)
  (with-slots (window) (xcb:unmarshal-new 'xcb:ConfigureNotify data)
    (when (eq window exwm--root)
      (exwm-randr-refresh))))

(cl-defun exwm-randr--init ()
  "Initialize RandR extension and EXWM RandR module."
  (exwm--log)
  (when exwm-randr--connection
    (cl-return-from exwm-randr--init))
  (setq exwm-randr--connection (xcb:connect))
  (set-process-query-on-exit-flag (slot-value exwm-randr--connection 'process) nil)
  (when (= 0 (slot-value (xcb:get-extension-data exwm-randr--connection 'xcb:randr)
                         'present))
    (xcb:disconnect exwm-randr--connection)
    (setq exwm-randr--connection nil)
    (error "[EXWM] RandR extension is not supported by the server"))
  (with-slots (major-version minor-version)
      (xcb:+request-unchecked+reply exwm-randr--connection
          (make-instance 'xcb:randr:QueryVersion
                         :major-version 1 :minor-version 5))
    (unless (and (= major-version 1) (>= minor-version 5))
      (xcb:disconnect exwm-randr--connection)
      (setq exwm-randr--connection nil)
      (error "[EXWM] The server only support RandR version up to %d.%d"
             major-version minor-version))
    ;; External monitor(s) may already be connected.
    (run-hooks 'exwm-randr-screen-change-hook)
    (exwm-randr-refresh)
    ;; Listen for `ScreenChangeNotify' to notify external tools to
    ;; configure RandR and `CrtcChangeNotify/OutputChangeNotify' to
    ;; refresh the workspace layout.
    (xcb:+event exwm-randr--connection 'xcb:randr:ScreenChangeNotify
                #'exwm-randr--on-ScreenChangeNotify)
    (xcb:+event exwm-randr--connection 'xcb:randr:Notify
                #'exwm-randr--on-Notify)
    (xcb:+event exwm-randr--connection 'xcb:ConfigureNotify
                #'exwm-randr--on-ConfigureNotify)
    (xcb:+request exwm-randr--connection
        (make-instance 'xcb:randr:SelectInput
                       :window exwm--root
                       :enable (logior
                                xcb:randr:NotifyMask:ScreenChange
                                xcb:randr:NotifyMask:CrtcChange
                                xcb:randr:NotifyMask:OutputChange)))
    (xcb:flush exwm-randr--connection)
    (add-hook 'exwm-workspace-list-change-hook #'exwm-randr-refresh))
  ;; Prevent frame parameters introduced by this module from being
  ;; saved/restored.
  (dolist (i '(exwm-randr-monitor))
    (unless (assq i frameset-filter-alist)
      (push (cons i :never) frameset-filter-alist))))

(defun exwm-randr--exit ()
  "Exit the RandR module."
  (exwm--log)
  (remove-hook 'exwm-workspace-list-change-hook #'exwm-randr-refresh)
  (when exwm-randr--connection
    (xcb:disconnect exwm-randr--connection)
    (setq exwm-randr--connection nil)))

(provide 'exwm-randr)
;;; exwm-randr.el ends here
