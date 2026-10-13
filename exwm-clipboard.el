;;; exwm-clipboard.el --- Emacs owns CLIPBOARD in EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: TJ <william@theesfeld.net>

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

;; Emacs is the CLIPBOARD owner.  `M-w' copies onto the kill ring and
;; `C-y' yanks from it.  Line-mode simulation keys send those to X
;; clients as copy and paste.  When a client copies, this module reads
;; CLIPBOARD and `kill-new's the text so Emacs owns it again.  The
;; primary selection is not watched.

;; `x-selection-owner-p' is Emacs's local alist, not the server.  A
;; client can own CLIPBOARD while that flag is still t.  Then
;; `gui-get-selection' returns the old kill, and a snapshot that
;; trusted the flag would skip.  The server owner from
;; `GetSelectionOwner' is what counts.  A stale local owner is
;; dropped with `x-disown-selection-internal' (ignored by the server
;; when Emacs is not the owner) before the read.

;;; Code:

(require 'exwm-core)
(require 'xcb-xfixes)

(declare-function x-disown-selection-internal "xselect.c"
                  (selection &optional time-object terminal))

(defconst exwm-clipboard--timeout 500
  "Milliseconds to wait for a CLIPBOARD reply.
`x-selection-timeout' is 0 by default, which waits forever.  A client
that exits without answering must not freeze the window manager.")

(defconst exwm-clipboard--settle 0.05
  "Seconds to wait after an owner change before reading CLIPBOARD.
The client needs a moment to finish offering the selection.")

(defvar exwm-clipboard--atom nil
  "X atom for CLIPBOARD, or nil before initialization.")

(defvar exwm-clipboard--listening nil
  "Non-nil while CLIPBOARD owner changes are being snapshotted.")

(defvar exwm-clipboard--registered nil
  "Non-nil after the XFixes listener has been attached.")

(defvar exwm-clipboard--suppress nil
  "Non-nil while this module is itself pushing the kill ring.
The XFixes event caused by that push is an echo and must not be
copied again.")

(defvar exwm-clipboard--timer nil
  "Pending timer to take CLIPBOARD, or nil.")

(defvar exwm-clipboard--event-time nil
  "X server time of the last CLIPBOARD owner change, or nil.")

(defcustom exwm-clipboard t
  "Non-nil to keep Emacs as the CLIPBOARD owner.

A client copy is read when its owner changes and pushed with
`kill-new', so Emacs owns CLIPBOARD again and the text remains
after that client exits.  An echo of that same text is not pushed
again.  The primary selection is left to `select-enable-primary'.

Set this to nil to leave CLIPBOARD with the client that owns it."
  :type 'boolean
  :group 'exwm
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when (and (boundp 'exwm--connection) exwm--connection)
           (if value
               (exwm-clipboard--init)
             (exwm-clipboard--exit)))))

(defun exwm-clipboard--accept-p (text owner-p kill-head)
  "Return non-nil when TEXT should be pushed onto the kill ring.
OWNER-P means Emacs already owns CLIPBOARD.  KILL-HEAD is the
current kill-ring head, or nil.  The same text is refused so that
this snapshot does not loop."
  (and (not exwm-clipboard--suppress)
       (not owner-p)
       (stringp text)
       (not (string-empty-p text))
       (not (and (stringp kill-head) (string= text kill-head)))))

(defun exwm-clipboard--request-types ()
  "Return CLIPBOARD targets to try, in order."
  (let ((types (if (and (boundp 'x-select-request-type)
                        x-select-request-type)
                   x-select-request-type
                 '(UTF8_STRING COMPOUND_TEXT STRING
                               text/plain\;charset=utf-8 TEXT
                               text/plain))))
    (if (consp types) types (list types))))

(defun exwm-clipboard--text ()
  "Return CLIPBOARD text, or nil when the selection is not text."
  (let ((types (exwm-clipboard--request-types))
        text)
    (while (and types (not (and (stringp text) (not (string-empty-p text)))))
      (condition-case err
          (setq text (gui-get-selection 'CLIPBOARD (car types)))
        (error
         (exwm--log "clipboard: %s" err)
         (setq text nil)))
      (setq types (cdr types)))
    (and (stringp text) (not (string-empty-p text)) text)))

(defun exwm-clipboard--owner ()
  "Return the X window that owns CLIPBOARD, or 0."
  (if (and exwm--connection exwm-clipboard--atom)
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:GetSelectionOwner
                                      :selection exwm-clipboard--atom))))
        (or (and reply (slot-value reply 'owner)) 0))
    0))

(defun exwm-clipboard--xid (value)
  "Return VALUE as an X window id, or nil."
  (cond ((and (numberp value) (not (zerop value))) value)
        ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value)
              (let ((n (string-to-number value)))
                (and (not (zerop n)) n))))))

(defun exwm-clipboard--emacs-xid-p (xid)
  "Return non-nil if XID is an Emacs frame window on this display."
  (let ((id (exwm-clipboard--xid xid)))
    (and id
         (catch 'found
           (dolist (frame (frame-list))
             (dolist (param '(window-id outer-window-id))
               (let ((frame-id (exwm-clipboard--xid
                                (frame-parameter frame param))))
                 (when (and frame-id (= id frame-id))
                   (throw 'found t)))))))))

(defun exwm-clipboard--drop-stale-owner ()
  "Forget a local CLIPBOARD owner flag that the server does not agree with.
`x-disown-selection-internal' is ignored by the server when Emacs
is not the owner, so a client's CLIPBOARD is left intact."
  (when (and (fboundp 'gui-backend-selection-owner-p)
             (gui-backend-selection-owner-p 'CLIPBOARD)
             (fboundp 'x-disown-selection-internal))
    (condition-case err
        (x-disown-selection-internal 'CLIPBOARD)
      (error (exwm--log "clipboard disown: %S" err)))))

(defun exwm-clipboard--set-user-time (time)
  "Use TIME as the X last-user-time so `SetSelectionOwner' is accepted.
Emacs otherwise stamps the request with an older user event, and
the server ignores it when a client has copied more recently."
  (when (and time (fboundp 'x-display-set-last-user-time))
    (x-display-set-last-user-time time)))

(defun exwm-clipboard--claim (text)
  "Make Emacs the CLIPBOARD owner of TEXT without a new kill-ring entry."
  (when (and (stringp text) (not (string-empty-p text)))
    (exwm-clipboard--set-user-time exwm-clipboard--event-time)
    (let ((exwm-clipboard--suppress t)
          (select-enable-clipboard t)
          (select-enable-primary nil))
      (gui-select-text text))))

(defun exwm-clipboard--steal ()
  "Read a client's CLIPBOARD and make Emacs the owner of that text."
  (exwm-clipboard--drop-stale-owner)
  (let* ((x-selection-timeout exwm-clipboard--timeout)
         (text (exwm-clipboard--text))
         (kill-head (car kill-ring)))
    (exwm-clipboard--set-user-time exwm-clipboard--event-time)
    (cond
     ((exwm-clipboard--accept-p text nil kill-head)
      (let ((exwm-clipboard--suppress t))
        (kill-new text)))
     ((and (stringp text) (not (string-empty-p text)))
      (exwm-clipboard--claim text)))))

(defun exwm-clipboard--take ()
  "Make Emacs the CLIPBOARD owner of the current text, if any."
  (setq exwm-clipboard--timer nil)
  (when (and exwm-clipboard--listening
             exwm--connection
             (not exwm-clipboard--suppress))
    (let ((owner (exwm-clipboard--owner)))
      (cond
       ((exwm-clipboard--emacs-xid-p owner))
       ((zerop owner)
        (exwm-clipboard--claim (car kill-ring)))
       (t
        (exwm-clipboard--steal))))))

(defun exwm-clipboard--schedule (&optional time)
  "Read CLIPBOARD after `exwm-clipboard--settle' seconds.
TIME is the X server time of the owner change, when known."
  (when time
    (setq exwm-clipboard--event-time time))
  (when (timerp exwm-clipboard--timer)
    (cancel-timer exwm-clipboard--timer))
  (setq exwm-clipboard--timer
        (run-at-time exwm-clipboard--settle nil #'exwm-clipboard--take)))

(defun exwm-clipboard--snapshot ()
  "Push the current CLIPBOARD text onto the kill ring.
Called from XFixes.  Defers so the client can finish offering the
selection, then makes Emacs the owner."
  (when (and exwm-clipboard--listening
             exwm--connection
             (not exwm-clipboard--suppress))
    (exwm-clipboard--schedule)))

(defun exwm-clipboard--on-notify (data _synthetic)
  "Snapshot CLIPBOARD after an XFixes SelectionNotify in DATA."
  (when (and exwm-clipboard--listening exwm-clipboard--atom
             (not exwm-clipboard--suppress))
    (let ((event (xcb:unmarshal-new 'xcb:xfixes:SelectionNotify data)))
      (with-slots (selection timestamp) event
        (when (= selection exwm-clipboard--atom)
          (exwm-clipboard--schedule timestamp))))))

(defun exwm-clipboard--select (mask)
  "Ask the server to deliver CLIPBOARD owner events for MASK."
  (xcb:+request exwm--connection
      (make-instance 'xcb:xfixes:SelectSelectionInput
                     :window exwm--root
                     :selection exwm-clipboard--atom
                     :event-mask mask))
  (xcb:flush exwm--connection))

(defun exwm-clipboard--init ()
  "Listen for CLIPBOARD owner changes."
  (when (and exwm--connection
             exwm-clipboard
             (not exwm-clipboard--listening))
    (condition-case err
        (progn
          (unless exwm-clipboard--registered
            (let ((data (xcb:get-extension-data exwm--connection 'xcb:xfixes)))
              (when (= 0 (slot-value data 'present))
                (warn "[EXWM] XFixes is unavailable; clipboard text will not be saved")
                (error "XFixes missing")))
            (let ((version (xcb:+request-unchecked+reply exwm--connection
                                (make-instance 'xcb:xfixes:QueryVersion
                                               :client-major-version 2
                                               :client-minor-version 0))))
              (when (or (null version)
                        (< (slot-value version 'major-version) 2))
                (warn "[EXWM] XFixes is too old; clipboard text will not be saved")
                (error "XFixes too old")))
            (setq exwm-clipboard--atom (exwm--intern-atom "CLIPBOARD"))
            (xcb:+event exwm--connection 'xcb:xfixes:SelectionNotify
                        #'exwm-clipboard--on-notify)
            (setq exwm-clipboard--registered t))
          (exwm-clipboard--select
           (logior xcb:xfixes:SelectionEventMask:SetSelectionOwner
                   xcb:xfixes:SelectionEventMask:SelectionWindowDestroy
                   xcb:xfixes:SelectionEventMask:SelectionClientClose))
          (setq exwm-clipboard--listening t)
          (exwm-clipboard--schedule))
      (error
       (exwm--log "clipboard init: %S" err)
       (setq exwm-clipboard--listening nil)))))

(defun exwm-clipboard--exit ()
  "Stop listening for CLIPBOARD owner changes."
  (when (timerp exwm-clipboard--timer)
    (cancel-timer exwm-clipboard--timer)
    (setq exwm-clipboard--timer nil))
  (when (and exwm--connection exwm-clipboard--atom exwm-clipboard--listening)
    (condition-case err
        (exwm-clipboard--select 0)
      (error (exwm--log "clipboard exit: %S" err))))
  (setq exwm-clipboard--listening nil))

(defun exwm-clipboard--reset ()
  "Forget listener state.  Call this when the X connection is gone."
  (when (timerp exwm-clipboard--timer)
    (cancel-timer exwm-clipboard--timer))
  (setq exwm-clipboard--listening nil
        exwm-clipboard--registered nil
        exwm-clipboard--atom nil
        exwm-clipboard--suppress nil
        exwm-clipboard--timer nil
        exwm-clipboard--event-time nil))

(provide 'exwm-clipboard)

;;; exwm-clipboard.el ends here
