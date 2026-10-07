;;; exwm-clipboard.el --- CLIPBOARD snapshot for EXWM  -*- lexical-binding: t -*-

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

;; A copy inside an X client lives only as long as that client owns
;; CLIPBOARD.  This module listens for XFixes selection-owner changes
;; and pushes the text onto the kill ring, so it can still be yanked
;; after the client exits.  `kill-new' leaves Emacs as the owner
;; through `interprogram-cut-function'.  The primary selection is not
;; watched.

;;; Code:

(require 'exwm-core)
(require 'xcb-xfixes)

(defconst exwm-clipboard--timeout 500
  "Milliseconds to wait for a CLIPBOARD reply.
`x-selection-timeout' is 0 by default, which waits forever.  A client
that exits without answering must not freeze the window manager.")

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

(defcustom exwm-clipboard t
  "Non-nil to copy CLIPBOARD text onto the kill ring.

A client copy is read when its owner changes and pushed with
`kill-new', so the text remains after that client exits.  Emacs
becomes the selection owner through `interprogram-cut-function'.
An echo of that same text, including one from another clipboard
manager, is not pushed again.  The primary selection is left to
`select-enable-primary'.

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
this snapshot, or another clipboard manager that takes ownership of
it, does not loop."
  (and (not exwm-clipboard--suppress)
       (not owner-p)
       (stringp text)
       (not (string-empty-p text))
       (not (and (stringp kill-head) (string= text kill-head)))))

(defun exwm-clipboard--text ()
  "Return CLIPBOARD text, or nil when the selection is not text."
  (let* ((types (if (and (boundp 'x-select-request-type)
                         x-select-request-type)
                    x-select-request-type
                  '(UTF8_STRING COMPOUND_TEXT STRING
                                text/plain\;charset=utf-8)))
         (types (if (consp types) types (list types)))
         text)
    (while (and types (not (and (stringp text) (not (string-empty-p text)))))
      (condition-case err
          (setq text (gui-get-selection 'CLIPBOARD (car types)))
        (error
         (exwm--log "clipboard: %s" err)
         (setq text nil)))
      (setq types (cdr types)))
    (and (stringp text) (not (string-empty-p text)) text)))

(defun exwm-clipboard--snapshot ()
  "Push the current CLIPBOARD text onto the kill ring."
  (when (and exwm-clipboard--listening
             exwm--connection
             (not exwm-clipboard--suppress))
    (let* ((x-selection-timeout exwm-clipboard--timeout)
           (owner-p (and (fboundp 'gui-backend-selection-owner-p)
                         (gui-backend-selection-owner-p 'CLIPBOARD))))
      (unless owner-p
        (let ((text (exwm-clipboard--text)))
          (when (exwm-clipboard--accept-p text nil (car kill-ring))
            (let ((exwm-clipboard--suppress t))
              (kill-new text))))))))

(defun exwm-clipboard--on-notify (data _synthetic)
  "Snapshot CLIPBOARD after an XFixes SelectionNotify in DATA."
  (when (and exwm-clipboard--listening exwm-clipboard--atom)
    (let ((event (xcb:unmarshal-new 'xcb:xfixes:SelectionNotify data)))
      (with-slots (selection owner) event
        (when (and (= selection exwm-clipboard--atom)
                   (not (zerop owner)))
          (exwm-clipboard--snapshot))))))

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
          (setq exwm-clipboard--listening t))
      (error
       (exwm--log "clipboard init: %S" err)
       (setq exwm-clipboard--listening nil)))))

(defun exwm-clipboard--exit ()
  "Stop listening for CLIPBOARD owner changes."
  (when (and exwm--connection exwm-clipboard--atom exwm-clipboard--listening)
    (condition-case err
        (exwm-clipboard--select 0)
      (error (exwm--log "clipboard exit: %S" err))))
  (setq exwm-clipboard--listening nil))

(defun exwm-clipboard--reset ()
  "Forget listener state.  Call this when the X connection is gone."
  (setq exwm-clipboard--listening nil
        exwm-clipboard--registered nil
        exwm-clipboard--atom nil
        exwm-clipboard--suppress nil))

(provide 'exwm-clipboard)

;;; exwm-clipboard.el ends here
