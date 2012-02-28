;;; dbus-kit.el --- DBus helper functions

;; Copyright (C) 2010  Dmitry Dzhus

;; Author: Dmitry Dzhus <dima@sphinx.net.ru>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the unfinished attempt to write media mounter/unmounter
;; Emacs interface to udisks.

;;; Code:

(require 'dbus)

(eval-when-compile
  (require 'cl))

;; Get a list of known system services
(dbus-list-known-names :system)

;; List objects
(dbus-introspect-get-method-names
 :system 
 "org.freedesktop.UDisks" 
 "/org/freedesktop/UDisks"
 "org.freedesktop.UDisks")

;; Learn about methods
(dbus-introspect :system "org.freedesktop.PolicyKit1" "/org/freedesktop/PolicyKit1/Authority")
(dbus-introspect :system "org.freedesktop.UDisks" "/org/freedesktop/UDisks")
(dbus-get-property :system "org.freedesktop.UDisks" "/org/freedesktop/UDisks"
                   "org.freedesktop.UDisks" "KnownFilesystems")

(defvar dbk/udisks-drives-buffer-name "*UDisks drive*")

(defun dbk/list-polkit-actions ()
  (dbus-call-method :system
                    "org.freedesktop.PolicyKit1"  
                    "/org/freedesktop/PolicyKit1/Authority"
                    "org.freedesktop.PolicyKit1.Authority" 
                    "EnumerateActions"
                    ""))

;; We need this because process-attributes returns absolute time
(defun dbk/get-emacs-start ()
  "Get Emacs process start time in jiffies since the system boot.

Available on GNU/Linux only."
  (if (not (eq system-type 'gnu/linux))
      (error "Available on GNU/Linux only.")
    (with-current-buffer
        (find-file-noselect (format "/proc/%d/stat" (emacs-pid)))
      (goto-char (point-min))
      ;; Start time is 22nd field (see proc(5))
      (dotimes (n 22)
        (right-word))
      (number-at-point))))

(defun dbk/check-polkit-authorization (action)
  "Check is current Emacs process is authorized to do ACTION.

Return '(IS-AUTHORIZED IS-CHALLENGE DETAILS) list."
  (dbus-call-method-non-blocking
   :system
   "org.freedesktop.PolicyKit1"  
   "/org/freedesktop/PolicyKit1/Authority"
   "org.freedesktop.PolicyKit1.Authority" 
   "CheckAuthorization"
   `(:struct "unix-process" 
             (:array
              (:dict-entry "start-time" (:variant ,(dbk/get-emacs-start)))
              (:dict-entry "pid" (:variant ,(emacs-pid)))))
   action
   '(:array :signature "{ss}")
   0
   ""))

(defun dbk/drive-mountable-p (device)
  (and
   (string-equal
    (dbus-get-property :system "org.freedesktop.UDisks"
                       device "org.freedesktop.UDisks.Device" "IdUsage")
    "filesystem")
   (not (dbus-get-property :system "org.freedesktop.UDisks"
                           device "org.freedesktop.UDisks.Device" "DeviceIsMounted"))))

(defun dbk/drive-unmountable-p (device)
  (and
   (string-equal
    (dbus-get-property :system "org.freedesktop.UDisks"
                       device "org.freedesktop.UDisks.Device" "IdUsage")
    "filesystem")
   (dbus-get-property :system "org.freedesktop.UDisks"
                      device "org.freedesktop.UDisks.Device" "DeviceIsMounted")))

;; (defun dbk/udisks-handle-media (device)
;;   (when
      
;;     (message 
;;      (format "Recently changed device %s has mountable filesystem" device))))

(defun dbk/udisks-list-drives ()
  "Return a list of object paths for udisks devices present."
  (dbus-call-method-non-blocking
   :system
   "org.freedesktop.UDisks"
   "/org/freedesktop/UDisks"
   "org.freedesktop.UDisks"
   "EnumerateDevices"))

(defun dbk/get-drive-properties (device)
  "Return associative list of DEVICE properties from udisks."
  (mapcar
   (lambda (property)
     (cons property
           (dbus-get-property
            :system
            "org.freedesktop.UDisks"
            device
            "org.freedesktop.UDisks.Device"
            property)))
   (dbus-introspect-get-property-names
    :system 
    "org.freedesktop.UDisks"
    device
    "org.freedesktop.UDisks.Device")))

(defun dbk/mount-drive (device)
  (let ((fs-type (dbus-get-property
                  :system
                  "org.freedesktop.UDisks"
                  device
                  "org.freedesktop.UDisks.Device"
                  "IdType")))
  (dbus-call-method-non-blocking
   :system
   "org.freedesktop.UDisks"
   device
   "org.freedesktop.UDisks.Device"
   "FilesystemMount"
   fs-type '(:array))))

(defun dbk/unmount-drive (device)
  (dbus-call-method-non-blocking
   :system
   "org.freedesktop.UDisks"
   device
   "org.freedesktop.UDisks.Device"
   "FilesystemUnmount" '(:array)))
   
(defmacro dbk/filter (predicate list)
  `(let (res)
     (dolist (d ,list res)
       (when (,predicate d)
         (add-to-list 'res d)))))

(defun dbk/list-mountable-drives ()
  "Return a list of drives which can be mounted."
  ;; Can't use remove* here ;_;
  (dbk/filter dbk/drive-mountable-p (dbk/udisks-list-drives)))

(defun dbk/list-unmountable-drives ()
  "Return a list of drives which can be unmounted."
  (dbk/filter dbk/drive-unmountable-p (dbk/udisks-list-drives)))

(provide 'dbus-kit)
;;; dbus-kit.el ends here
