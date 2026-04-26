;;; insert-time.el --- insert time and date stamps at point -*- lexical-binding: t -*-

;; Copyright (C) 2001-2015 Ryan McGeary
;; Author: Ryan McGeary
;; Keywords: time date insert format

;; This code is free; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;;
;; Purpose
;; -------
;; insert-time provides an easy way to insert time and date stamps into
;; your files
;;
;; Usage
;; -----
;; Example of lines to be added to your .emacs:
;;
;;     (require 'insert-time)
;;
;;     ; in case you don't like the defaults
;;     ; (see `format-time-string' for format)
;;     (setq insert-date-format "%Y-%m-%d")
;;     (setq insert-time-format "%H:%M:%S")
;;     (setq insert-date-time-format "%Y-%m-%dT%T%z")
;;
;;     ; keyboard shortcuts
;;     (define-key global-map [(control c)(d)] 'insert-date-time)
;;     (define-key global-map [(control c)(control v)(d)] 'insert-personal-time-stamp)
;;
;; How it works
;; ------------
;; just a few inserts using the emacs built in format-time-string
;;
;; Limitations
;; -----------
;;

;;; History:
;;
;; 13-Jul-2015 - iso8601 defaults
;; 30-Jan-2007 - defvar -> defcustom
;; 25-Mar-2001 - created by
;;               Ryan McGeary

;;; Code:

(defgroup insert-time nil
  "Insert time and date stamps at point."
  :group 'editing)

(defcustom insert-date-format "%Y-%m-%d"
  "*Format for `insert-date' (see `format-time-string' for how to
  format)."
  :type 'string
  :group 'insert-time)

(defcustom insert-time-format "%T%z"
  "*Format for `insert-time' (see `format-time-string' for how to
  format)."
  :type 'string
  :group 'insert-time)

(defcustom insert-date-time-format "%Y-%m-%dT%T%z"
  "*Format for `insert-date-time' (see `format-time-string' for
  how to format)."
  :type 'string
  :group 'insert-time)

(defcustom current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format used by `insert-current-date-time'.
See `format-time-string' for supported format specifiers."
  :type 'string
  :group 'insert-time)

(defcustom current-time-format "%a %H:%M:%S"
  "Format used by `insert-current-time'.
See `format-time-string' for supported format specifiers."
  :type 'string
  :group 'insert-time)

(defun insert-time--insert-formatted-time (format &optional append-newline)
  "Insert the current time formatted with FORMAT at point.
When APPEND-NEWLINE is non-nil, append a trailing newline."
  (insert (format-time-string format (current-time)))
  (when append-newline
    (insert "\n")))

(defun insert-date ()
  "Inserts the current date at point in the format specified by
`insert-date-format'."
  (interactive "*")
  (insert-time--insert-formatted-time insert-date-format))

(defun insert-time ()
  "Inserts the current time at point in the format specified by
`insert-time-format'."
  (interactive "*")
  (insert-time--insert-formatted-time insert-time-format))

(defun insert-date-time ()
  "Insert the current date-time at point.
The format is controlled by `insert-date-time-format'."
  (interactive "*")
  (insert-time--insert-formatted-time insert-date-time-format))

(defun insert-personal-time-stamp ()
  "Inserts the current time-stamp at point for the current user
in the formats specified by `insert-date-time-format'.  It
inserts a timestamp of \"(<`user-login-name'> :
<`insert-date-time-format'>)\" Uses `insert-date-time'."
  (interactive "*")
  (insert "(" user-login-name " : ")
  (insert-date-time)
  (insert ")"))


(defun insert-timestamp ()
  "Insert a compact numeric timestamp at point."
  (interactive "*")
  (insert-time--insert-formatted-time "%Y%m%d%H%M%S"))

(defun insert-date-week ()
  "Insert the current date plus abbreviated weekday at point."
  (interactive "*")
  (insert-time--insert-formatted-time "%Y-%m-%d %a"))

(defun insert-current-date-time ()
  "Insert the current date and time into the current buffer.
Uses `current-date-time-format' to format the inserted value."
  (interactive "*")
;       (insert (let () (comment-start)))
  (insert-time--insert-formatted-time current-date-time-format t))

(defun insert-current-time ()
  "Insert the current time into the current buffer.
Uses `current-time-format' to format the inserted value."
  (interactive "*")
  (insert-time--insert-formatted-time current-time-format t))

(provide 'insert-time)
;;; insert-time.el ends here
