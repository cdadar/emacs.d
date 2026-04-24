;;; init-erc.el --- Emacs Prelude: ERC mode configuration -*- lexical-binding: t -*-
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for ERC mode, which should make your
;; IRC experience a bit more pleasant.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(use-package erc
  :ensure nil
  :commands (erc start-irc stop-irc)
  :preface
  (defun filter-server-buffers ()
    (delq nil
          (mapcar
           (lambda (x) (and (erc-server-buffer-p x) x))
           (buffer-list))))

  (defun start-irc ()
    "Connect to IRC."
    (interactive)
    (when (y-or-n-p "Do you want to start IRC? ")
      (erc :server "irc.libera.chat" :port 6667 :nick erc-nick)))

  (defun stop-irc ()
    "Disconnects from all irc servers"
    (interactive)
    (dolist (buffer (filter-server-buffers))
      (message "Server buffer: %s" (buffer-name buffer))
      (with-current-buffer buffer
        (erc-quit-server "Asta la vista"))))
  :custom
  ;; The following are commented out by default, but users of other
  ;; non-Emacs IRC clients might find them useful.
  ;; Kill buffers for channels after /part
  (erc-kill-buffer-on-part t)

  ;; Kill buffers for private queries after quitting the server
  (erc-kill-queries-on-quit t)

  ;; Kill buffers for server messages after quitting the server
  (erc-kill-server-buffer-on-quit t)

  ;; utf-8 always and forever
  (erc-server-coding-system '(utf-8 . utf-8))

  ;; open query buffers in the current window
  (erc-query-display 'buffer))

(use-package erc-join
  :ensure nil
  :after erc
  :config
  (erc-autojoin-mode 1))

(use-package erc-track
  :ensure nil
  :after erc
  :custom
  ;; exclude boring stuff from tracking
  (erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (erc-track-mode 1))

(use-package erc-goodies
  :ensure nil
  :after erc
  :custom
  ;; Interpret mIRC-style color commands in IRC chats
  (erc-interpret-mirc-color t)
  :config
  ;; truncate long irc buffers
  (erc-truncate-mode +1))

(use-package erc-notify
  :ensure nil
  :after erc
  :preface
  (defvar erc-notify-nick-alist nil
    "Alist of nicks and the last time they tried to trigger a
notification")

  (defvar erc-notify-timeout 10
    "Number of seconds that must elapse between notifications from
the same person.")

  (defun erc-notify-allowed-p (nick &optional delay)
    "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
    (unless delay (setq delay erc-notify-timeout))
    (let ((cur-time (time-to-seconds (current-time)))
          (cur-assoc (assoc nick erc-notify-nick-alist))
          (last-time nil))
      (if cur-assoc
          (progn
            (setq last-time (cdr cur-assoc))
            (setcdr cur-assoc cur-time)
            (> (abs (- cur-time last-time)) delay))
        (push (cons nick cur-time) erc-notify-nick-alist)
        t))))

(use-package erc-autoaway
  :ensure nil
  :after erc
  :custom
  ;; autoaway setup
  (erc-auto-discard-away t)
  (erc-autoaway-idle-seconds 600)
  :config
  (if (boundp 'erc-autoaway-idle-method)
      (setq erc-autoaway-idle-method 'emacs)
    (setq erc-autoaway-use-emacs-idle t)))

(use-package erc-spelling
  :ensure nil
  :after erc
  :config
  ;; enable spell checking
  ;; set different dictionaries by different servers/channels
  ;; (setq erc-spelling-dictionaries '(("#emacs" "american")))
  (when *spell-check-support-enabled*
    (erc-spelling-mode 1)))

(use-package erc-log
  :ensure nil
  :after erc
  :custom
  ;; logging
  (erc-log-channels-directory "~/.erc/logs/")
  (erc-save-buffer-on-part t)
  :config
  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))
  ;; FIXME - this advice is wrong and is causing problems on Emacs exit
  ;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  ;;   (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t)))))
  )

(provide 'init-erc)

;;; init-erc.el ends here
