(defvar mu4e-installation-path nil
  "Installation path for mu4e.")

(setq mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")

(add-to-list 'load-path mu4e-installation-path)


(require 'mu4e)


(setq mu4e-maildir       "~/mail"
      mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash"
      mu4e-refile-folder "/archive")

(setq mu4e-get-mail-command "getmail"
      mu4e-update-interval 300)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.exmail.qq.com"
      smtpmail-smtp-service 465)
(setq user-full-name "chens")
(setq user-mail-address "chens@fangde.com")

(provide 'init-email)
