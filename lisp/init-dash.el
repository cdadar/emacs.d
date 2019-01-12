;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser

(when *is-a-mac*
  (require-package 'dash-at-point)
  (global-set-key (kbd "C-c D") 'dash-at-point))


(defun cdadar/zeal-installed-p ()
  (let ((zeal "/usr/bin/zeal"))
    (file-executable-p zeal)))

(when (and *linux* (not (package-installed-p 'zeal-at-point)))
  (message "CHecking whether Zeal in installed")
  (when (cdadar/zeal-installed-p)
    (require-package 'zeal-at-point)))

(when (package-installed-p 'zeal-at-point)
  (global-set-key (kbd "C-c D") 'zeal-at-point))


(provide 'init-dash)
;;; init-dash.el ends here
