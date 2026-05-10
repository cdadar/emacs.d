;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser

(use-package dash-at-point
  :if *is-a-mac*
  :bind (("C-c D" . dash-at-point)))

(defun +zeal-installed-p ()
  (let ((zeal "/usr/bin/zeal"))
    (file-executable-p zeal)))

(use-package zeal-at-point
  :if (and *linux* (+zeal-installed-p))
  :bind
  (("C-c D" . zeal-at-point)))

(provide 'init-dash)
;;; init-dash.el ends here
