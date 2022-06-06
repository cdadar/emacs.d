;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser

(when *is-a-mac*
  (use-package dash-at-point
    :bind (("C-c D" . dash-at-point))))

(defun +zeal-installed-p ()
  (let ((zeal "/usr/bin/zeal"))
    (file-executable-p zeal)))

(when (and *linux* (+zeal-installed-p))
  (use-package zeal-at-point
    :bind
    (("C-c D" . zeal-at-point))))

(provide 'init-dash)
;;; init-dash.el ends here
