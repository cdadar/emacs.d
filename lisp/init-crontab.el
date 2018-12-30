;;; init-crontab.el --- Working with crontabs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(ensure-lib-from-url 'crontab-mode "http://www.mahalito.net/~harley/elisp/crontab-mode.el")

(require 'crontab-mode)

(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'" "\\^cron\\(tab\\)?\\.*")

;; (require-package 'crontab-mode)
;; (add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(provide 'init-crontab)
;;; init-crontab.el ends here
