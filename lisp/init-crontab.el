;;; init-crontab.el --- Working with crontabs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package crontab-mode
  :config
  (add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'" "\\^cron\\(tab\\)?\\.*"))

(provide 'init-crontab)
;;; init-crontab.el ends here
