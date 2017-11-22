(ensure-lib-from-url 'crontab-mode "http://www.mahalito.net/~harley/elisp/crontab-mode.el")

(require 'crontab-mode)

(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'" "cron\\(tab\\)?\\.*")

(provide 'init-crontab)
