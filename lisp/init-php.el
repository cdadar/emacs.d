(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode))


(after-load 'php-mode
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq php-template-compatibility nil)
  (subword-mode 1))

(provide 'init-php)
