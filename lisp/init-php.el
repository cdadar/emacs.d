;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package php-mode
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq php-template-compatibility nil)
  (subword-mode 1)

  (use-package geben)

  (use-package php-refactor-mode
    :hook
    (php-mode . php-refactor-mode))
  (use-package smarty-mode))

(provide 'init-php)
;;; init-php.el ends here
