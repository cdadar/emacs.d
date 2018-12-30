;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (after-load 'php-mode
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (setq php-template-compatibility nil)
    (subword-mode 1))


  (maybe-require-package 'geben)



  (when (maybe-require-package 'php-refactor-mode)
    (add-hook 'php-mode-hook 'php-refactor-mode))

  (when (maybe-require-package 'company-php)
    (after-load 'company
      (push 'company-ac-php-backend company-backends))))




(provide 'init-php)
;;; init-php.el ends here
