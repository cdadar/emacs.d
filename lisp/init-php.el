;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (with-eval-after-load 'php-mode
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (setq php-template-compatibility nil)
    (subword-mode 1)

    (maybe-require-package 'geben)

    (when (maybe-require-package 'company-php)
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'company-ac-php-backend)))

    (when (maybe-require-package 'php-refactor-mode)
      (add-hook 'php-mode-hook 'php-refactor-mode))))




(provide 'init-php)
;;; init-php.el ends here
