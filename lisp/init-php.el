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
      (add-hook 'php-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-ac-php-backend))))))




(provide 'init-php)
