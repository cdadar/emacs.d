(when (maybe-require-package 'rpm-spec-mode)
  (add-to-list 'auto-mode-alist '("\\.spec\\'" . rpm-spec-mode)))

(provide 'init-rpm)
