;;; init-golang.el --- golang support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(when (maybe-require-package 'go-mode)
  (after-load 'go-mode
    (add-hook 'go-mode-hook
              (lambda () (setq-default tab-width 2))))
  (when (maybe-require-package 'company-go)
    (after-load 'company
      (add-hook 'go-mode-hook
                (push 'company-go company-backends)))))

(provide 'init-golang)
