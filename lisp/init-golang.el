;;; init-golang.el --- golang support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;;; Basic golang setup
(when (maybe-require-package 'go-mode)
  (after-load 'go-mode

    (add-auto-mode 'go-mode "\\.go\\'")

    (add-hook 'go-mode-hook (lambda ()
                              (setq tab-width 4)
                              (exec-path-from-shell-copy-env "GOPATH")))
    (add-hook 'go-mode-hook 'subword-mode)

    (add-hook 'go-mode-hook
              (lambda () (setq-default tab-width 2))))


  ;;; Company-go
  (when (maybe-require-package 'company-go)
    (after-load 'company
      (add-hook 'go-mode-hook
                (push 'company-go company-backends))))

  ;;; Golang eldoc
  (when (maybe-require-package 'go-eldoc)
    (after-load 'go-eldoc
      (add-hook 'go-mode-hook 'go-eldoc-setup))))

(provide 'init-golang)
