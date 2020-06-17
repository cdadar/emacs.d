;;; init-golang.el --- golang support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;;; Basic golang setup
(when (maybe-require-package 'go-mode)
  (with-eval-after-load 'go-mode

    (add-auto-mode 'go-mode "\\.go\\'")

    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook (lambda ()
                              (setq tab-width 4)
                              (exec-path-from-shell-copy-env "GOPATH")))
    (add-hook 'go-mode-hook 'subword-mode)

    (add-hook 'go-mode-hook
              (lambda () (setq-default tab-width 2)))

    ;;; Company-go
    (when (maybe-require-package 'company-go)
      (with-eval-after-load 'company
        (add-hook 'go-mode-hook
                  (add-to-list 'company-backends 'company-go))))

    ;;; Golang eldoc
    (when (maybe-require-package 'go-eldoc)
      (with-eval-after-load 'go-eldoc
        (add-hook 'go-mode-hook 'go-eldoc-setup)))


    (when (maybe-require-package 'go-guru)
      (with-eval-after-load 'go-guru
        (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)))
    (maybe-require-package 'go-rename)))



(provide 'init-golang)
