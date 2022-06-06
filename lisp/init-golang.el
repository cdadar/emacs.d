;;; init-golang.el --- golang support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;;; Basic golang setup
(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  (go-mode . (lambda ()
               (setq tab-width 4)
               (exec-path-from-shell-copy-env "GOPATH")))
  (go-mode . subword-mode)
  :config
  (add-auto-mode 'go-mode "\\.go\\'")
  (setq gofmt-command "goimports")
  (use-package go-eldoc
    :hook
    (go-mode . go-eldoc-setup))
  (use-package go-guru
    :hook
    (go-mode . go-guru-hl-identifier-mode))
  (use-package go-rename))

(provide 'init-golang)
