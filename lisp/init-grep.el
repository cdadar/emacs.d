;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))


(when (executable-find "rg")
  (use-package rg
    :config
    (global-set-key (kbd "M-?") 'rg-project)))


(use-package grep-dired
  :vc(:url "https://github.com/manateelazycat/grep-dired"))




(provide 'init-grep)
;;; init-grep.el ends here
