;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))




(when (executable-find "ag")
  (use-package ag
    :config
    (global-set-key (kbd "M-?") 'ag-project)
    (setq-default ag-highlight-search t))
  (use-package wgrep-ag))

(when (executable-find "rg")
  (use-package rg
    :config
    (global-set-key (kbd "M-?") 'rg-project)))


(if emacs/>=30p
    (use-package grep-dired
      :ensure nil
      :vc(:url "https://github.com/manateelazycat/grep-dired"))
  (use-package grep-dired
    :load-path "site-lisp/grep-dired"
    ))



(provide 'init-grep)
;;; init-grep.el ends here
