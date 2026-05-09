;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package grep
  :ensure nil
  :custom
  (grep-highlight-matches t)
  (grep-scroll-output t))

(when *is-a-mac*
  (use-package locate
    :ensure nil
    :custom
    (locate-command "mdfind")))

(use-package rg
  :if (executable-find "rg")
  :commands (rg rg-project))

(use-package grep-dired
  :vc (:url "https://github.com/manateelazycat/grep-dired"))

(provide 'init-grep)
;;; init-grep.el ends here
