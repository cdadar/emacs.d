;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(use-package yagist)
(use-package bug-reference-github
  :hook
  (prog-mode . bug-reference-prog-mode))


(use-package github-clone)
(use-package forge)
(use-package github-review)

(use-package flymake-actionlint
  :hook (yaml-mode . flymake-actionlint-action-load-when-actions-file))

(provide 'init-github)
;;; init-github.el ends here
