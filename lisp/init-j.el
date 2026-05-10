;;; init-j.el --- Basic support for programming in J -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package j-mode
  :custom
  (j-console-cmd "jconsole")
  :hook
  (inferior-j-mode-hook . (lambda () (electric-pair-mode -1))))


(provide 'init-j)
;;; init-j.el ends here
