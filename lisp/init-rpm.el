;;; init-rpm.el --- rpm support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package rpm-spec-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.spec\\'" . rpm-spec-mode)))

(provide 'init-rpm)
