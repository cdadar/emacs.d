;;; init-rpm.el --- rpm support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(when (maybe-require-package 'rpm-spec-mode)
  (add-to-list 'auto-mode-alist '("\\.spec\\'" . rpm-spec-mode)))

(provide 'init-rpm)
