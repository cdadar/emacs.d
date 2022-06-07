;;; init-vimrc.el --- vimrc support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vimrc\\'" . vimrc-mode)))


(provide 'init-vimrc)
