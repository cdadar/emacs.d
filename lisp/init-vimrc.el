;;; init-vimrc.el --- vimrc support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(when (maybe-require-package 'vimrc-mode)
  (add-to-list 'auto-mode-alist '("\\.vimrc\\'" . vimrc-mode)))

(provide 'init-vimrc)
