;;; init-dict.el --- youdao dictionary  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t)
  :bind
  (("C-c C-y" . youdao-dictionary-search-at-point)))

(provide 'init-dict)
