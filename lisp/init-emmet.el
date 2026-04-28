;;; init-emmet.el --- emmet plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package emmet-mode
  :hook ((web-mode html-mode sgml-mode css-mode js-mode js2-mode) . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t)
  (emmet-jsx-className-braces? nil)
  (emmet-self-closing-tag-style " /"))

(provide 'init-emmet)
