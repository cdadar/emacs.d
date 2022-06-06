;;; init-emmet.el --- emmet plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package emmet-mode
  :hook ((web-mode html-mode sgml-mode css-mode js-mode js2-mode) . emmet-mode)
  :cofnig
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? nil)
  (setq emmet-self-closing-tag-style " /"))

(provide 'init-emmet)
