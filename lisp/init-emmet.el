;;; init-emmet.el --- emmet plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(when (maybe-require-package 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'js-mode-hook 'emmet-mode))


(with-eval-after-load 'emmet-mode
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? nil)
  (setq emmet-self-closing-tag-style " /"))

(provide 'init-emmet)
