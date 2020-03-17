;;; init-yasnippet.el --- yasnippet config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


;; loading yasnippet will slow the startup
;; but it's necessary cost

(when (maybe-require-package 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  ;;加载模板位置

  (setf yas-indent-line 'fixed)
  (when (maybe-require-package 'auto-yasnippet)
    (global-set-key (kbd "C-c & C-w") 'aya-create)
    (global-set-key (kbd "C-c & C-y") 'aya-expand)
    (global-set-key (kbd "C-c & C-o") 'aya-open-line)))




(provide 'init-yasnippet)
