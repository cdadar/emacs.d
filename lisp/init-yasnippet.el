;;; init-yasnippet.el --- yasnippet config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


;; loading yasnippet will slow the startup
;; but it's necessary cost

(when (maybe-require-package 'yasnippet)
  (yas-global-mode t)
  ;;加载模板位置
  (setq yas/root-directory (expand-file-name "./snippets" user-emacs-directory))
  (yas/load-directory yas/root-directory)

  (setf yas-indent-line 'fixed)

  ;; (when (maybe-require-package 'diminish)
  ;;   (after-load 'yasnippet
  ;;     (diminish 'yas-minor-mode)))
  )

(provide 'init-yasnippet)
