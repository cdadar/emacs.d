;;; init-yasnippet.el --- yasnippet config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


;; loading yasnippet will slow the startup
;; but it's necessary cost

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  (setf yas-indent-line 'fixed)
  (use-package auto-yasnippet
    :bind
    (("C-c & C-w" . aya-create)
     ("C-c & C-y" . aya-expand)
     ("C-c & C-o" . aya-open-line))))

(provide 'init-yasnippet)
