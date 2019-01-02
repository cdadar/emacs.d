;;; init-emojify.el --- emojify support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(maybe-require-package 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)
(after-load 'emojify-mode
  (when (maybe-require-package 'company-emoji)
    (push 'company-emoji company-backends)))


(provide 'init-emojify)
