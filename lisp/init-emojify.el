;;; init-emojify.el --- emojify support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(maybe-require-package 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)
(after-load 'emojify-mode
  (when (maybe-require-package 'company-emoji)
    (add-to-list 'company-backends 'company-emoji)))


(provide 'init-emojify)
