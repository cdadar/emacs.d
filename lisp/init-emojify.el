;;; init-emojify.el --- emojify support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(maybe-require-package 'emojify)
(with-eval-after-load 'emojify-mode
  (when (maybe-require-package 'company-emoji)
    (add-to-list 'company-backends 'company-emoji)))


(provide 'init-emojify)
