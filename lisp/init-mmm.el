;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mmm-mode
  :custom
  (mmm-submode-decoration-level 2)
  (mmm-global-mode 'buffers-with-submode-classes)
  :config
  (require 'mmm-auto))


(provide 'init-mmm)
;;; init-mmm.el ends here
