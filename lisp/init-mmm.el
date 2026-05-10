;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mmm-mode
  :custom
  (mmm-submode-decoration-level 2)
  :config
  (require 'mmm-auto)
  (setq mmm-global-mode 'buffers-with-submode-classes))


(provide 'init-mmm)
;;; init-mmm.el ends here
