;;; init-eglot.el --- Language Server Protocol Support for Emacss -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:
(when (maybe-require-package 'eglot)
  (add-hook 'prog-mode-hook 'eglot-ensure))

(provide 'init-eglot)
;;; init-eglot.el ends here
