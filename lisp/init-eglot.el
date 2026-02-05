;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :init
  (setq-default eglot-extend-to-xref t)
  (setq eglot-code-action-indications '(eldoc-hint mode-line))
  :config
  (use-package consult-eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here
