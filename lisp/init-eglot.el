;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :config
  (setq eglot-code-action-indications '(eldoc-hint mode-line))
  (use-package consult-eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here
