;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-code-action-indicator "✓")
  (eglot-code-action-indications '(eldoc-hint mode-line))
  :config
  (use-package consult-eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here
