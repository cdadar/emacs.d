;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-code-action-indicator "✓")
  (eglot-code-action-indications '(eldoc-hint mode-line))
  :config
  ;; Emacs 31 turns on eglot-semantic-tokens-mode by default, which can be
  ;; slow and visually noisy; disable it.
  (defun sanityinc/disable-eglot-semantic-tokens ()
    (eglot-semantic-tokens-mode -1))
  (add-hook 'eglot-managed-mode-hook #'sanityinc/disable-eglot-semantic-tokens)
  (use-package consult-eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here
