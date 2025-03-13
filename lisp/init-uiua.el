;;; init-uiua.el --- Support for the Uiua programming language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(or (use-package uiua-ts-mode) (use-package uiua-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((uiua-mode uiua-ts-mode) . ("uiua" "lsp"))))

(use-package nixpkgs-fmt)

(provide 'init-uiua)
;;; init-uiua.el ends here
