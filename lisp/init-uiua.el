;;; init-uiua.el --- Support for the Uiua programming language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package uiua-ts-mode
  :if (fboundp 'uiua-ts-mode))

(use-package uiua-mode
  :defer t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((uiua-mode uiua-ts-mode) . ("uiua" "lsp"))))

(use-package nixpkgs-fmt)

(provide 'init-uiua)
;;; init-uiua.el ends here
