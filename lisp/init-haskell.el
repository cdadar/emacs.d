;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :hook
  (haskell-mode . subword-mode)
  (haskell-cabal-mode . subword-mode)
  (haskell-mode . interactive-haskell-mode)
  ;; Indentation
  (haskell-mode-hook . turn-on-haskell-indentation)
  ;; Source code helpers
  (haskell-mode-hook . haskell-auto-insert-module-template)
  :bind
  (:map haskell-mode-map
        ("C-c h" . hoogle)
        ("C-o" . open-line))
  :config
  (add-auto-mode 'haskell-mode "\\.ghci\\'")
  (use-package reformatter
    :config
    (reformatter-define hindent
      :program "hindent"
      :lighter " Hin")

    (defalias 'hindent-mode 'hindent-on-save-mode)
    (reformatter-define ormolu
      :program "ormolu"
      :lighter " Orm"))
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'haskell-mode)))


(use-package dhall-mode)

(provide 'init-haskell)
;;; init-haskell.el ends here
