;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package nix-ts-mode
  :config
  (when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'nix t))
    (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))
  (use-package nix-mode
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '((nix-mode) . ("nil"))))))

(use-package nixpkgs-fmt
  :after (:any nix-ts-mode nix-mode))
(use-package nixfmt)
(use-package nix-sandbox)
(use-package nix-buffer)
(use-package nixos-options)



(provide 'init-nix)
;;; init-nix.el ends here
