;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package nix-ts-mode
  :config
  ;; If the TS mode is installed, then the non-TS mode is not, so
  ;; nobody will have added an auto-mode-alist entry
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (use-package nix-mode
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ,(eglot-alternatives '("nixd" "nil")))))))

(use-package nixpkgs-fmt
  :after (:any nix-ts-mode nix-mode))
(use-package nixfmt)
(use-package nix-sandbox)
(use-package nix-buffer)
(use-package nixos-options)



(provide 'init-nix)
;;; init-nix.el ends here
