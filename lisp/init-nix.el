;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package nix-ts-mode
  :init
  (defun sanityinc/set-nix-ts-auto-mode ()
        (when (and (fboundp 'treesit-ready-p)
                   (treesit-ready-p 'nix t)
                   (fboundp 'nix-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))))
  :hook
  (after-init . sanityinc/set-nix-ts-auto-mode)
  :config
  (use-package nix-mode
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '((nix-mode) . ("nil"))))))

(use-package nixpkgs-fmt
  :after (:any nix-ts-mode nix-mode))
(use-package nix-sandbox)
(use-package nix-buffer)
(use-package nixos-options)



(provide 'init-nix)
;;; init-nix.el ends here
