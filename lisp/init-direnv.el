;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :diminish
  :config
  (defun sanityinc/maybe-enable-envrc-global-mode ()
    "Enable `envrc-global-mode' if `direnv' is installed."
    (when (executable-find "direnv")
      (envrc-global-mode)))
  :bind
  (:map envrc-mode-map
        ("C-c $" . envrc-command-map))
  :hook
  (after-init . sanityinc/maybe-enable-envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
