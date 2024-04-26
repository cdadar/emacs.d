;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :diminish
  :bind
  (:map envrc-mode-map
        ("C-c $" . envrc-command-map))
  :hook
  (after-init . envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
