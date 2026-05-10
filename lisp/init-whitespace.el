;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package simple
  :ensure nil
  :custom
  (show-trailing-whitespace nil)
  :preface
  (defun sanityinc/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t))
  :hook
  ((prog-mode text-mode conf-mode) . sanityinc/show-trailing-whitespace)
  :bind
  ([remap just-one-space] . cycle-spacing))

(use-package whitespace-cleanup-mode
  :diminish
  :hook
  (after-init . global-whitespace-cleanup-mode))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
