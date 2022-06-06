;;; init-evil.el --- evil-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook
  (after-init . evil-mode)
  :config
  (use-package evil-collection
    :config
    (evil-collection-init)))

(use-package 'general)

(provide 'init-evil)
;;; init-evil.el ends here
