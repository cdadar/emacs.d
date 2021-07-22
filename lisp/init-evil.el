;;; init-evil.el --- evil-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'evil)

  (add-hook 'after-init-hook 'evil-mode)

  (with-eval-after-load 'evil
    (when (maybe-require-package 'evil-collection)
      (evil-collection-init))))


(when (maybe-require-package 'general)
  )

(provide 'init-evil)
;;; init-evil.el ends here
