;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :diminish
  :hook
  ((prog-mode text-mode) . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! c" . flymake-start))
  :config
  (unless (version< emacs-version "28.1")
    (setq eldoc-documentation-function 'eldoc-documentation-compose))
  (use-package flymake-collection
    :hook
    (after-init  . flymake-collection-hook-setup)
    (python-mode . python-mode-setup-flymake)
    :config
    (defun python-mode-setup-flymake ()
      (add-hook 'flymake-diagnostic-functions 'flymake-collection-pycodestyle nil t)
      (flymake-mode +1))))

(provide 'init-flymake)
;;; init-flymake.el ends here
