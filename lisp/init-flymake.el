;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'flymake "1.2.1")

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'text-mode-hook 'flymake-mode)

(with-eval-after-load 'flymake
  ;; Provide some flycheck-like bindings in flymake mode to ease transition
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start))

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose))

(with-eval-after-load 'flymake
  (when (maybe-require-package 'flymake-collection)
    (add-hook 'after-init-hook 'flymake-collection-hook-setup)
    (add-hook 'python-mode-hook
              (defun python-mode-setup-flymake ()
                (add-hook 'flymake-diagnostic-functions 'flymake-collection-pycodestyle nil t)
                (flymake-mode +1)))))

(provide 'init-flymake)
;;; init-flymake.el ends here
