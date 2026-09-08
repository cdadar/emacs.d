;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-documentation-function #'eldoc-documentation-compose))

(use-package flymake
  :hook
  ((prog-mode text-mode) . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! c" . flymake-start))
  :config
  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-to-list 'eldoc-documentation-functions 'flymake-eldoc-function))))

(use-package flycheck
  ;; Pure checker provider for flymake-flycheck; no flycheck UI is enabled.
  ;; Deferred until flymake-flycheck pulls it in.
  :defer t
  :config
  ;; Disable flycheck checkers for which we have flymake equivalents.
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))

;; Use flycheck checkers with flymake, to extend its coverage
(use-package flymake-flycheck
  :after (flymake flycheck)
  :hook
  (flymake-mode . flymake-flycheck-auto))

(provide 'init-flymake)
;;; init-flymake.el ends here
