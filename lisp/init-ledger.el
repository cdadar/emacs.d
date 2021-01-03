;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ledger-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(dat\\|ledger\\)\\'" . ledger-mode))

  (when (maybe-require-package 'flycheck-ledger)
    (with-eval-after-load 'flycheck
      (with-eval-after-load 'ledger-mode
        (require 'flycheck-ledger))))

  (with-eval-after-load 'ledger-mode
    (define-key ledger-mode-map (kbd "RET") 'newline)
    (define-key ledger-mode-map (kbd "C-o") 'open-line))

  (setq ledger-highlight-xact-under-point nil
        ledger-use-iso-dates nil)

  (with-eval-after-load 'ledger-mode
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-copy-env "LEDGER_FILE")))


  (when (maybe-require-package 'company-ledger)
    (with-eval-after-load 'company
      (with-eval-after-load 'ledger-mode
        (add-to-list 'company-backends 'company-ledger))))

  (add-hook 'ledger-mode-hook 'goto-address-prog-mode))

(require-package 'hledger-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
