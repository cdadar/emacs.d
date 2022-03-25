;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ledger-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(dat\\|ledger\\)\\'" . ledger-mode))

  (setq ledger-highlight-xact-under-point nil
        ledger-use-iso-dates nil)

  (add-hook 'ledger-mode-hook 'goto-address-prog-mode))

(with-eval-after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line)

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE"))
  (with-eval-after-load 'flymake
    (when (maybe-require-package 'ledger-flymake)
      (ledger-flymake-enable))))


(require-package 'hledger-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
