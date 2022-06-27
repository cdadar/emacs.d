;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ledger-mode
  :hook
  (ledger-mode . goto-address-prog-mode)
  :bind
  (:map ledger-mode-map
        ("RET" . newline)
        ("C-o" . open-line))
  :mode ("\\.\\(dat\\|ledger\\)\\'" . ledger-mode)
  :config
  (setq ledger-highlight-xact-under-point nil
        ledger-use-iso-dates nil)

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE"))

  (with-eval-after-load 'flymake
    (use-package ledger-flymake
      :config
      (ledger-flymake-enable))))

(use-package hledger-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
