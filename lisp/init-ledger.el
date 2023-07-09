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
        ledger-use-iso-dates nil
        ledger-mode-should-check-version nil)

  (set ledger-reports
       '(("当月收支分类统计明细" "%(binary) [[ledger-mode-flags]] -f %(ledger-file) reg ^Expenses ^Incomes -M -X CNY --begin '2023-07-01'")
         ("每月收支分类明细" "%(binary) [[ledger-mode-flags]] -f %(ledger-file) reg ^Expenses ^Incomes -M -X CNY")
         ("bal" "%(binary) -f %(ledger-file) bal")
         ("reg" "%(binary) -f %(ledger-file) reg")
         ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
         ("account" "%(binary) -f %(ledger-file) reg %(account)")))

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE")
)

  (with-eval-after-load 'flymake
    (ledger-flymake-enable)))

(use-package hledger-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
