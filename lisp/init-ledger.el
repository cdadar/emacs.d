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
    (with-eval-after-load 'ledger-mode
      (defvar company-ledger-active-p nil
        "The status of company-ledger plugins. Default is disable.")

      (defun +toggle-company-ledger ()
        "Toggle company ledger"
        (interactive)
        (when (boundp 'company-backends)
          (if company-ledger-active-p
              (progn
                (setq company-backends (delete 'company-ledger company-backends))
                (setq company-ledger-active-p nil)
                (message "company ledger has disable."))
            (if (not company-mode)
                (company-mode t))
            (add-to-list 'company-backends 'company-ledger)
            (setq company-ledger-active-p t)
            (message "company ledger has enable."))))))

  (add-hook 'ledger-mode-hook 'goto-address-prog-mode))

(require-package 'hledger-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
