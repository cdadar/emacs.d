;;; init-lsp.el --- Language Server Protocol Support for Emacss -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(when (maybe-require-package 'lsp-mode)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (global-set-key (kbd "C-c l") nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (add-hook 'lisp-mode-hook  #'lsp-enable-which-key-integration)
  (add-hook 'js-mode-hook  #'lsp-enable-which-key-integration)

  (with-eval-after-load 'lsp-mode
    (lsp-headerline-breadcrumb-mode -1)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-headerline-breadcrumb-segments '(symbols))
    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 1024)) ;; 1mb
    )

  (when (maybe-require-package 'consult-lsp)
    (with-eval-after-load 'consult-lsp
      (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)))


  (when (maybe-require-package 'company-lsp)
    (push 'company-lsp company-backends))


  )


(provide 'init-lsp)
;;; init-lsp.el ends here
