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

    ;; enable log only for debug
    (setq lsp-log-io nil)
    ;; use `evil-matchit' instead
    (setq lsp-enable-folding nil)
    ;; no real time syntax check
    (setq lsp-diagnostic-package :none)
    ;; handle yasnippet by myself
    (setq lsp-enable-snippet nil)
    ;; use `company-ctags' only.
    ;; Please note `company-lsp' is automatically enabled if it's installed
    (setq lsp-enable-completion-at-point nil)
    ;; turn off for better performance
    (setq lsp-enable-symbol-highlighting nil)
    ;; use find-fine-in-project instead
    (setq lsp-enable-links nil)
    ;; auto restart lsp
    (setq lsp-restart 'auto-restart)
    ;; don't watch 3rd party javascript libraries
    (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)
    ;; don't ping LSP language server too frequently
    (defvar lsp-on-touch-time 0)
    (defun my-lsp-on-change-hack (orig-fun &rest args)
      ;; do NOT run `lsp-on-change' too frequently
      (when (> (- (float-time (current-time))
                  lsp-on-touch-time) 120) ;; 2 mins
        (setq lsp-on-touch-time (float-time (current-time)))
        (apply orig-fun args)))
    (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack)
    )

  (when (maybe-require-package 'consult-lsp)
    (with-eval-after-load 'consult-lsp
      (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)))


  (when (maybe-require-package 'company-lsp)
    (push 'company-lsp company-backends))


  )


(provide 'init-lsp)
;;; init-lsp.el ends here
