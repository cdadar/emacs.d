;;; init-lsp.el --- Language Server Protocol Support for Emacss -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(use-package lsp-mode
  :bind
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (("C-c l" . nil))
  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
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
  :hook
  (lisp-mode . lsp-enable-which-key-integration)
  (js-mode . lsp-enable-which-key-integration))

(use-package consult-lsp
  :bind
  (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

(provide 'init-lsp)
;;; init-lsp.el ends here
