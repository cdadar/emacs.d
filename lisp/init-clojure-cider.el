;;; init-clojure-cider.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-clojure)

(use-package cider
  :init
  (setq nrepl-popup-stacktraces nil)
  :hook
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . paredit-mode)
  (clojure-ts-mode . cider-mode))

;; Flymake path for Clojure: prefer eglot + clojure-lsp when available.
;; This keeps diagnostics in the Flymake ecosystem. If `clojure-lsp' is not
;; installed, this block is inert.
(use-package eglot
  :ensure nil
  :if (executable-find "clojure-lsp")
  :hook ((clojure-mode clojure-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojure-ts-mode) . ("clojure-lsp"))))

(provide 'init-clojure-cider)
;;; init-clojure-cider.el ends here
