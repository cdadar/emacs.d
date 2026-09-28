;;; init-dap.el --- Debugging via the Debug Adapter Protocol -*- lexical-binding: t -*-
;;; Commentary:

;; `dape' drives debug adapters (debugpy, delve, codelldb, ...) the same
;; way `eglot' drives language servers.  Language servers must be
;; installed separately, as with eglot.
;;
;; NOTE: `init-gud' remains commented out in init.el; dape replaces it.

;;; Code:

(use-package dape
  ;; <f5>..<f8> are taken (theme, recompile, split, deft); <f9> is free.
  :bind (("<f9>" . dape))
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  (dape-stack-trace-levels 5))

(provide 'init-dap)
;;; init-dap.el ends here
