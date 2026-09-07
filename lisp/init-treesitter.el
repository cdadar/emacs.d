;;; init-treesitter.el --- Enable Treesitter-based major modes -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Uses the built-in treesit (Emacs 29+) plus treesit-auto, which
;;; installs grammars on demand and remaps standard major modes to
;;; their -ts-mode counterparts. Pre-built grammars can be placed in
;;; ~/.emacs.d/tree-sitter as libtree-sitter-<lang>.<ext>.
;;; Code:

(use-package clojure-ts-mode)
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install t)
  :config
  ;; Keep zig on manual config from init-zig.el to avoid noisy startup
  ;; warnings when the zig grammar is not installed locally.
  (setq treesit-auto-langs (delq 'zig treesit-auto-langs)))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  ;; Enable all built-in ts-modes whose grammars are available;
  ;; remapping to -ts modes is handled by treesit-auto above.
  (treesit-enabled-modes t))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
