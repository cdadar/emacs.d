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

(use-package flycheck-clojure
  :after (clojure-mode cider flycheck)
  :config
  (flycheck-clojure-setup))

(provide 'init-clojure-cider)
;;; init-clojure-cider.el ends here
