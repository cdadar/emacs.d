;;; init-clojure.el --- Clojure support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See also init-clojure-cider.el
(use-package clojure-mode
  :config
  (dolist (m '(clojure-mode-hook clojure-ts-mode-hook))
      (add-hook m 'sanityinc/lisp-setup)))

(use-package cljsbuild-mode
  :after (:any clojure-mode clojure-ts-mode))

(use-package elein
  :after (:any clojure-mode clojure-ts-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
