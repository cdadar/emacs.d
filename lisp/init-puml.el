;;; init-plantuml.el --- plantuml support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(use-package plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(provide 'init-puml)
