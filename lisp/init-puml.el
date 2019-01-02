;;; init-plantuml.el --- plantuml support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(when (maybe-require-package 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(provide 'init-puml)
