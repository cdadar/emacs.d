;;; init-haml.el --- Haml template support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haml-mode
  :bind
  (:map haml-mode-map ("C-o" . open-line)))

(provide 'init-haml)
;;; init-haml.el ends here
