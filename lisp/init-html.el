;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

(use-package tagedit
  :mode ("\\.\\(jsp\\|tmpl\\)\\'" . html-mode)
  :hook (sgml-mode . tagedit-mode)
  :bind (:map tagedit-mode-map
              ("M-?" . nil)
              ("M-s" . nil))
  :config
  (tagedit-add-paredit-like-keybindings))

(provide 'init-html)
;;; init-html.el ends here
