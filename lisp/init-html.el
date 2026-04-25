;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

(use-package tagedit
  :mode ("\\.\\(jsp\\|tmpl\\)\\'" . html-mode)
  :hook (sgml-mode . tagedit-mode)
  :config
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (define-key tagedit-mode-map (kbd "M-s") nil))

(provide 'init-html)
;;; init-html.el ends here
