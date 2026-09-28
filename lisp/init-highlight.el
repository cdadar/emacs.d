;;; init-highlight.el --- Highlight things in buffers -*- lexical-binding: t -*-
;;; Commentary:

;; Small visual aids that apply to editing buffers: what changed since
;; the last save, and where a block of code is indented to.

;;; Code:

;; Highlight the region modified since the buffer was last saved.
(use-package goggles
  :hook ((prog-mode text-mode conf-mode) . goggles-mode))

;; Draw indentation guides.
(use-package indent-bars
  :hook (prog-mode yaml-mode yaml-ts-mode toml-ts-mode)
  :custom
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.4))
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-color-by-depth nil)
  (indent-bars-no-descend-string t)
  (indent-bars-prefer-character t)
  (indent-bars-treesit-support
   (and (fboundp 'treesit-available-p) (treesit-available-p))))

(provide 'init-highlight)
;;; init-highlight.el ends here
