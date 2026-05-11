;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc")
  (add-auto-mode 'markdown-mode "\\.md\\..markdown\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
