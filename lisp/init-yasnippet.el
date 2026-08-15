;;; init-yasnippet.el --- yasnippet config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(defun my-gclip ()
  "Return the clipboard text, falling back to the kill-ring head.
Used by the `gclip' / `gtodo' yasnippets."
  (or (ignore-errors (gui-get-selection 'CLIPBOARD))
      (car kill-ring)
      ""))


;; loading yasnippet will slow the startup
;; but it's necessary cost

(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode t)
  (use-package auto-yasnippet
    :bind
    (("C-c & C-w" . aya-create)
     ("C-c & C-y" . aya-expand)
     ("C-c & C-o" . aya-open-line))))

(provide 'init-yasnippet)
