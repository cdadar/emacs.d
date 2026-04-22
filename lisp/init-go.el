;;; init-go.el --- Support for the Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic golang setup

(defun sanityinc/go-mode-setup ()
  "Shared setup for Go buffers."
  (setq-local tab-width 4)
  (when (fboundp 'exec-path-from-shell-copy-env)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package reformatter
  :demand t
  :config
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(use-package go-mode
  :commands (gofmt-before-save)
  :hook ((go-mode . sanityinc/go-mode-setup)
         (go-mode . subword-mode)
         (go-mode . gofmt-before-save))
  :mode ("\\.go\\'" . go-mode)
  :custom
  (gofmt-command "goimports"))

(use-package go-eldoc
  :hook ((go-mode . go-eldoc-setup)
         (go-ts-mode . go-eldoc-setup)))

(use-package go-guru
  :hook ((go-mode . go-guru-hl-identifier-mode)
         (go-ts-mode . go-guru-hl-identifier-mode)))

(use-package go-rename)

(use-package go-ts-mode
  :ensure nil
  :if (and (fboundp 'treesit-ready-p)
           (treesit-ready-p 'go t))
  :hook ((go-ts-mode . sanityinc/go-mode-setup)
         (go-ts-mode . subword-mode)
         (go-ts-mode . go-format-on-save-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  :mode ("/go\\.mod\\'" . go-mod-ts-mode))

(provide 'init-go)
;;; init-go.el ends here
