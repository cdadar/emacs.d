;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(use-package pip-requirements)


(use-package flymake-ruff
  :init
  (defun sanityinc/flymake-ruff-maybe-enable ()
    (when (executable-find "ruff")
      (flymake-ruff-load)))
  :hook
  (python-mode . sanityinc/flymake-ruff-maybe-enable))

(use-package ruff-format
  :after reformatter
  :config
  (reformatter-define ruff-fix
    :program ruff-format-command
    :args (list "check" "--fix-only" "--stdin-filename" (or (buffer-file-name) input-file))
    :lighter " RuffFix"))

(use-package toml-mode
  :mode ("\\(poetry\\|uv\\)\\.lock\\'" . toml-mode))


(use-package reformatter
  :config
  (reformatter-define black :program "black" :args '("-")))


(use-package uv
  :if (executable-find "uv")
  :vc (:url "https://github.com/johannes-mueller/uv.el" :rev :newest)
  :commands (uv uv-init uv-add uv-remove uv-sync uv-lock uv-run uv-export uv-tool uv-python))

(use-package py-autopep8
  :if (executable-find "autopep8")
  :after python
  :hook (python-mode . py-autopep8-enable-on-save)
  :custom (py-autopep8-options '("--max-line-length=120")))

(use-package project
  :ensure nil
  :config
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))

(use-package projectile
  :config
  (add-to-list 'projectile-project-root-files "pyproject.toml"))


(provide 'init-python)
;;; init-python.el ends here
