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

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(with-eval-after-load 'python

  (when (maybe-require-package 'virtualenvwrapper)

    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell)             ;; if you want eshell support
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual" "venv"))

    (add-hook 'venv-postmkvirtualenv-hook
              (lambda () (shell-command "pip install pyflakes"))))

  (when (executable-find "autopep8")
    (when (maybe-require-package 'py-autopep8)
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
      (setq py-autopep8-options '("--max-line-length=120")))))


(provide 'init-python)
;;; init-python.el ends here
