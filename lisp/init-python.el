(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))

(after-load 'python

  (when (maybe-require-package 'virtualenvwrapper)

    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual" "venv"))
    (setq projectile-switch-project-action
          '(lambda ()
             (venv-projectile-auto-workon)
             (projectile-find-file)))

    ;; (add-hook 'venv-postmkvirtualenv-hook
    ;;           (lambda () (shell-command "pip install pylint")))
    )


  (when (executable-find "autopep8")
    (when (maybe-require-package 'py-autopep8)
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
      (setq py-autopep8-options '("--max-line-length=100")))))









(provide 'init-python)
