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

  (require-package 'pyenv-mode)

  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

  (when (maybe-require-package 'virtualenvwrapper)

    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    (add-hook 'projectile-before-switch-project-hook
              '(lambda ()
                 (progn
                   (venv-deactivate))))
    (add-hook 'projectile-after-switch-project-hook
              '(lambda ()
                 (progn
                   (when (eq major-mode 'python-mode)
                     (venv-workon (projectile-project-name))))))))









(provide 'init-python)
