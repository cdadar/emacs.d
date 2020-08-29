;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (with-eval-after-load 'python
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                           (anaconda-mode 1))))
    (add-hook 'anaconda-mode-hook
              (lambda ()
                (anaconda-eldoc-mode (if anaconda-mode 1 0)))))
  (with-eval-after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))
  (when (maybe-require-package 'company-anaconda)
    (with-eval-after-load 'company
      (with-eval-after-load 'python
        (add-to-list 'company-backends 'company-anaconda)))))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black"))

(with-eval-after-load 'python

  (when (maybe-require-package 'virtualenvwrapper)

    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell)             ;; if you want eshell support
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual" "venv"))
    (setq projectile-switch-project-action
          '(lambda ()
             (venv-projectile-auto-workon)
             (projectile-find-file)))

    (add-hook 'venv-postmkvirtualenv-hook
              (lambda () (shell-command "pip install pyflakes")))
    )

  (when (maybe-require-package 'flycheck-pyflakes)
    (require 'flycheck-pyflakes)
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint))

  (when (executable-find "autopep8")
    (when (maybe-require-package 'py-autopep8)
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
      (setq py-autopep8-options '("--max-line-length=100")))))


(provide 'init-python)
;;; init-python.el ends here
