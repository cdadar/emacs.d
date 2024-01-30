;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode)

(use-package typescript-mode
  :config
  (use-package tide
    :hook
    (typescript-mode . setup-tide-mode)
    ;; formats the buffer before saving
    (before-save . tide-format-before-save)
    :mode "*\\.d.ts\\'"
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (tide-hl-identifier-mode +1))))


;; (use-package indium) ;; because this depend company

;; (use-package js-import) ;; because this depend projectile

;;; Basic js-mode setup
(use-package js-mode
  :ensure nil
  :mode ("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode)
  :config
  (setq-default js-indent-level 2))

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("*\\.{components|pages}\\/.*\\.js\\'" . rjsx-mode)
         ("src\\/routes\\/.*\\.js\\'" . rjsx-mode))
  :config
  (advice-add 'js-jsx-mode :override #'rjsx-mode))


;; js2-mode
(use-package js2-mode
  :hook
  (js2-mode . (lambda () (setq mode-name "JS2")))
  :interpreter ("node" . js2-mode)
  :config
  ;; Change some defaults: customize them to override
  (setq-default js2-bounce-indent-p nil)
  (setq-local js2-mode-show-parse-errors t)
  (setq-local js2-mode-show-strict-warnings t)
  (when (derived-mode-p 'js-mode)
    (js2-minor-mode 1))
  (js2-imenu-extras-setup)
    (sanityinc/major-mode-lighter 'js2-mode "JS2")
      (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))


(require 'derived)
(when (or (executable-find "rg") (executable-find "ag"))
  (use-package xref-js2
    :config
    (when (executable-find "rg")
      (setq-default xref-js2-search-program 'rg))
    (defun sanityinc/enable-xref-js2 ()
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
    (let ((base-mode (if (fboundp 'js-base-mode) 'js-base-mode 'js-mode)))
      (with-eval-after-load 'js
        (add-hook (derived-mode-hook-name base-mode) 'sanityinc/enable-xref-js2)
        (define-key js-mode-map (kbd "M-.") nil)
        (when (boundp 'js-ts-mode-map)
          (define-key js-ts-mode-map (kbd "M-." nil)))))
    (with-eval-after-load 'js2-mode
      (define-key js2-mode-map (kbd "M-.") nil))))


;; Run and interact with an inferior JS via js-comint.el

(use-package js-comint
  :config
  (setq js-comint-program-command "node")
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    :init-value nil :lighter " InfJS" :keymap inferior-js-minor-mode-map)
  :bind
  (:map inferior-js-minor-mode-map
        ("C-x C-e" . js-send-last-sexp)
        ("C-c b" . js-send-buffer))
  :hook
  ((js2-mode-hook js-mode-hook) . inferior-js-keys-mode))


(use-package add-node-modules-path
  :hook
  ((typescript-mode js-mode js2-mode coffee-mode rjsx-mode) . add-node-modules-path))

(use-package js2-refactor
  :hook
  ((typescript-mode js-mode js2-mode coffee-mode rjsx-mode) . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(when (executable-find "prettier")
  (use-package reformatter
    :config
    (reformatter-define prettier-javascript
      :program "prettier"
      :args '("--parser=babel" "--arrow-parens=avoid"))

    (add-hook 'js2-mode-hook 'prettier-javascript-on-save-mode)
    (add-hook 'rjsx-mode-hook 'prettier-javascript-on-save-mode)))

(use-package js-doc
  :config
  (setq js-doc-mail-address "your email address"
        js-doc-author (format "your name <%s>" js-doc-mail-address)
        js-doc-url "url of your website"
        js-doc-license "license name")
  :hook
  (js2-mode . (lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag))))

(with-eval-after-load 'compile
  ;; Compilation mode for ESLint
  ;; Copied from https://github.com/Fuco1/compile-eslint/blob/master/compile-eslint.el
  (defun compile-eslint--find-filename ()
    "Find the filename for current error."
    (save-match-data
      (save-excursion
        (when (re-search-backward (rx bol (group "/" (+ any)) eol))
          (list (match-string 1))))))

  (let ((form `(eslint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-eslint--find-filename
                2 3 2 1)))
    (if (assq 'eslint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))

  (push 'eslint compilation-error-regexp-alist))

;; Add eslint --fix
(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (ignore-errors
    (shell-command (concat "eslint --fix " (buffer-file-name)))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config
  (use-package mocha-snippets))

(provide 'init-javascript)
;;; init-javascript.el ends here
