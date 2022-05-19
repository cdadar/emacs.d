;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)

(when (maybe-require-package 'typescript-mode)
  (when (maybe-require-package 'tide)
    (add-to-list 'auto-mode-alist '("*\\.d.ts\\'" . typescript-mode))
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (tide-hl-identifier-mode +1))
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (with-eval-after-load 'tide
      ;; formats the buffer before saving
      (add-hook 'before-save-hook 'tide-format-before-save))))

(maybe-require-package 'indium)

(maybe-require-package 'js-import)

;;; Basic js-mode setup

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(when (maybe-require-package 'rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("*\\.{components|pages}\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("src\\/routes\\/.*\\.js\\'" . rjsx-mode))

  (advice-add 'js-jsx-mode :override #'rjsx-mode))

(setq-default js-indent-level 2)


;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  (setq-local js2-mode-show-parse-errors t)
  (setq-local js2-mode-show-strict-warnings t)
  (when (derived-mode-p 'js-mode)
    (js2-minor-mode 1))

  (when (executable-find "eslint")
    (when (maybe-require-package 'flymake-eslint)
      (flymake-eslint-enable)))

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (js2-imenu-extras-setup))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

(with-eval-after-load 'js2-mode
  (sanityinc/major-mode-lighter 'js2-mode "JS2")
  (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))



(when (and (or (executable-find "rg") (executable-find "ag"))
           (maybe-require-package 'xref-js2))
  (when (executable-find "rg")
    (setq-default xref-js2-search-program 'rg))
  (defun sanityinc/enable-xref-js2 ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") nil)
    (add-hook 'js-mode-hook 'sanityinc/enable-xref-js2))
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook 'sanityinc/enable-xref-js2)))



;;; Coffeescript

(when (maybe-require-package 'coffee-mode)
  (with-eval-after-load 'coffee-mode
    (setq-default coffee-tab-width js-indent-level))

  (when (fboundp 'coffee-mode)
    (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))))



;; Run and interact with an inferior JS via js-comint.el

(when (maybe-require-package 'js-comint)
  (setq js-comint-program-command "node")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    :init-value nil :lighter " InfJS" :keymap inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))


;; Alternatively, use skewer-mode

(when (maybe-require-package 'skewer-mode)
  (with-eval-after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))



(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode rjsx-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(when (maybe-require-package 'js2-refactor)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(when (and (executable-find "prettier")
           (maybe-require-package 'reformatter))

;;;###autoload (autoload 'prettier-javascript-on-save-mode "prettier-javascript" nil t)
  (reformatter-define prettier-javascript
    :program "prettier"
    :args '("--parser=babel" "--arrow-parens=avoid"))

  (add-hook 'js2-mode-hook 'prettier-javascript-on-save-mode)
  (add-hook 'rjsx-mode-hook 'prettier-javascript-on-save-mode))

(when (maybe-require-package 'js-doc)
  (setq js-doc-mail-address "your email address"
        js-doc-author (format "your name <%s>" js-doc-mail-address)
        js-doc-url "url of your website"
        js-doc-license "license name")

  (add-hook 'js2-mode-hook
            #'(lambda ()
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
(when (maybe-require-package 'mocha)
  (maybe-require-package 'mocha-snippets))

(provide 'init-javascript)
;;; init-javascript.el ends here
