;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'typescript-mode)

(maybe-require-package 'indium)


;;; Basic js-mode setup

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(when (maybe-require-package 'rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("src\\/routes\\/.*\\.js\\'" . rjsx-mode))
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)
    (define-key rjsx-mode-map ">" nil)))

(setq-default js-indent-level 2)

(when (maybe-require-package 'vue-mode)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (with-eval-after-load 'vue-mode))

(when (maybe-require-package 'ng2-mode)
  (add-to-list 'auto-mode-alist '("*\\.{component|service|pipe|directive|guard|module}\\.ts\\'" . ng2-mode))
  (add-to-list 'auto-mode-alist '("*\\.component\\.html\\'" . ng2-mode))
  (with-eval-after-load 'ng2-mode))

;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))
  (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (when (maybe-require-package 'ac-js2)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'ac-js2-company)))


  (js2-imenu-extras-setup))

;; In Emacs >= 25, the following is an alias for js-indent-level anyway
(setq-default js2-basic-offset 2)

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
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(when (maybe-require-package 'js2-refactor)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(when (and (executable-find "prettier")
           (maybe-require-package 'reformatter))

  (reformatter-define prettier-javascript
    :program "prettier"
    :args '("--parser=babel"))

  (add-hook 'js2-mode-hook 'prettier-javascript-on-save-mode)
  (add-hook 'rjsx-mode-hook 'prettier-javascript-on-save-mode))



;; (when (and (executable-find "tern")
;;            (maybe-require-package 'tern))
;;   (add-hook 'js2-mode-hook 'tern-mode)
;;   (add-hook 'rjsx-mode-hook 'tern-mode)
;;   (add-hook 'web-mode-hook 'tern-mode)
;;   (with-eval-after-load 'tern
;;     ;; Disable completion keybindings, as we use xref-js2 instead
;;     (define-key tern-mode-keymap (kbd "M-.") nil)
;;     (define-key tern-mode-keymap (kbd "M-,") nil)
;;     (when (maybe-require-package 'company-tern)
;;       (add-to-list 'company-backends 'company-tern))))



(when (maybe-require-package 'js-doc)
  (setq js-doc-mail-address "your email address"
        js-doc-author (format "your name <%s>" js-doc-mail-address)
        js-doc-url "url of your website"
        js-doc-license "license name")

  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag))))


;; Add NodeJS error format
(with-eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (list 'node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                        1 ;; file
                        2 ;; line
                        3 ;; column
                        ))
     (add-to-list 'compilation-error-regexp-alist 'node)

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
         (push form compilation-error-regexp-alist-alist)))))


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

(provide 'init-javascript)
;;; init-javascript.el ends here
