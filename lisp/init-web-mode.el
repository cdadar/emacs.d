;;; init-web-mode.el --- web-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . web-mode)) ; salesforce
(add-to-list 'auto-mode-alist '("\\.app\\'" . web-mode)) ; salesforce
(add-to-list 'auto-mode-alist '("\\.page\\'" . web-mode)) ; salesforce
(add-to-list 'auto-mode-alist '("\\.component\\'" . web-mode)) ; salesforce
(add-to-list 'auto-mode-alist '("\\.wp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xul?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(jst\\|ejs\\|rhtml\\)\\(\\.erb\\)?\\'"  . web-mode)) ; ruby

(with-eval-after-load 'web-mode
  '(progn
     ;; make org-mode export fail, I use evil and evil-matchit
     ;; to select text, so expand-region.el is not used

     (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
     (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
     (setq web-mode-enable-auto-pairing t)
     (setq web-mode-enable-css-colorization t)
     (setq web-mode-markup-indent-offset 2)
     (setq web-mode-code-indent-offset 2)
     (setq web-mode-css-indent-offset 2)
     (setq web-mode-enable-auto-expanding t)
     (setq web-mode-imenu-regexp-list
           '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
             ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
             ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
             ;; angular imenu
             (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))))

(when (and (executable-find "prettier")
           (maybe-require-package 'reformatter))
  (reformatter-define prettier-css
    :program "prettier"
    :args '("--parser=css"))

;;;###autoload (autoload 'prettier-html-on-save-mode "prettier-html" nil t)
  (reformatter-define prettier-html
    :program "prettier"
    :args '("--parser=html"))

  (add-hook 'web-mode-hook 'prettier-html-on-save-mode))

(when (maybe-require-package 'ng2-mode)
  (add-to-list 'auto-mode-alist '("*\\.{component|service|pipe|directive|guard|module}\\.ts\\'" . ng2-mode))
  (add-to-list 'auto-mode-alist '("*\\.component\\.html\\'" . ng2-mode))
  (with-eval-after-load 'ng2-mode
    (when (functionp 'prettier-html-on-save-mode)
      (add-hook 'ng2-mode 'prettier-html-on-save-mode))))

(with-eval-after-load 'web-mode
  (require 'instant-rename-tag))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
