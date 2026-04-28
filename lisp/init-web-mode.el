;;; init-web-mode.el --- web-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.cmp\\'" . web-mode)           ; salesforce
   ("\\.app\\'" . web-mode)           ; salesforce
   ("\\.page\\'" . web-mode)          ; salesforce
   ("\\.component\\'" . web-mode)     ; salesforce
   ("\\.wp\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.tmpl\\'" . web-mode)
   ("\\.module\\'" . web-mode)
   ("\\.inc\\'" . web-mode)
   ("\\.hbs\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[gj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.ftl\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.xul?\\'" . web-mode)
   ("\\.eex?\\'" . web-mode)
   ("\\.xml?\\'" . web-mode)
   ("\\.\\(jst\\|ejs\\|rhtml\\)\\(\\.erb\\)?\\'" . web-mode)) ; ruby
  :custom
  (web-mode-enable-auto-closing t)      ; enable auto close tag in text-mode
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-expanding t)
  :config
  ;; make org-mode export fail, I use evil and evil-matchit
  ;; to select text, so expand-region.el is not used
  (remove-hook 'web-mode-hook #'er/add-web-mode-expansions)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

(use-package instant-rename-tag
  :after web-mode
  :vc (:url "https://github.com/manateelazycat/instant-rename-tag" :rev :newest))

(use-package reformatter
  :if (executable-find "prettier")
  :demand t
  :config
  (reformatter-define prettier-css
    :program "prettier"
    :args '("--parser=css"))
  (reformatter-define prettier-html
    :program "prettier"
    :args '("--parser=html"))
  (add-hook 'web-mode-hook #'prettier-html-on-save-mode)
  (add-hook 'ng2-html-mode-hook #'prettier-html-on-save-mode))

(use-package ng2-mode
  :mode (("\\.\\(component\\|service\\|pipe\\|directive\\|guard\\|module\\)\\.ts\\'" . ng2-mode)
         ("\\.component\\.html\\'" . ng2-mode)))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
