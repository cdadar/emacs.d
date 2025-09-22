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
   ("\\.\\(jst\\|ejs\\|rhtml\\)\\(\\.erb\\)?\\'"  . web-mode) ; ruby
   )
  :config
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
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
  (if emacs/>=30p
      (use-package instant-rename-tag
        :vc(:url"https://github.com/manateelazycat/instant-rename-tag" :rev :newest))
    (use-package instant-rename-tag
      :ensure nil
      :load-path "site-lisp/instant-rename-tag")))


(when (executable-find "prettier")
  (use-package reformatter
    :config
    (reformatter-define prettier-css
      :program "prettier"
      :args '("--parser=css"))
             ;;;###autoload (autoload 'prettier-html-on-save-mode "prettier-html" nil t)
    (reformatter-define prettier-html
      :program "prettier"
      :args '("--parser=html"))
    (add-hook 'web-mode-hook 'prettier-html-on-save-mode)))


(when (use-package ng2-mode
        :mode  (("*\\.{component|service|pipe|directive|guard|module}\\.ts\\'" . ng2-mode)
                ("*\\.component\\.html\\'" . ng2-mode))
        :config
        (when (functionp 'prettier-html-on-save-mode)
          (add-hook 'ng2-mode 'prettier-html-on-save-mode))))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
