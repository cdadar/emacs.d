;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :hook
  ((css-mode html-mode sass-mode) . rainbow-mode))


;;; Embedding in html
(use-package mmm-mode
  :config
  (with-eval-after-load 'mmm-vars
    (mmm-add-group
     'html-css
     '((css-cdata
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t]*\n?"
        :back "[ \t]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css-inline
        :submode css-mode
        :face mmm-code-submode-face
        :front "style=\""
        :back "\"")))
    (dolist (mode (list 'html-mode 'nxml-mode))
      (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))


;;; SASS and SCSS
(use-package sass-mode)
(use-package scss-mode
  :config
  (setq-default scss-compile-at-save nil)
  )


;;; LESS
(use-package less-css-mode
  :config
  (use-package skewer-less
    :hook (less-css-mode . skewer-less-mode)))


;; Skewer CSS
(use-package skewer-mode
  :hook (css-mode . skewer-css-mode))


;;; Use eldoc for syntax hints
(use-package css-eldoc
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc")
  :hook
  (css-mode . turn-on-css-eldoc))


(provide 'init-css)
;;; init-css.el ends here
