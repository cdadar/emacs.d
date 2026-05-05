;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :hook
  ((css-mode html-mode sass-mode scss-mode less-css-mode) . rainbow-mode))


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
  :init
  ;; `scss-mode' still uses legacy Flymake variables while current Flymake
  ;; no longer defines them before old-style backends are loaded.
  (defvar flymake-allowed-file-name-masks nil)
  (defvar flymake-err-line-patterns nil)
  :custom
  (scss-compile-at-save nil))


;;; LESS
(use-package less-css-mode
  :ensure nil)


;;; Use eldoc for syntax hints
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook
  (css-mode . turn-on-css-eldoc))


(provide 'init-css)
;;; init-css.el ends here
