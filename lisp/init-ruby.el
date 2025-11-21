;;; init-ruby.el --- Support for the Ruby language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(use-package ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'"
               "\\.gemspec\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'subword-mode)

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'ruby-mode))

(use-package rspec-mode)


(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(use-package inf-ruby
  :config
  (defun sanityinc/ruby-load-file (&optional choose-file)
    (interactive "P")
    (if (or choose-file (not buffer-file-name))
        (call-interactively 'ruby-load-file)
      (save-some-buffers)
      (ruby-load-file buffer-file-name)))
  (define-key inf-ruby-minor-mode-map [remap ruby-load-file] 'sanityinc/ruby-load-file))


;;; Ruby compilation

(with-eval-after-load 'ruby-mode
  (use-package ruby-compilation
    :bind
    (:map ruby-mode-map
          ([S-f7] . ruby-compilation-this-buffer)
          ([f7] . ruby-compilation-this-test))
    :config
    (defalias 'rake 'ruby-compilation-rake)))


;;; Robe
(with-eval-after-load 'ruby-mode
  (use-package robe
    :hook
    (ruby-mode . robe-mode)))


;;; ri support
(use-package yari
  :config
  (defalias 'ri 'yari))


(use-package yard-mode
  :hook
  (ruby-mode . yard-mode))



;;; ERB
(use-package mmm-mode
  :mode  ("\\.jst\\.ejs\\'"  . html-erb-mode)
  :config
  (require 'derived)

  (defun sanityinc/set-up-mode-for-erb (mode)
    (add-hook (derived-mode-hook-name mode) (lambda () (require 'mmm-erb)))
    (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

  (dolist (mode '(html-mode html-erb-mode nxml-mode))
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))

  (mapc 'sanityinc/set-up-mode-for-erb
        '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

  (add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")

  (mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
  (sanityinc/set-up-mode-for-erb 'yaml-mode)

  (dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
    (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb)))

(provide 'init-ruby)
;;; init-ruby.el ends here
