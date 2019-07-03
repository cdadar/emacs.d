;;; init-ggtags.el --- ggtags  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-




(when (maybe-require-package 'ggtags)
  (add-hook 'after-init-hook #'ggtags-mode)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'php-mode)
                (ggtags-mode 1))))
  (after-load 'diminish
    (diminish 'ggtags-mode))
  (after-load 'ggtags
    '(progn
       (define-key ggtags-navigation-map (kbd "M-<") nil)
       (define-key ggtags-navigation-map (kbd "M->") nil)
       (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
       (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
       (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
       (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
       (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
       (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
       (define-key ggtags-mode-map (kbd "C-c g a") 'counsel-gtags-find-definition)
       (define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag-dwim)
       (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
       (define-key ggtags-mode-map (kbd "C-c <") 'ggtags-prev-mark)
       (define-key ggtags-mode-map (kbd "C-c >") 'ggtags-next-mark)))
  (push 'company-gtags company-backends))

(when (maybe-require-package 'gxref)
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))



(provide 'init-gtags)
