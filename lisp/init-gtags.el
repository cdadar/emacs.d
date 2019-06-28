;;; init-ggtags.el --- ggtags  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


(when (maybe-require-package 'counsel-gtags)
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (add-hook 'php-mode-hook 'counsel-gtags-mode)
  (after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s ,") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
    ;; (add-hook 'after-save-hook 'counsel-gtags-update-tags)
    ))

(when (maybe-require-package 'ggtags)
  (after-load 'ggtags
    '(progn
       (define-key ggtags-navigation-map (kbd "M-<") nil)
       (define-key ggtags-navigation-map (kbd "M->") nil)
       ;; (global-set-key (kbd "M-.") 'ggtags-find-tag-dwim)
       ;; (global-set-key (kbd "M-?") 'ggtags-grep)
       )))

(when (maybe-require-package 'gxref)
  (after-load 'gxref
    '(progn
       (add-to-list 'xref-backend-functions 'gxref-xref-backend))))

(provide 'init-gtags)
