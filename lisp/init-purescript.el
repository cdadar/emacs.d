;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package purescript-mode
  :hook
  ((purescript-mode . turn-on-purescript-indentation)
   (purescript-mode . (lambda ()
                        (add-hook 'before-save-hook 'purescript-sort-imports nil t)))
   (purescript-mode . (apply-partially 'prettify-symbols-mode -1)))
  :bind
  (:map purescript-mode-map ("C-o" . open-line)))

(use-package reformatter
  :after (purescript-mode)
  :config (reformatter-define purty
            :program "purty" :lighter " purty"))


(use-package psci
  :after (purescript-mode)
  :hook
  (purescript-mode . inferior-psci-mode))

(use-package add-node-modules-path
  :after (purescript-mode psci)
  :hook (purescript-mode . add-node-modules-path)
  :config (advice-add 'psci :around (lambda (oldfun &rest args)
                                      (let ((psci/purs-path (or (executable-find "purs")
                                                                psci/purs-path)))
                                        (apply oldfun args)))))

(provide 'init-purescript)
