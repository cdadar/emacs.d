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

;; (when (maybe-require-package 'purescript-mode)
;;   (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

;;   (add-hook 'purescript-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook 'purescript-sort-imports nil t)))

;;   (add-hook 'purescript-mode-hook (apply-partially 'prettify-symbols-mode -1))

;;   (with-eval-after-load 'purescript-mode
;;     (define-key purescript-mode-map (kbd "C-o") 'open-line))

;;   (when (maybe-require-package 'reformatter)
;;     (reformatter-define purty
;;       :program "purty" :lighter " purty"))

;;   (when (maybe-require-package 'psci)
;;     (add-hook 'purescript-mode-hook 'inferior-psci-mode))

;;   (when (maybe-require-package 'add-node-modules-path)
;;     (with-eval-after-load 'purescript-mode
;;       (add-hook 'purescript-mode-hook 'add-node-modules-path))
;;     (with-eval-after-load 'psci
;;       (advice-add 'psci :around (lambda (oldfun &rest args)
;;                                   (let ((psci/purs-path (or (executable-find "purs")
;;                                                             psci/purs-path)))
;;                                     (apply oldfun args)))))))

(provide 'init-purescript)
