;;; init-citre.el --- Initialize Citre configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(use-package citre
  :diminish
  :defer t
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; set this if you'd like to use ctags options generated by citre
   ;; directly, rather than further editing them.
   citre-edit-ctags-options-manually nil
   citre-auto-enable-citre-mode-modes '(prog-mode)))

(provide 'init-citre)
;;; init-citre.el ends here
