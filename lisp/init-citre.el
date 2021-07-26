;;; init-lsp.el --- Initialize Citre configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(when (executable-find "/usr/local/bin/readtags")
  (when (maybe-require-package 'citre)
    (require 'citre-config)
    (add-hook 'prog-mode-hook 'citre-auto-enable-citre-mode)
    (setq
     citre-readtags-program "/usr/local/bin/readtags"
     citre-ctags-program "/usr/local/bin/ctags"
     ;; Set this if you want to always use one location to create a tags file.
     citre-default-create-tags-file-location 'global-cache
     ;; See the "Create tags file" section above to know these options
     citre-use-project-root-when-creating-tags t
     citre-prompt-language-for-ctags-command t)
    (with-eval-after-load 'citre
      (defun citre-jump+ ()
        (interactive)
        (condition-case _
            (citre-jump)
          (error (call-interactively #'xref-find-definitions))))
      (global-set-key (kbd "C-x c j") 'citre-jump+)
      (global-set-key (kbd "C-x c k") 'citre-jump-back)
      (global-set-key (kbd "C-x c p") 'citre-ace-peek)
      (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

      (with-eval-after-load 'projectile
        (setq citre-project-root-function #'projectile-project-root))
      (with-eval-after-load 'cc-mode (require 'citre-lang-c))
      (with-eval-after-load 'dired (require 'citre-lang-fileref)))))

(provide 'init-citre)
;;; init-citre.el ends here
