;;; init-lsp.el --- Initialize Citre configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(when (executable-find "/usr/local/bin/readtags")
  (when (maybe-require-package 'citre)
    (autoload 'citre-update-tags-file "citre")
    (autoload 'citre-update-this-tags-file "citre")
    (autoload 'citre-edit-tags-file-recipe "citre")
    (autoload 'citre-create-tags-file "citre")
    (autoload 'citre-jump "citre")
    (autoload 'citre-mode "citre")
    (autoload 'citre-peek "citre")
    (autoload 'citre-ace-peek "citre")
    (autoload 'citre-auto-enable-citre-mode "citre")
    (setq
     citre-readtags-program "/usr/local/bin/readtags"
     citre-ctags-program "/usr/local/bin/ctags"
     ;; Set this if you want to always use one location to create a tags file.
     citre-default-create-tags-file-location 'global-cache
     ;; See the "Create tags file" section above to know these options
     citre-use-project-root-when-creating-tags t
     citre-prompt-language-for-ctags-command t)
    (defun citre-jump+ ()
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))
    (global-set-key (kbd "C-x c j") 'citre-jump+)
    (global-set-key (kbd "C-x c k") 'citre-jump-back)
    (global-set-key (kbd "C-x c p") 'citre-ace-peek)
    (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

    (with-eval-after-load 'citre
      (with-eval-after-load 'project
        (setq citre-project-root-function #'project-root))
      (with-eval-after-load 'cc-mode (require 'citre-lang-c))
      (with-eval-after-load 'dired (require 'citre-lang-fileref))
      (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog)))))

(provide 'init-citre)
;;; init-citre.el ends here
