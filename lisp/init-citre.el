;;; init-citre.el --- Initialize Citre configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;
;;; Code:

(when (executable-find "/usr/local/bin/readtags")
  (use-package citre
    :diminish
    :bind (("C-x c j" . citre-jump+)
           ("C-x c J" . citre-jump-back)
           ("C-x c p" . citre-peek)
           ("C-x c a" . citre-ace-peek)
           ("C-x c u" . citre-update-this-tags-file))
    :init
    (require 'citre-config)
    (setq citre-auto-enable-citre-mode-modes '(prog-mode))

    (defun citre-jump+ ()
      "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))))

(provide 'init-citre)
;;; init-citre.el ends here
