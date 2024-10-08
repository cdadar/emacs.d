;;; init-ocaml.el --- Support the OCaml language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tuareg
  :config
  (defvar-local tuareg-previous-tuareg-buffer nil
    "Buffer from which we jumped to the REPL.")

  (defun sanityinc/tuareg-repl-switch ()
    (interactive)
    (let ((last-tuareg-buf (when (derived-mode-p 'tuareg-mode)
                             (current-buffer))))
      (tuareg-run-ocaml)
      (pop-to-buffer tuareg-interactive-buffer-name)
      (when last-tuareg-buf
        (setq-local tuareg-previous-tuareg-buffer last-tuareg-buf))))

  (defun sanityinc/tuareg-repl-switch-back ()
    (interactive)
    (when tuareg-previous-tuareg-buffer
      (pop-to-buffer tuareg-previous-tuareg-buffer)))
  :bind
  (
   :map tuareg-mode-map
   ("C-c C-z" . sanityinc/tuareg-repl-switch)
   :map tuareg-interactive-mode-map
   ("C-c C-z" . sanityinc/tuareg-repl-switch-back)))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (use-package ocaml-ts-mode
  :after (eglot)
  :config
  (add-to-list 'eglot-server-programs '(((ocaml-ts-mode :language-id "ocaml")) "ocamllsp"))))


(use-package dune
  :config
  (use-package dune-format))

;; Add my own lightweight ocp-indent reformatter, instead of the clunky upstream package
(use-package reformatter
  :config
  (defcustom ocp-indent-args nil
    "Arguments for \"ocp-indent\" invocation.")
  (reformatter-define ocp-indent
    :program "ocp-indent"
    :args ocp-indent-args
    :lighter " OCP"))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
