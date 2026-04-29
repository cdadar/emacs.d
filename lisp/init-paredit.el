;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/maybe-map-paredit-newline ()
  "Bind RET to `paredit-newline' in suitable Paredit buffers."
  (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
              (minibufferp))
    (local-set-key (kbd "RET") #'paredit-newline)))

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun sanityinc/conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (when (memq this-command paredit-minibuffer-commands)
    (enable-paredit-mode)))

(defun cdadar/disable-puni-mode ()
  "Disable `puni-mode' in buffers using Paredit-based Lisp editing."
  (puni-mode -1))

(use-package paredit
  :diminish
  ;; Use paredit in the minibuffer
  ;; TODO: break out into separate package
  ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
  :hook ((paredit-mode . sanityinc/maybe-map-paredit-newline)
         (minibuffer-setup . sanityinc/conditionally-enable-paredit-mode))
  :bind (:map paredit-mode-map
              ;; Suppress certain paredit keybindings to avoid clashes, including
              ;; my global binding of M-?
              ("RET" . nil)
              ("C-<left>" . nil)
              ("C-<right>" . nil)
              ("C-M-<left>" . nil)
              ("C-M-<right>" . nil)
              ("M-s" . nil)
              ("M-?" . nil)
              ("M-<up>" . paredit-splice-sexp-killing-backward))
  :config
  (add-hook 'sanityinc/lispy-modes-hook #'enable-paredit-mode))

(use-package paredit-extension
  :ensure nil
  :after paredit
  :load-path "site-lisp/paredit-extension")

(use-package puni
  :bind (:map puni-mode-map
              ("M-(" . puni-wrap-round)
              ("C-(" . puni-slurp-backward)
              ("C-)" . puni-slurp-forward)
              ("C-}" . puni-barf-forward)
              ("C-{" . puni-barf-backward)
              ("M-(" . puni-splice-killing-backward)
              ("C-w" . nil))
  :config
  (add-hook 'sanityinc/lispy-modes-hook #'cdadar/disable-puni-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
