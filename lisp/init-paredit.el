;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :diminish
  :config
  (defun sanityinc/maybe-map-paredit-newline ()
    (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))

  (add-hook 'paredit-mode-hook 'sanityinc/maybe-map-paredit-newline)

  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("RET" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp-killing-backward)

  ;; Use paredit in the minibuffer
  ;; TODO: break out into separate package
  ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
  (add-hook 'minibuffer-setup-hook 'sanityinc/conditionally-enable-paredit-mode)

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

  (add-hook 'sanityinc/lispy-modes-hook 'enable-paredit-mode)

  (use-package paredit-extension
    :ensure nil
    :load-path "site-lisp/paredit-extension"))


(use-package puni
  :config
  (add-hook 'sanityinc/lispy-modes-hook (lambda () (puni-mode -1)))
  :bind
  (:map puni-mode-map
        (("M-(" . puni-wrap-round )
         ("C-(" . puni-slurp-backward )
         ("C-)" . puni-slurp-forward )
         ("C-}" . puni-barf-forward )
         ("C-{" . puni-barf-backward )
         ("M-(" . puni-splice-killing-backward )
         ("C-w" . nil))))

(provide 'init-paredit)
;;; init-paredit.el ends here
