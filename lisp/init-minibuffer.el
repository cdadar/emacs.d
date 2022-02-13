;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless))


  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  (add-hook 'minibuffer-setup-hook 'sanityinc/use-orderless-in-minibuffer)

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act)))

  (when (maybe-require-package 'consult)
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key (kbd "M-P"))))

    (defun consult-hide-lines ()
      (interactive)
      (consult-focus-lines nil (consult--completion-filter 'consult-location nil) "! "))

    (defun consult-reset-lines ()
      (interactive)
      (consult-focus-lines t))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

    (when (maybe-require-package 'projectile)
      (setq-default consult-project-root-function 'projectile-project-root))

    (when (and (executable-find "rg") (maybe-require-package 'affe))
      (defun sanityinc/affe-grep-at-point (&optional dir initial)
        (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                        (symbol-name s))))
        (affe-grep dir initial))
      (global-set-key (kbd "M-?") 'sanityinc/affe-grep-at-point)
      (sanityinc/no-consult-preview sanityinc/affe-grep-at-point)
      (with-eval-after-load 'affe (sanityinc/no-consult-preview affe-grep)))

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)



    (global-set-key (kbd "C-c h") 'consult-history)
    (global-set-key (kbd "C-c M-m") 'consult-mode-command)
    (global-set-key (kbd "C-c b") 'consult-bookmark)
    (global-set-key (kbd "C-c k") 'consult-kmacro)
    (global-set-key (kbd "C-x M-:") 'consult-complex-command)
    (global-set-key (kbd "C-x b") 'consult-buffer)
    ;; Custom M-# bindings for fast register access
    (global-set-key (kbd "M-#") 'consult-register-load)
    (global-set-key (kbd "M-'") 'consult-register-store)
    (global-set-key (kbd "C-M-#") 'consult-register)
    ;; M-g bindings (goto-map)
    (global-set-key (kbd "M-g g") 'consult-goto-line)
    (global-set-key (kbd "M-g o") 'consult-outline)
    (global-set-key (kbd "M-g m") 'consult-mark)
    (global-set-key (kbd "M-g k") 'consult-global-mark)
    (global-set-key (kbd "M-g i") 'consult-project-imenu) ;; Alternative: consult-imenu
    (global-set-key (kbd "M-g I") 'consult-imenu)
    (global-set-key (kbd "M-g e") 'consult-error)
    (global-set-key (kbd "M-g E") 'consult-compile-error)
    ;; M-s bindings (search-map)
    (global-set-key (kbd "M-s g") 'consult-git-grep)      ;; Alternatives: consult-grep, consult-ripgrep
    (global-set-key (kbd "M-s G") 'consult-grep)      ;; Alternatives: consult-grep, consult-ripgrep
    (global-set-key (kbd "M-s f") 'consult-find)          ;; Alternatives: consult-locate, find-fd
    (global-set-key (kbd "M-s F") 'find-fd)          ;; Alternatives: consult-locate, find-fd
    (global-set-key (kbd "M-s l") 'consult-line)
    (global-set-key (kbd "M-s m") 'consult-multi-occur)
    (global-set-key (kbd "M-s k") 'consult-keep-lines)
    (global-set-key (kbd "M-s u") 'consult-focus-lines)
    (global-set-key (kbd "M-s s") 'consult-isearch)
    ;; Other bindings
    (global-set-key (kbd "M-y") 'consult-yank-pop)
    (global-set-key (kbd "<help> a") 'consult-apropos)

    ;; Custom command wrappers. It is generally encouraged to write your own
    ;; commands based on the Consult commands. Some commands have arguments which
    ;; allow tweaking. Furthermore global configuration variables can be set
    ;; locally in a let-binding.
    (defun find-fd (&optional dir initial)
      (interactive "P")
      (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
        (consult-find dir initial)))

    (require 'consult-xref)
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    (with-eval-after-load 'flycheck
      (when (maybe-require-package 'consult-flycheck)
        (global-set-key (kbd "M-s e") 'consult-flycheck)))

    ;; Optionally tweak the register preview window.
    ;; * Sort the registers
    ;; * Hide the mode line
    ;; * Resize the window, such that the contents fit exactly
    (advice-add #'register-preview :around
                (lambda (fun buffer &optional show-empty)
                  (let ((register-alist (seq-sort #'car-less-than-car register-alist)))
                    (funcall fun buffer show-empty))
                  (when-let (win (get-buffer-window buffer))
                    (with-selected-window win
                      (setq-local mode-line-format nil)
                      (setq-local window-min-height 1)
                      (fit-window-to-buffer)))))

    (when (maybe-require-package 'embark-consult)
      (with-eval-after-load 'embark
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

    (maybe-require-package 'consult-flycheck))

  ;; (with-eval-after-load 'company
  ;;   (when (maybe-require-package 'consult-company)
  ;;     (define-key company-mode-map [remap completion-at-point] #'consult-company)))

  (with-eval-after-load 'yasnippet
    (when (maybe-require-package 'consult-yasnippet)
      (global-set-key (kbd "M-Y") 'consult-yasnippet)))

  ;; (when (maybe-require-package 'consult-dir)
  ;;   (define-key global-map (kbd "C-x C-d") #'consult-dir)
  ;;   (define-key minibuffer-local-completion-map (kbd "C-x C-d") #'consult-dir)
  ;;   (define-key minibuffer-local-completion-map (kbd "C-x C-j") #'consult-dir-jump-file))
  )

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))




(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
