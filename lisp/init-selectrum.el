;;; init-selectrum.el --- Config for selectrum       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook 'selectrum-mode)

  (when (maybe-require-package 'selectrum-prescient)
    (require 'prescient)
    (prescient-persist-mode 1)
    (selectrum-prescient-mode 1)
    (global-set-key [remap execute-extended-command] 'execute-extended-command))

  (when (maybe-require-package 'embark)
    (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
    (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act-noexit))

  (when (maybe-require-package 'consult)
    (when (maybe-require-package 'projectile)
      (setq-default consult-project-root-function 'projectile-project-root))

    (when (executable-find "rg")
      (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
        (interactive (list prefix-arg (let ((s (symbol-at-point)))
                                        (when s (symbol-name s)))))
        (consult-ripgrep dir initial))
      (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point))
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
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
    (global-set-key (kbd "M-g e") 'consult-error)
    ;; M-s bindings (search-map)
    (global-set-key (kbd "M-s g") 'consult-git-grep)      ;; Alternatives: consult-grep, consult-ripgrep
    (global-set-key (kbd "M-s f") 'consult-find)          ;; Alternatives: consult-locate, find-fd
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

    ;; Optionally configure the register preview function. This gives a
    ;; consistent display for both `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
          register-preview-function #'consult-register-preview)

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

    (maybe-require-package 'consult-flycheck)))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(with-eval-after-load 'desktop
  ;; Try to prevent old minibuffer completion system being reactivated in
  ;; buffers restored via desktop.el
  (push (cons 'counsel-mode nil) desktop-minor-mode-table)
  (push (cons 'ivy-mode nil) desktop-minor-mode-table))

(provide 'init-selectrum)
;;; init-selectrum.el ends here
