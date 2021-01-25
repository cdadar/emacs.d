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
      (global-set-key (kbd "M-?") 'consult-ripgrep))
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

    (when (maybe-require-package 'embark-consult)
      (with-eval-after-load 'embark
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode))))

  (maybe-require-package 'consult-flycheck)

  (when (maybe-require-package 'marginalia)
    (add-hook 'after-init-hook 'marginalia-mode)
    (setq-default marginalia-annotators '(marginalia-annotators-heavy))))

(with-eval-after-load 'desktop
  ;; Try to prevent old minibuffer completion system being reactivated in
  ;; buffers restored via desktop.el
  (push (cons 'counsel-mode nil) desktop-minor-mode-table)
  (push (cons 'ivy-mode nil) desktop-minor-mode-table))

(provide 'init-selectrum)
;;; init-selectrum.el ends here
