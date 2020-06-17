(require-package 'treemacs)
(with-eval-after-load 'treemacs
  (setq treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-collapse-dirs              3
        treemacs-silent-refresh             nil
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (define-key global-map [f8] 'treemacs-toggle)
  (define-key global-map (kbd "M-0") 'treemacs-select-window)
  (define-key global-map (kbd "C-c 1") 'treemacs-delete-other-windows)
  (define-key global-map (kbd "M-m") nil)
  (define-key global-map (kbd "M-m ft") 'treemacs-toggle)
  (define-key global-map (kbd "M-m fT") 'treemacs)
  (define-key global-map (kbd "M-m f C-t") 'treemacs-find-file)

  (when (maybe-require-package 'projectile)
    (require-package 'treemacs-projectile)
    (setq treemacs-header-function #'treemacs-projectile-create-header)
    (define-key global-map (kbd "M-m fP") 'treemacs-projectile)
    (define-key global-map (kbd "M-m fp") 'treemacs-projectile-toggle)
    )
  )



(provide 'init-treemacs)
