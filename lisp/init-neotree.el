(when (maybe-require-package 'neotree)
  (setq neo-smart-open t)
  ;; neotree icons setting
  ;; (when (maybe-require-package 'all-the-icons)
  ;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

  (when (maybe-require-package 'projectile)
    (with-eval-after-load 'projectile
      (defun neotree-project-dir ()
        "Open NeoTree using the git root."
        (interactive)
        (let ((project-dir (projectile-project-root))
              (file-name (buffer-file-name)))
          (neotree-toggle)
          (if project-dir
              (if (neo-global--window-exists-p)
                  (progn
                    (neotree-dir project-dir)
                    (neotree-find file-name)))
            (message "Could not find git project root."))))
      (global-set-key [f8] 'neotree-project-dir))))

(provide 'init-neotree)
