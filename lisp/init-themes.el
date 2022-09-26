;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi)  ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))



(use-package dimmer
  :hook
  (after-init . dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)
  ;; TODO: file upstream as a PR
  (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))

  ;; Don't dim in terminal windows. Even with 256 colours it can
  ;; lead to poor contrast.  Better would be to vary dimmer-fraction
  ;; according to frame type.
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))

(use-package minions
  :bind
  (([S-down-mouse-3] . minions-minor-modes-menu))
  :hook
  (after-init . minions-mode))

(provide 'init-themes)
;;; init-themes.el ends here
