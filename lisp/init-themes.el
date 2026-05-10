;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package emacs
  :ensure nil
  :custom
  ;; Add all your customizations prior to loading the themes
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-region '(bg-only no-extend))
  :config
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi)
  :bind
  (("<f5>" . modus-themes-toggle)))



(defun sanityinc/dimmer-refresh-after-background-mode-change (&rest _)
  "Refresh dimmer after frame background mode changes."
  (dimmer-process-all))

(use-package dimmer
  :hook
  ((after-init . dimmer-mode))
  :custom
  (dimmer-fraction 0.15)
  :config
  ;; TODO: file upstream as a PR
  (advice-add 'frame-set-background-mode :after #'sanityinc/dimmer-refresh-after-background-mode-change)

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
  ((after-init . minions-mode)))

(provide 'init-themes)
;;; init-themes.el ends here
