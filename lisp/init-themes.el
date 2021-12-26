;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'modus-themes)

;; Toggle between light and dark
(with-eval-after-load 'modus-themes
  (modus-themes-load-themes))

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))


(when (maybe-require-package 'minions)
  (minions-mode 1)
  (with-eval-after-load 'minions-mode
    (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)))

(provide 'init-themes)
;;; init-themes.el ends here
