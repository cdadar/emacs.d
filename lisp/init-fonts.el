;;; init-fonts.el --- fonts conifg -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package cnfonts
  :if (display-graphic-p)
  :hook
  ((after-init . cnfonts-reset-fontsize))
  :bind
  (:map cnfonts-mode-map
        ("C-<mouse-5>" . nil)
        ("C-<mouse-4>" . nil)
        ("C-<wheel-down>" . nil)
        ("C-<wheel-up>" . nil))
  :config
  (cnfonts-mode 1))

(defun cdadar/disable-mouse-text-scaling (&optional _arg)
  "Disable mouse or touchpad gestures that try to scale text."
  (interactive "P")
  (user-error "Mouse wheel text scaling is disabled in this configuration"))

(use-package mouse
  :ensure nil
  :init
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t)

  :bind
  (("<C-mouse-4>" . cdadar/disable-mouse-text-scaling)
   ("<C-mouse-5>" . cdadar/disable-mouse-text-scaling)
   ("<C-wheel-up>" . cdadar/disable-mouse-text-scaling)
   ("<C-wheel-down>" . cdadar/disable-mouse-text-scaling)))

(provide 'init-fonts)

;;; init-fonts.el ends here
