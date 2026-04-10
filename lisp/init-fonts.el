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



(use-package mouse
  :ensure nil
  :init
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t)

  :bind
  (("<C-mouse-4>" . nil)
   ("<C-mouse-5>" . nil)
   ("<C-wheel-up>" . nil)
   ("<C-wheel-down>" . nil)))

(provide 'init-fonts)

;;; init-fonts.el ends here
