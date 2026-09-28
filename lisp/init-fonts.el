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

;; Icon font used by the `nerd-icons' integrations (dired, ibuffer, corfu,
;; marginalia).  Installed with
;;   brew install --cask font-symbols-only-nerd-font
;; or `M-x nerd-icons-install-fonts'.  `cdadar/nerd-font-available-p'
;; (init-utils.el) keeps those integrations off until the font is there.
(use-package nerd-icons
  :commands (nerd-icons-install-fonts)
  :config
  (unless (cdadar/nerd-font-available-p)
    (message "Nerd Font symbols font is missing: run \
`M-x nerd-icons-install-fonts' or `brew install --cask font-symbols-only-nerd-font'")))

(use-package mouse
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  :bind
  (("<C-mouse-4>" . cdadar/disable-mouse-text-scaling)
   ("<C-mouse-5>" . cdadar/disable-mouse-text-scaling)
   ("<C-wheel-up>" . cdadar/disable-mouse-text-scaling)
   ("<C-wheel-down>" . cdadar/disable-mouse-text-scaling)
   ("<C-M-wheel-up>" . cdadar/disable-mouse-text-scaling)
   ("<C-M-wheel-down>" . cdadar/disable-mouse-text-scaling)))

(provide 'init-fonts)

;;; init-fonts.el ends here
