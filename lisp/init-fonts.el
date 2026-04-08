;;; init-fonts.el --- fonts conifg -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package cnfonts
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

(provide 'init-fonts)

;;; init-fonts.el ends here
