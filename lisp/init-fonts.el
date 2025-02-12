;;; init-fonts.el --- fonts conifg -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package cnfonts
  :config
  (cnfonts-mode 1)
  :hook
  ((after-init . cnfonts-reset-fontsize)))

(provide 'init-fonts)

;;; init-fonts.el ends here
