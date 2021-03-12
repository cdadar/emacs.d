 ;;; personal.el --- Emacs Prelude: Personal settings
;;
;;; Commentary:
;; Personal settings to augment those of Prelude
;; Install Emacs through homebrew with --cocoa --srgb

;;; Code:

;;; make cursor style bar
(setq-default cursor-type 'box)

(when (maybe-require-package 'cnfonts)
  ;; 让 cnfonts 随着 Emacs 自动生效。
  (cnfonts-enable)

  (add-hook 'after-make-frame-functions 'cnfonts-set-font-with-saved-step)
  (add-hook 'window-setup-hook 'cnfonts-set-font-with-saved-step)
  ;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
  (cnfonts-set-spacemacs-fallback-fonts))

(provide 'init-personal)
;;; personal.el ends here
