 ;;; personal.el --- Emacs Prelude: Personal settings
;;
;;; Commentary:
;; Personal settings to augment those of Prelude
;; Install Emacs through homebrew with --cocoa --srgb

;;; Code:

;;; make cursor style bar
(setq-default cursor-type 'box)

(defun cdadar/reset-frame-size (&optional frame)
  "重设窗体大小"
  (interactive)
  (when frame
    (select-frame frame))
  (progn (set-frame-width (selected-frame) 120)
         (set-frame-height (selected-frame) 50)))

(add-hook 'after-make-frame-functions 'cdadar/reset-frame-size)


(when (maybe-require-package 'cnfonts)
  ;; 让 cnfonts 随着 Emacs 自动生效。
  (cnfonts-enable)

  (add-hook 'after-make-frame-functions 'cnfonts-set-font-with-saved-step)
  (add-hook 'window-setup-hook 'cnfonts-set-font-with-saved-step)
  ;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
  (cnfonts-set-spacemacs-fallback-fonts))

(provide 'init-personal)
;;; personal.el ends here
