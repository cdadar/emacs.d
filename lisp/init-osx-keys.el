;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :if *is-a-mac*
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'none)
  (mouse-wheel-scroll-amount '(1
                                ((shift) . 5)
                                ((control))))
  :init
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  :bind
  (("M-`" . ns-next-frame)
   ("M-h" . ns-do-hide-emacs)
   ("M-˙" . ns-do-hide-others)
   ("M-ˍ" . ns-do-hide-others))
  :config
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil)))

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
