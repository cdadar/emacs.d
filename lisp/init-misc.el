;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Misc config - yet to be placed in separate files

(use-package emacs
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode)
         (conf-mode . goto-address-prog-mode)
         (after-save . executable-make-buffer-file-executable-if-script-p)
         (after-save . sanityinc/set-mode-for-new-scripts))
  :custom
  (use-short-answers t)
  (goto-address-mail-face 'link))

(use-package tcl
  :ensure nil
  :mode ("^Portfile\\'" . tcl-mode))

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))


(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

;; Handle the prompt pattern for the 1password command-line interface
(use-package comint
  :ensure nil
  :config
  (setq comint-password-prompt-regexp
        (concat
         comint-password-prompt-regexp
         "\\|^Please enter your password for user .*?:\\s *\\'")))

(use-package regex-tool
  :custom
  (regex-tool-backend 'perl))

(use-package re-builder
  :ensure nil
  :bind (:map reb-mode-map
              ("C-c C-k" . reb-quit)))

(use-package conf-mode
  :ensure nil
  :mode ("^Procfile\\'" . conf-mode))


(provide 'init-misc)
;;; init-misc.el ends here
