;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Misc config - yet to be placed in separate files

(add-auto-mode 'tcl-mode "^Portfile\\'")

(setq use-short-answers t)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'conf-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

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
  :config
  (setq-default regex-tool-backend 'perl))

(use-package re-builder
  :ensure nil
  :bind (:map reb-mode-map
              ("C-c C-k" . reb-quit)))

(add-auto-mode 'conf-mode "^Procfile\\'")


(provide 'init-misc)
;;; init-misc.el ends here
