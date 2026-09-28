;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Misc config - yet to be placed in separate files

(use-package emacs
  :ensure nil
  :hook ((after-save . executable-make-buffer-file-executable-if-script-p)
         (after-save . sanityinc/set-mode-for-new-scripts))
  :custom
  (use-short-answers t))

(use-package goto-addr
  :ensure nil
  :init
  (require 'goto-addr)
  :hook ((prog-mode . goto-address-prog-mode)
         (conf-mode . goto-address-prog-mode))
  :custom
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


;;
;; Editing conveniences
;;

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; NOTE: no `easy-kill' here.  It binds [remap kill-ring-save], which
;; `whole-line-or-region-mode' (init-editing-utils.el) already owns, so the
;; binding would be unreachable.  Swap one for the other if you prefer
;; easy-kill: drop `whole-line-or-region' and add
;;   (use-package easy-kill :bind ([remap kill-ring-save] . easy-kill))

;; Jump back to where the last edit happened
(use-package goto-chg
  :bind (("C-," . goto-last-change)))

;; Copy&paste the GUI clipboard from a text terminal
(unless *win64*
  (use-package xclip
    :hook (after-init . xclip-mode)))

;; Browse devdocs.io documents with EWW
(use-package devdocs
  :commands (devdocs-install devdocs-lookup)
  :bind (("C-h D" . devdocs-dwim)))

;; A better *Help* buffer
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :hook (helpful-mode . cursor-sensor-mode))

;; Fontify calls to known functions in emacs-lisp buffers
(use-package highlight-defined
  :hook (emacs-lisp-mode inferior-emacs-lisp-mode))

;;
;; macOS integration
;;

(use-package reveal-in-folder
  :if *is-a-mac*
  :bind (("C-c C-r" . reveal-in-folder)))

(use-package osx-dictionary
  :if *is-a-mac*
  :bind (("C-c w" . osx-dictionary-search-word-at-point)))


(provide 'init-misc)
;;; init-misc.el ends here
