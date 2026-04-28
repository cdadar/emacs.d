;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/desktop-time-restore (orig &rest args)
  "Measure how long `desktop-read' takes when calling ORIG with ARGS."
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))

(defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
  "Measure how long `desktop-create-buffer' takes via ORIG.
VER, FILENAME, and ARGS are forwarded unchanged."
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(use-package desktop
  :ensure nil
  :hook (after-init . desktop-save-mode)
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-auto-save-timeout 600)
  (desktop-globals-to-save
   '((comint-input-ring        . 50)
     (compile-history          . 30)
     desktop-missing-file-warning
     (dired-regexp-history     . 20)
     (extended-command-history . 30)
     (face-name-history        . 20)
     (file-name-history        . 100)
     (grep-find-history        . 30)
     (grep-history             . 30)
     (magit-revision-history   . 50)
     (minibuffer-history       . 50)
     (org-clock-history        . 50)
     (org-refile-history       . 50)
     (org-tags-history         . 50)
     (query-replace-history    . 60)
     (read-expression-history  . 60)
     (regexp-history           . 60)
     (regexp-search-ring       . 20)
     register-alist
     (search-ring              . 20)
     (shell-command-history    . 50)
     tags-file-name
     tags-table-list))
  :config
  (advice-add 'desktop-read :around #'sanityinc/desktop-time-restore)
  (advice-add 'desktop-create-buffer :around #'sanityinc/desktop-time-buffer-create))

;; Restore histories and registers after saving

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package session
  :custom
  (session-save-file (locate-user-emacs-file ".session"))
  (session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  :config
  (setq session-save-file-coding-system 'utf-8)
  :hook
  (after-init . session-initialize))


(provide 'init-sessions)
;;; init-sessions.el ends here
