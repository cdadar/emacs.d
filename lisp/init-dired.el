;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :bind (:map ctl-x-map
              (("C-j" . dired-jump))
         :map ctl-x-4-map
         (("C-j" . dired-jump-other-window))
         :map dired-mode-map
              (("e" . dired-open-externally)
               ([mouse-2] . dired-find-file)
               ("C-c C-q" . wdired-change-to-wdired-mode)))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-recursive-deletes 'top)
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-filename))
     arg)))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode)
  (require 'dired-x))

(use-package diff-hl
  :after dired
  :hook
  (dired-mode . diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
