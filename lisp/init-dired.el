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
               ("C-c C-q" . wdired-change-to-wdired-mode)
               ("S" . dired-quick-sort)
               (")" . dired-git-info-mode)
               ("C-c C-r" . dired-rsync)))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
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

;; Dired extras used by the bindings in `dired' above.
;;
;; NOTE: the keybindings must stay in the `dired' form.  `use-package' wraps
;; a `:bind (:map MAP ...)' in `(if (boundp 'MAP) ... (eval-after-load PKG ...))'
;; when the package is deferred, and here `dired-mode-map' is still unbound
;; while this file loads -- so a binding declared in these forms would wait
;; for `dired-quick-sort' and friends to load, which never happens by
;; itself.  `eval-after-load dired' (what the `dired' form gets) does fire.
(use-package dired-quick-sort
  :commands dired-quick-sort)

(use-package dired-git-info
  :commands dired-git-info-mode)

(use-package dired-rsync
  :commands dired-rsync)

;; Show file icons
(use-package nerd-icons-dired
  :if (cdadar/nerd-font-available-p)
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
