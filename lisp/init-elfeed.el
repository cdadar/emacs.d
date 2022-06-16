;;; init-elfeed.el --- elfeed plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package elfeed
  :bind
  (("C-x w" . elfeed)
   :map elfeed-search-mode-map
   ("q" . bjm/elfeed-save-db-and-bury))
  :config
  (progn
    (setq elfeed-use-curl 't)
    (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
    ;;functions to support syncing .elfeed between machines
    ;;makes sure elfeed reads index from disk before launching
    (defun bjm/elfeed-load-db-and-open ()
      "Wrapper to load the elfeed db from disk before opening"
      (interactive)
      (elfeed-db-load)
      (elfeed)
      (elfeed-search-update--force))

    ;;write to disk when quiting
    (defun bjm/elfeed-save-db-and-bury ()
      "Wrapper to save the elfeed db to disk before burying buffer"
      (interactive)
      (elfeed-db-save)
      (quit-window))
    (use-package elfeed-org
      :config
      (elfeed-org))))




(provide 'init-elfeed)
