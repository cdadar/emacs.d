;;; init-elfeed.el --- elfeed plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package elfeed
  :commands (elfeed-org)
  :bind
  (("C-x w" . (lambda () (interactive)  (progn (elfeed-org) (elfeed))))
   :map elfeed-search-mode-map
   ("q" . bjm/elfeed-save-db-and-bury))
  :init
  (setq elfeed-use-curl 't)
  :config
  (progn
    (use-package elfeed-org
      :init
      (progn
        (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
        (setq rmh-elfeed-org-files (list rmh-elfeed-org-file)))
      :config
      (elfeed-org))


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
      (quit-window))))



(provide 'init-elfeed)
