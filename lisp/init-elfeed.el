;;; init-elfeed.el --- elfeed plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(when (maybe-require-package 'elfeed)
  (global-set-key (kbd "C-x w") 'elfeed)
  (after-load 'elfeed
    (define-key elfeed-search-mode-map "q" 'bjm/elfeed-save-db-and-bury))

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

  (after-load 'org
    (when (maybe-require-package 'elfeed-org)
      (elfeed-org))))

(provide 'init-elfeed)
