;;; init-anki.el --- Anki integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; anki-connect: low-level AnkiConnect API client, depended on by
;; anki-editor, org-anki, and anki-helper.
(use-package anki-connect)

;; anki-editor: synchronize Org headings with Anki notes.
(use-package anki-editor
  :custom
  (anki-editor-org-tags-as-anki-tags t)
  (anki-editor-create-decks t)
  :config

  (defvar cdadar/anki-editor-cloze-number 1
    "Current Anki cloze number for helper commands.")

  (defun cdadar/anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1."
    (interactive)
    (setq cdadar/anki-editor-cloze-number (or arg 1)))

  (defun cdadar/anki-editor-cloze-region-auto-incr ()
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region cdadar/anki-editor-cloze-number "")
    (setq cdadar/anki-editor-cloze-number (1+ cdadar/anki-editor-cloze-number))
    (forward-sexp))

  (defun cdadar/anki-editor-cloze-region-dont-incr ()
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- cdadar/anki-editor-cloze-number) "")
    (forward-sexp))

  (defun cdadar/anki-editor-push-tree ()
    "Push all notes under current tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (cdadar/anki-editor-reset-cloze-number))

  ;; Initialize cloze counter.
  (cdadar/anki-editor-reset-cloze-number))

;; anki-mode: major mode for editing Anki cards.
(use-package anki-mode
  :commands (anki-mode
             anki-mode-new-card
             anki-mode-cloze-region))

;; org-anki: alternative org-to-Anki synchronizer.
(use-package org-anki
  :commands (org-anki-sync-entry
             org-anki-sync-all
             org-anki-import-deck))

;; anki-helper: additional Anki workflow utilities.
(use-package anki-helper
  :vc (:url "https://github.com/Elilif/emacs-anki-helper" :rev :newest)
  :commands (anki-helper-entry-sync
             anki-helper-entry-sync-all
             anki-helper-sync))

(provide 'init-anki)
;;; init-anki.el ends here
