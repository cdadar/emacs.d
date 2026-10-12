;;; init-citar.el --- citar -*- lexical-binding: t -*-

;; Copyright (C) 2024 chens
;;
;; Version: 0.0.1
;; Keywords: citar
;; Author: chens
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;;; Code:

(require 'oc)
(require 'parsebib)
(require 'subr-x)

(defun cdadar/bibtex-keys-in (text)
  "Return the list of entry keys of the BibTeX entries in TEXT."
  (with-temp-buffer
    (insert text)
    (hash-table-keys (car (parsebib-parse-bib-buffer)))))

(defun cdadar/bibtex-key-found-in (key files)
  "Return the first file in FILES that defines the BibTeX KEY, or nil."
  (seq-find (lambda (file)
              (when (file-readable-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (gethash key (car (parsebib-parse-bib-buffer))))))
            files))

(defun cdadar/bibtex-append-entry (file entry)
  "Append BibTeX ENTRY to FILE, separated from the previous entry by a blank line."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (when (and (> (point) (point-min)) (not (looking-back "\n\n" 2)))
      (insert "\n"))
    (insert entry "\n")
    (save-buffer)))

(defun cdadar/bibtex-to-org-cite ()
  "Turn a BibTeX entry into an org-cite citation for it.

The entry is read from the active region, or from the head of the kill
ring when the region is inactive, in which case the citation is
inserted at point instead of replacing the region.  When the entry's
key is not in the buffer's bibliography yet, the entry is appended to
the first bibliography file so that the citation resolves."
  (interactive)
  (let* ((region (and (use-region-p) (cons (region-beginning) (region-end))))
         (where (if region "the region" "the kill ring"))
         (entry (string-trim
                 (if region
                     (buffer-substring-no-properties (car region) (cdr region))
                   (or (car kill-ring) ""))))
         (keys (cdadar/bibtex-keys-in entry)))
    (unless keys
      (user-error "No BibTeX entry found in %s" where))
    (when (cdr keys)
      (user-error "%d BibTeX entries in %s; convert them one at a time"
                  (length keys) where))
    (let* ((key (car keys))
           (files (org-cite-list-bibliography-files))
           (target (car files))
           (found (cdadar/bibtex-key-found-in key files)))
      (cond (found (message "%s is already in %s" key found))
            ((not target) (user-error "No bibliography file to add %s to" key))
            (t (cdadar/bibtex-append-entry target entry)
               (message "Added %s to %s" key target)))
      (goto-char (if region (car region) (point)))
      (when region (delete-region (car region) (cdr region)))
      (insert (format "[cite:@%s]" key)))))

(use-package org
  :ensure nil
  ;; C-c b is a prefix: a key cannot be both a command and a prefix, and
  ;; keeping org-cite-insert on the bare C-c b would forbid C-c b b.
  :bind (:map org-mode-map
              ("C-c b i" . org-cite-insert)
              ("C-c b b" . cdadar/bibtex-to-org-cite)))

(use-package citar
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :hook
  ((LaTeX-mode org-mode) . citar-capf-setup))


(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode)
  :custom
  (citar-at-point-function 'embark-act))


(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(provide 'init-citar)
;;; init-citar.el ends here
