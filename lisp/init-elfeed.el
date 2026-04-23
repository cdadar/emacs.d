;;; init-elfeed.el --- elfeed plugin  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(defvar rmh-elfeed-org-file (locate-user-emacs-file "elfeed.org")
  "Default Org file used by elfeed-org.")

(defvar rmh-elfeed-org-auto-ignore-invalid-feeds nil
  "Whether elfeed-org should ignore invalid feeds automatically.")

(defvar rmh-elfeed-org-files (list rmh-elfeed-org-file)
  "Org files used by elfeed-org.")

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (require 'elfeed)
  (require 'elfeed-org)
  (elfeed-org)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :commands (elfeed bjm/elfeed-load-db-and-open)
  :bind
  (("C-x w" . bjm/elfeed-load-db-and-open)
   :map elfeed-search-mode-map
   ("q" . bjm/elfeed-save-db-and-bury))
  :custom
  (elfeed-use-curl t))

(use-package elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-auto-ignore-invalid-feeds nil)
  (setq rmh-elfeed-org-files (list rmh-elfeed-org-file))
  :config
  (elfeed-org))

(use-package elfeed-autotag
  :hook (after-init . elfeed-autotag)
  :init
  (setq elfeed-autotag-files (list rmh-elfeed-org-file)))



(defun spike-leung/org-open-rss-feed-as-site-in-elfeed-org-files (orig-fun &rest args)
  "Advice for `org-open-at-point' to redirect RSS links only in a specific file."
  (let* ((element (org-element-context))
         (link (and (eq (org-element-type element) 'link)
                    (org-element-property :raw-link element))))
    (if (and buffer-file-name
             (string-equal (expand-file-name (buffer-file-name))
                           (expand-file-name rmh-elfeed-org-file))
             link
             (string-match-p (rx (or "rss" "feed" "atom" "xml")) link))
        (let* ((url-parts (url-generic-parse-url link))
               (scheme (url-type url-parts))
               (host (url-host url-parts))
               (site-url (concat scheme "://" host)))
          (message "Opening site for feed: %s" site-url)
          (browse-url site-url))
      (apply orig-fun args))))

(unless (advice-member-p #'spike-leung/org-open-rss-feed-as-site-in-elfeed-org-files
                         'org-open-at-point)
  (advice-add 'org-open-at-point :around
              #'spike-leung/org-open-rss-feed-as-site-in-elfeed-org-files))


(defconst spike-leung/elfeed-search-filter "@3-months-ago +unread"
  "Query string filtering shown entries.")

(defun spike-leung/get-feed-candidates (&optional level)
  "Extract headings title from `rmh-elfeed-org-files' as consult candidates.
If LEVEL exist, filter heading which level is greater or equal LEVEL."
  (mapcan
   (lambda (elfeed-org-file)
     (with-current-buffer (or (find-buffer-visiting elfeed-org-file)
                              (find-file-noselect elfeed-org-file))
       (delq nil
             (org-element-map (org-element-parse-buffer 'headline) 'headline
               (lambda (hl)
                 ;; property 的值可以在这里找： https://orgmode.org/worg/dev/org-element-api.html
                 (when (or (null level) (>= (org-element-property :level hl) level))
                   (let* ((raw-title (org-element-property :raw-value hl))
                          (title (org-link-display-format raw-title))
                          (annotation (org-entry-get (org-element-property :begin hl)
                                                     "description"))
                          (feed-url (when (string-match org-link-bracket-re raw-title)
                                      (match-string 1 raw-title))))
                     (list :items (list title) :feed-url feed-url :annotation annotation))))
               nil))))
   rmh-elfeed-org-files))

(defun spike-leung/elfeed-preview-state (state candidate)
  "Return consult state function for live `elfeed' preview.
See `consult--with-preview' about STATE and CANDIDATE."
  (let* ((cand (car candidate))
         (metadata (cdr candidate))
         (feed-url (plist-get metadata :feed-url)))
    (pcase state
      ('setup
       (unless (get-buffer "*elfeed-search*")
         (elfeed-apply-hooks-now)
         (elfeed-org)
         (elfeed)
         (elfeed-search-clear-filter))
       (display-buffer "*elfeed-search*" '(display-buffer-reuse-window)))
      ('preview
       (elfeed-search-clear-filter)
       (when (and cand (get-buffer "*elfeed-search*"))
         (unless (string-empty-p cand)
           (elfeed-search-set-filter (concat spike-leung/elfeed-search-filter " =" (string-replace " " "." cand))))))
      ('return
       (when (and cand feed-url (not (string-empty-p cand)))
         (elfeed-search-set-filter (concat spike-leung/elfeed-search-filter " =" (string-replace " " "." cand)))
         (elfeed-update-feed feed-url))))))

(defun spike-leung/consult-elfeed ()
  "Select feed from `rmh-elfeed-org-files' with live preview in `elfeed'."
  (interactive)
  (let* ((candidates (spike-leung/get-feed-candidates 3)))
    (consult--multi candidates
                    :prompt "Feed: "
                    :state #'spike-leung/elfeed-preview-state
                    :history 'spike-leung/consult-elfeed-history
                    :annotate (lambda (cand)
                                (let* ((match-cand (seq-find
                                                    (lambda (v)
                                                      (string= (car (plist-get v :items)) cand))
                                                    candidates))
                                       (annotation (and match-cand (plist-get match-cand :annotation))))
                                  (when annotation
                                    (concat (make-string 25 ?\s) annotation)))))
    (when (get-buffer "*elfeed-search*")
      (pop-to-buffer "*elfeed-search*"))))




(provide 'init-elfeed)
