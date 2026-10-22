;;; init-org-gtd.el --- GTD：日程、重复任务、计时与 capture -*- lexical-binding: t -*-
;;; Commentary:
;; Org 的 GTD 工作流：agenda 窗口钩子、每月第 n 个周几的重复规则、
;; clocking（含 macOS 的 org-clock-statusbar）、capture 模板，以及 refile /
;; archive / agenda effort 统计这些助手函数。
;; `use-package org' 的 :config 里只保留 `cdadar/org-setup-*' 的调用。
;;; Code:

;; === Files ===
;; capture 模板与 agenda 要用到的文件路径。这些是**默认值**：`lisp/init-local.el'
;; （不入库）用 `setq' 覆盖。全部从 `org-directory' 派生，换目录只改那一个。
(defvar org-agenda-file-inbox (expand-file-name "inbox.org" org-directory))
(defvar org-agenda-file-note (expand-file-name "notes.org" org-directory))
(defvar org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
(defvar org-agenda-file-work (expand-file-name "work.org" org-directory))
(defvar org-agenda-file-journal (expand-file-name "journal.org" org-directory))
(defvar org-agenda-file-code-snippet (expand-file-name "snippet.org" org-directory))
(defvar org-agenda-file-blogposts (expand-file-name "all-posts.org" org-directory))
(defvar org-capture-web-bookmarks (expand-file-name "web.org" org-directory))
(defvar org-capture-anki (expand-file-name "anki.org" org-directory))
(defvar org-default-notes-file org-agenda-file-inbox)

;; === Agenda window hooks ===
(defun cdadar/org-agenda-align-on-config-change ()
  (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))

(defun cdadar/org-setup-agenda-window-hooks ()
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook #'cdadar/org-agenda-align-on-config-change)))

;; === Custom recurring schedule ===
(defconst cdadar/org-monthly-nth-weekday-property "ORG_MONTHLY_NTH"
  "Property name for the monthly nth weekday rule.")

(defconst cdadar/org-monthly-weekday-property "ORG_MONTHLY_WEEKDAY"
  "Property name for the monthly weekday rule.")

(defun cdadar/org-nth-weekday-of-month (year month weekday nth)
  "Return encoded time for the NTH WEEKDAY in YEAR MONTH.
WEEKDAY follows Emacs convention: 0=Sunday .. 6=Saturday.
NTH supports 1..5, or -1 for the last WEEKDAY of the month."
  (let ((last-day (calendar-last-day-of-month month year)))
    (cond
     ((> nth 0)
      (let* ((first-dow (calendar-day-of-week (list month 1 year)))
             (delta (mod (- weekday first-dow) 7))
             (day (+ 1 delta (* 7 (1- nth)))))
        (when (> day last-day)
          (error "Month %04d-%02d has no %dth weekday %d"
                 year month nth weekday))
        (encode-time 0 0 0 day month year)))
     ((= nth -1)
      (let* ((last-dow (calendar-day-of-week (list month last-day year)))
             (delta (mod (- last-dow weekday) 7))
             (day (- last-day delta)))
        (encode-time 0 0 0 day month year)))
     (t
      (error "Unsupported monthly nth value: %S" nth)))))

(defun cdadar/org-next-monthly-nth-weekday (from-time weekday nth)
  "Return the next monthly NTH WEEKDAY strictly after FROM-TIME."
  (let* ((decoded (decode-time from-time))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         candidate)
    (while
        (progn
          (setq candidate
                (condition-case nil
                    (cdadar/org-nth-weekday-of-month year month weekday nth)
                  (error nil)))
          (if (and candidate (time-less-p from-time candidate))
              nil
            (setq month (1+ month))
            (when (> month 12)
              (setq month 1
                    year (1+ year)))
            t)))
    candidate))

(defun cdadar/org-monthly-nth-weekday-entry-p ()
  "Return non-nil when current entry defines a valid monthly nth weekday rule."
  (let ((nth-str (org-entry-get nil cdadar/org-monthly-nth-weekday-property))
        (weekday-str (org-entry-get nil cdadar/org-monthly-weekday-property)))
    (when (and nth-str weekday-str
               (string-match-p "^-?[0-9]+$" nth-str)
               (string-match-p "^-?[0-9]+$" weekday-str))
      (let ((nth (string-to-number nth-str))
            (weekday (string-to-number weekday-str)))
        (and (member nth '(-1 1 2 3 4 5))
             (<= 0 weekday 6))))))

(defvar cdadar/org-reschedule-monthly-nth-weekday--in-progress nil
  "Non-nil while monthly nth weekday rescheduling is already running.")

(defun cdadar/org-reschedule-monthly-nth-weekday ()
  "Reschedule current entry using its monthly nth weekday properties."
  (when (and (not cdadar/org-reschedule-monthly-nth-weekday--in-progress)
             (derived-mode-p 'org-mode)
             (member org-state org-done-keywords)
             (not (member org-last-state org-done-keywords))
             (cdadar/org-monthly-nth-weekday-entry-p))
    (let* ((cdadar/org-reschedule-monthly-nth-weekday--in-progress t)
           (nth (string-to-number
                 (org-entry-get nil cdadar/org-monthly-nth-weekday-property)))
           (weekday (string-to-number
                     (org-entry-get nil cdadar/org-monthly-weekday-property)))
           (base-time (or (org-get-scheduled-time (point))
                          (current-time)))
           (next-time (cdadar/org-next-monthly-nth-weekday base-time weekday nth))
           (timestamp (format-time-string (org-time-stamp-format) next-time)))
      (org-schedule nil timestamp)
      (when (and org-todo-repeat-to-state
                 (not (string-empty-p org-todo-repeat-to-state)))
        (org-todo org-todo-repeat-to-state)))))

(defun cdadar/org-set-monthly-nth-weekday (nth weekday)
  "Set current heading to recur on monthly NTH WEEKDAY.
WEEKDAY uses Emacs convention: 0=Sunday .. 6=Saturday.
NTH supports 1..5, or -1 for the last weekday in month."
  (interactive
   (list
    (read-number "第几个周几（1..5，最后一个填 -1）: ")
    (read-number "周几（0=周日 .. 6=周六）: ")))
  (unless (member nth '(-1 1 2 3 4 5))
    (user-error "NTH 必须是 1..5 或 -1"))
  (unless (<= 0 weekday 6)
    (user-error "WEEKDAY 必须在 0..6 之间"))
  (org-entry-put nil cdadar/org-monthly-nth-weekday-property
                 (number-to-string nth))
  (org-entry-put nil cdadar/org-monthly-weekday-property
                 (number-to-string weekday))
  (message "Set monthly nth weekday rule: nth=%s weekday=%s" nth weekday))

;; === Clocking ===
(defun cdadar/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun cdadar/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(defun cdadar/org-clock-statusbar-available-p ()
  (and *is-a-mac*
       (executable-find "osascript")
       (file-directory-p "/Applications/org-clock-statusbar.app")))

(defun cdadar/org-clock-statusbar-escape-text (text)
  (replace-regexp-in-string
   "\"" "\\\\\""
   (or text "")
   t t))

(defun cdadar/org-clock-statusbar-clock-in ()
  (when (cdadar/org-clock-statusbar-available-p)
    (call-process "/usr/bin/osascript" nil 0 nil "-e"
                  (concat "tell application \"org-clock-statusbar\" to clock in \""
                          (cdadar/org-clock-statusbar-escape-text org-clock-current-task)
                          "\""))))

(defun cdadar/org-clock-statusbar-clock-out ()
  (when (cdadar/org-clock-statusbar-available-p)
    (call-process "/usr/bin/osascript" nil 0 nil "-e"
                  "tell application \"org-clock-statusbar\" to clock out")))

(defun cdadar/org-setup-clocking ()
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-in-hook #'cdadar/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook #'cdadar/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook #'cdadar/hide-org-clock-from-header-line)
  (with-eval-after-load 'org-clock
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))
  (when (cdadar/org-clock-statusbar-available-p)
    (add-hook 'org-clock-in-hook #'cdadar/org-clock-statusbar-clock-in)
    (add-hook 'org-clock-out-hook #'cdadar/org-clock-statusbar-clock-out)))

;; === Capture ===
(defun cdadar/org-get-year-and-month ()
  (list (format-time-string "%Y 年") (format-time-string "%m 月")))

(defun cdadar/org-find-month-tree ()
  (let* ((path (cdadar/org-get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading))))
        (if (re-search-forward re end t)
            (goto-char (point-at-bol))
          (progn
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

(defun cdadar/org-capture-template-goto-link ()
  (org-capture-put :target (list 'file+headline
                                 (nth 1 (org-capture-get :target))
                                 (org-capture-get :annotation)))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (nth 2 (org-capture-get :target))))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
        (org-end-of-subtree)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n"))))

(defun cdadar/org-generate-anki-note-body ()
  (interactive)
  (message "Fetching note types...")
  (let ((note-types (sort (anki-editor-note-types) #'string-lessp))
        (decks (sort (anki-editor-deck-names) #'string-lessp))
        deck note-type fields)
    (setq deck (completing-read "Choose a deck: " decks))
    (setq note-type (completing-read "Choose a note type: " note-types))
    (message "Fetching note fields...")
    (setq fields (anki-editor--anki-connect-invoke-result "modelFieldNames" `((modelName . ,note-type))))
    (concat "  :PROPERTIES:\n"
            "  :ANKI_DECK: " deck "\n"
            "  :ANKI_NOTE_TYPE: " note-type "\n"
            "  :END:\n\n"
            (mapconcat (lambda (str) (concat "** " str))
                       fields
                       "\n\n"))))

(defun cdadar/org-setup-capture ()
  (setq org-capture-templates
        '(("t" "Todo" entry (file org-agenda-file-inbox)
           "* TODO [#B] %?\n  %i\n %U"
           :empty-lines 1)
          ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* %?\n  %i\n %U"
           :empty-lines 1)
          ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
           "* TODO [#B] %?  :BLOG:\n  %i\n %U"
           :empty-lines 1)
          ("B" "Brain" plain (function org-brain-goto-end)
           "* %i%?" :empty-lines 1)
          ("s" "Code Snippet" entry
           (file org-agenda-file-code-snippet)
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("w" "work" entry (file+headline org-agenda-file-work "Work")
           "* TODO [#B] %?\n  %i\n %U"
           :empty-lines 1)
          ("W" "doing work" entry (file+headline org-agenda-file-work "Work")
           "* TODO [#B] %?\n  %i\n %U"
           :empty-lines 1 :clock-in t :clock-resume t)
          ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
           "* TODO [#C] %?\n  %i\n %a \n %U"
           :empty-lines 1)
          ("j" "Journal Entry"
           entry (file+olp+datetree org-agenda-file-journal)
           "* %?"
           :empty-lines 1)
          ("v" "Vocabulary" entry
           (file+headline org-capture-anki "Vocabulary")
           ,(concat "* %^{heading} :note:\n"
                    "%(cdadar/org-generate-anki-note-body)\n"))
          ("a" "Anki basic"
           entry
           (file+headline org-capture-anki "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n")
          ("A" "Anki cloze"
           entry
           (file+headline org-capture-anki "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n")
          ("p" "Protocol")
          ("pb" "Protocol Bookmarks" entry
           (file+headline org-capture-web-bookmarks "Bookmarks")
           "* %U - %:annotation" :immediate-finish t :kill-buffer t)
          ("pn" "Protocol Bookmarks" entry
           (file+headline org-capture-web-bookmarks "Notes")
           "* %U - %:annotation %^g\n\n  %?" :empty-lines 1 :kill-buffer t)
          ("pa" "Protocol Annotation" plain
           (file+function org-capture-web-bookmarks cdadar/org-capture-template-goto-link)
           "  %U - %?\n\n  %:initial" :empty-lines 1))))

(defun cdadar/org-setup-refile ()
  (advice-add 'org-refile :after #'cdadar/org-save-all-org-buffers-after-refile))

;;; --- Standalone utilities and interactive commands ---

(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

(defun cdadar/org-save-all-org-buffers-after-refile (&rest _)
  (org-save-all-org-buffers))

(defun cdadar/org-archive-done-tasks ()
  "archive of DONE AND CANCELLED in current buffer"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

(defun cdadar/org-agenda-calculate-efforts (limit)
  "Sum the efforts of scheduled entries up to LIMIT in the agenda buffer.
LIMIT may be nil, in which case it defaults to the end of the buffer."
  (let ((limit (or limit (point-max)))
        total)
    (save-excursion
      (while (< (point) limit)
        (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
          (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
        (forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity total))))))

(defun cdadar/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos)
      (while (setq pos (text-property-any
                        (point) (point-max) 'org-agenda-date-header t))
        (goto-char pos)
        (end-of-line)
        (insert-and-inherit (concat " ("
                                    (cdadar/org-agenda-calculate-efforts
                                     (or (next-single-property-change (point) 'day)
                                         (point-max)))
                                    ")"))
        (forward-line)))))

(defun cdadar/org-filter-by-tag (current-tag)
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun cdadar/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("work" "@office" "@home" "@computer" "@phone" "@kindle" "@trailing"
                         "bug" "demand" "video" "book" "game"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4)
                          (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16)
                          (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16)
                        (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend (lambda () (cdadar/org-filter-by-tag current-tag)))
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))


(provide 'init-org-gtd)
;;; init-org-gtd.el ends here
