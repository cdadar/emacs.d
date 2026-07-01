;;; Init-Org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :commands (org-capture org-agenda org-store-link)
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-M-<up>" . org-up-element))
  :mode ("\\.\\(org\\|org_archive\\)\\'" . org-mode)
  :hook ((org-mode . cdadar/org-mode-setup)
         (org-agenda-mode . hl-line-mode)
         (org-agenda-after-show . org-show-entry)
         (org-agenda-finalize . my/org-agenda-insert-efforts)
         (org-after-todo-state-change . cdadar/org-reschedule-monthly-nth-weekday))
  :custom
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-export-headline-levels 5)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-export-allow-bind-keywords t)
  (org-enforce-todo-dependencies nil)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-image-actual-width '(400))
  (org-tags-column 80)
  (org-tags-exclude-from-inheritance '("secret"))
  (org-deadline-warning-days 30)
  (org-lowest-priority 68)
  (org-support-shift-select t)
  (org-refile-use-cache nil)
  (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  (org-export-in-background nil)
  ;; clocking
  (org-clock-persist t)
  (org-clock-in-resume t)
  ;; Save clock data and notes in the LOGBOOK drawer
  (org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (org-clock-out-when-done t)
  (org-clock-clocked-in-display 'mode-line)
  (org-clock-mode-line-total 'today)
  ;; refile
  (org-refile-target-verify-function 'sanityinc/verify-refile-target)
  ;; agenda
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
     (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
     (sequence "WAITING(w@/!)" "TESTING(T!)" "PUBLISH(P!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (org-todo-keyword-faces
   '(("NEXT" :inherit warning)
     ("PROJECT" :inherit font-lock-string-face)))
  (org-tag-alist
   '(("@office" . ?o)
     ("@home" . ?h)
     ("@way" . ?w)
     ("@trailing" . ?t)
     ("@computer" . ?c)
     ("@phone" . ?p)
     ("@kindle" . ?k)
     ("bug" . ?b)
     ("demand" . ?d)
     ("video" . ?v)
     ("book" . ?B)))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-agenda-skip-scheduled-delay-if-deadline t)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up deadline-up scheduled-up effort-up category-keep)
     (todo priority-down category-up effort-up)
     (tags priority-down category-up effort-up)
     (search category-up)))
  (org-agenda-window-setup 'current-window)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::datetree/")
  (org-stuck-projects '("-INBOX/PROJECT" ("NEXT")))
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (org-agenda-custom-commands
   '(("R" "Review projects" tags-todo "-CANCELLED/"
      ((org-agenda-overriding-header "Reviews Scheduled")
       (org-agenda-skip-function 'org-review-agenda-skip)
       (org-agenda-cmp-user-defined 'org-review-compare)
       (org-agenda-sorting-strategy '(user-defined-down))))
     ("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("g" "GTD"
      ((agenda "" nil)
       (tags "INBOX"
             ((org-agenda-overriding-header "Inbox")
              (org-tags-match-list-sublevels nil)))
       (stuck ""
              ((org-agenda-overriding-header "Stuck Projects")
               (org-agenda-tags-todo-honor-ignore-options t)
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled 'future)))
       (tags-todo "-INBOX"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                          (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down priority-down effort-up category-keep))))
       (tags-todo "-INBOX/PROJECT"
                  ((org-agenda-overriding-header "Projects")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(priority-down category-keep))))
       (tags-todo "-INBOX/-NEXT"
                  ((org-agenda-overriding-header "Orphaned Tasks")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                          (org-agenda-skip-subtree-if 'nottodo '("TODO")))))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(priority-down category-keep))))
       (tags-todo "/WAITING"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-sorting-strategy
                    '(priority-down category-keep))))
       (tags-todo "/DELEGATED"
                  ((org-agenda-overriding-header "Delegated")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-sorting-strategy
                    '(priority-down category-keep))))
       (tags-todo "-INBOX"
                  ((org-agenda-overriding-header "On Hold")
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                          (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-sorting-strategy
                    '(priority-down category-keep))))))
     ("w" . "任务安排")
     ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
     ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
     ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
     ("wd" "不重要且不紧急的任务" tags-todo "+PRIORITY=\"D\"")
     ("b" "Blog" tags-todo "BLOG")
     ("p" . "项目安排")
     ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
     ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"chens\"")
     ("W" "Weekly Review"
      ((stuck "")
       (tags-todo "PROJECT")))
     ("c" . "特定标签")
     ("co" "At the office" tags-todo "@office")
     ("ch" "At the home" tags-todo "@home")
     ("ct" "At the travelling" tags-todo "@travelling")
     ("cw" "At the way" tags-todo "@way")
     ("cc" "At the computer" tags-todo "@computer")
     ("cp" "At the phone" tags-todo "@phone")
     ("ck" "At the kindle" tags-todo "@kindle")
     ("cb" "bug" tags-todo "bug")
     ("cd" "demand" tags-todo "demand")
     ("cB" "book" tags-todo "book")
     ("cv" "video" tags-todo "video")))
  (org-icalendar-include-todo 'all)
  (org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
  (org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
  :config
  ;; === Mode setup ===
  (defun cdadar/org-mode-setup ()
    (setq truncate-lines nil
          word-wrap t)
    (setq-local visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
    (visual-line-mode 1)
    (local-set-key [remap move-end-of-line] #'move-end-of-line)
    (local-set-key [remap move-beginning-of-line] #'move-beginning-of-line)
    (local-set-key [remap kill-line] #'kill-line))

  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil
    :lighter " Prose"
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil
                word-wrap t
                cursor-type 'bar)
          (when (derived-mode-p 'org-mode)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0))))

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

  ;; === Babel ===
  (defun cdadar/org-find-first-existing-file (&rest paths)
    (seq-find #'file-exists-p (delq nil paths)))

  (defun cdadar/org-setup-babel ()
    (let ((ditaa-jar (cdadar/org-find-first-existing-file
                      (locate-user-emacs-file "ditaa0_9.jar")
                      (expand-file-name "ditaa0_9.jar" user-emacs-directory)
                      (let ((env (getenv "DITAA_JAR")))
                        (and env (expand-file-name env)))))
          (plantuml-jar (cdadar/org-find-first-existing-file
                         (locate-user-emacs-file "plantuml.jar")
                         (expand-file-name "plantuml.jar" user-emacs-directory)
                         (let ((env (getenv "PLANTUML_JAR")))
                           (and env (expand-file-name env))))))
      (with-eval-after-load 'ob-ditaa
        (if ditaa-jar
            (setq org-ditaa-jar-path ditaa-jar)
          (message "Org Babel ditaa support not configured: set DITAA_JAR or place ditaa0_9.jar in %s" user-emacs-directory)))
      (with-eval-after-load 'ob-plantuml
        (cond
         ((executable-find "plantuml")
          (setq org-plantuml-exec-mode 'plantuml))
         (plantuml-jar
          (setq org-plantuml-jar-path plantuml-jar
                org-plantuml-exec-mode 'jar))
         (t
          (message "Org Babel PlantUML support not configured: install plantuml or set PLANTUML_JAR / plantuml.jar under %s" user-emacs-directory)))))
    (defvar sanityinc/org-babel-setup-done nil)
    (defun sanityinc/org-babel-setup ()
      (unless sanityinc/org-babel-setup-done
        (setq sanityinc/org-babel-setup-done t)
        (org-babel-do-load-languages
         'org-babel-load-languages
         (seq-filter
          (lambda (pair)
            (locate-library (concat "ob-" (symbol-name (car pair)))))
          '((R . t)
            (ditaa . t)
            (dot . t)
            (emacs-lisp . t)
            (gnuplot . t)
            (haskell . nil)
            (latex . t)
            (ledger . t)
            (ocaml . nil)
            (octave . t)
            (plantuml . t)
            (python . t)
            (ruby . t)
            (screen . nil)
            (sh . t)
            (shell . t)
            (sql . t)
            (sqlite . t))))))
    (with-eval-after-load 'ob-core
      (sanityinc/org-babel-setup)))

  ;; === Crypt ===
  (defun cdadar/org-setup-crypt ()
    (require 'org-crypt nil t)
    (when (featurep 'org-crypt)
      (org-crypt-use-before-save-magic)
      ;; 有解密条目的 buffer 跳过自动保存，避免 super-save 立刻重新加密
      (defvar-local cdadar/org-crypt-decrypting nil
        "Non-nil when an org-crypt entry has been decrypted in this buffer.")

      (defun cdadar/org-decrypt-mark (&rest _)
        (setq-local cdadar/org-crypt-decrypting t))
      (advice-add 'org-decrypt-entry :after #'cdadar/org-decrypt-mark)

      (defun cdadar/org-encrypt-unmark (&rest _)
        (unless (save-excursion
                  (goto-char (point-min))
                  (re-search-forward
                   (concat "^\\*+[ \t].*:" org-crypt-tag-matcher ":") nil t))
          (kill-local-variable 'cdadar/org-crypt-decrypting)))
      (advice-add 'org-encrypt-entry :after #'cdadar/org-encrypt-unmark)))

  ;; === Global prefix keymap ===
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (defun cdadar/org-setup-global-prefix ()
    (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
    (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
    (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
    (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
    (define-key sanityinc/org-global-prefix-map (kbd "r") 'org-clock-report)
    (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map))


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

  ;; === UI ===
  (defun cdadar/org-kill-link-at-point ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (let* ((context (org-element-context))
             (type (org-element-type context))
             (beg (org-element-property :begin context))
             (end (org-element-property :end context)))
        (when (eq type 'link)
          (kill-region beg end)))))

  (defun cdadar/org-setup-ui ()
    (when *is-a-mac*
      (define-key org-mode-map (kbd "M-h") nil)))

  ;; === Setup calls ===
  (cdadar/org-setup-agenda-window-hooks)
  (cdadar/org-setup-clocking)
  (cdadar/org-setup-babel)
  (cdadar/org-setup-crypt)
  (cdadar/org-setup-refile)
  (cdadar/org-setup-global-prefix)
  (cdadar/org-setup-capture)
  (cdadar/org-setup-ui))

(use-package org-crypt
  :ensure nil
  :custom
  (org-crypt-tag-matcher "secret")
  (org-crypt-key "6DF1ABB0"))

(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))

;;; --- Export post-processing helpers ---

(defconst cdadar/org-latex-english-main-font "TeX Gyre Termes"
  "English main font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-main-font "LXGW WenKai"
  "Default Chinese body font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-mono-font "LXGW WenKai Mono"
  "Default Chinese monospaced font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-quote-font "Kaiti SC"
  "Default Chinese quote font used for Org LaTeX/PDF exports.")

(defun cdadar/org-latex-tex-gyre-termes-mainfont-snippet ()
  "Return a portable fontspec snippet for TeX Gyre Termes.
Use file-name based lookup so TeX can find the OTF files through kpathsea even
when the TeX Live installation path changes."
  "\\setmainfont{texgyretermes}[
  Extension=.otf,
  UprightFont=*-regular,
  BoldFont=*-bold,
  ItalicFont=*-italic,
  BoldItalicFont=*-bolditalic,
  Scale=1.0]")

(defun cdadar/org-latex-use-english-main-font (text backend info)
  "Use `cdadar/org-latex-english-main-font' for Org LaTeX TEXT.
This keeps older notes that specify Times New Roman in their local headers
visually consistent with the current Org PDF export preference.  BACKEND and
INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\setmainfont\\(?:\\[[^]]*\\]\\)?{Times New Roman}\\(?:[[:blank:]]*%.*\\)?"
       (cdadar/org-latex-tex-gyre-termes-mainfont-snippet)
       text t t)
    text))

(defun cdadar/org-latex-quote-cjk-font-snippet ()
  "Return LaTeX setup for the preferred CJK quote font."
  (format "\\usepackage{etoolbox}
\\newCJKfontfamily\\quotecjkfont[Scale=1.0]{%s}
\\AtBeginEnvironment{quote}{\\quotecjkfont}"
          cdadar/org-latex-cjk-quote-font))

(defun cdadar/org-latex-needs-quote-cjk-font-p (text)
  "Return non-nil when exported LaTeX TEXT should get quote CJK font setup."
  (and (string-match-p "\\\\begin{quote}" text)
       (or (string-match-p "\\\\usepackage{xeCJK}" text)
           (string-match-p "\\\\setCJKmainfont" text))
       (not (string-match-p "\\\\newCJKfontfamily\\\\quotecjkfont" text))
       (not (string-match-p "\\\\AtBeginEnvironment{quote}{\\\\quotecjkfont}" text))))

(defun cdadar/org-latex-inject-quote-cjk-font (text backend info)
  "Inject preferred CJK quote font setup into exported LaTeX TEXT.
BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (cdadar/org-latex-needs-quote-cjk-font-p text))
      (replace-regexp-in-string
       "\\\\begin{document}"
       (concat (cdadar/org-latex-quote-cjk-font-snippet) "\n\\begin{document}")
       text t t)
    text))

(defconst cdadar/org-latex-global-symbol-fallback-snippet
  (mapconcat
   #'identity
   '("\\usepackage{newunicodechar}"
     "\\IfFontExistsTF{Arial Unicode MS}"
     "  {\\newfontfamily\\symbolfallback{Arial Unicode MS}}"
     "  {\\IfFontExistsTF{Apple Symbols}"
     "     {\\newfontfamily\\symbolfallback{Apple Symbols}}"
     "     {\\newfontfamily\\symbolfallback{DejaVu Sans}}}"
     "\\newunicodechar{※}{{\\symbolfallback ※}}"
     "\\newunicodechar{①}{{\\symbolfallback ①}}"
     "\\newunicodechar{②}{{\\symbolfallback ②}}"
     "\\newunicodechar{③}{{\\symbolfallback ③}}"
     "\\newunicodechar{④}{{\\symbolfallback ④}}"
     "\\newunicodechar{▸}{{\\symbolfallback ▸}}"
     "\\newunicodechar{‐}{{\\symbolfallback ‐}}"
     "\\newunicodechar{─}{{\\symbolfallback ─}}")
   "\n")
  "LaTeX header snippet providing fallback glyphs missing in common serif fonts.")

(defun cdadar/org-latex-global-needs-symbol-fallback-p (text)
  "Return non-nil when exported LaTeX TEXT needs fallback symbol font setup."
  (and (or (string-match-p "\\\\setmainfont\\(?:\\[[^]]*\\]\\)?{Times New Roman}" text)
           (string-match-p "\\\\setmainfont\\(?:\\[\(?:.\|\n\)*\\]\\)?{texgyretermes}" text)
           (string-match-p "\\\\setmainfont\\(?:\\[[^]]*\\]\\)?{TeX Gyre Termes}" text))
       (string-match-p "[※①②③④▸‐─]" text)
       (not (string-match-p "\\\\newfontfamily\\\\symbolfallback" text))))

(defun cdadar/org-latex-global-inject-symbol-fallback (text backend info)
  "Inject fallback glyph support into exported LaTeX TEXT.
BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (cdadar/org-latex-global-needs-symbol-fallback-p text))
      (replace-regexp-in-string
       "\\\\begin{document}"
       (concat cdadar/org-latex-global-symbol-fallback-snippet "\n\\begin{document}")
       text t t)
    text))

(defun cdadar/org-latex-fix-bibleref-compat (text backend info)
  "Normalize older bibleref snippets in exported LaTeX TEXT.
Some notes still use `\\usepackage[style=default]{bibleref}', but recent
bibleref releases reject that option.  Also rewrite the custom `\\verse{...}'
shortcut to `\\bibleref{...}' instead of redefining LaTeX's built-in verse
environment command.  BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "\\usepackage[style=default]{bibleref}" nil t)
          (replace-match "\\usepackage{bibleref}" t t))
        (goto-char (point-min))
        (while (search-forward "\\newcommand{\\verse}[1]{\\bibleref{#1}}" nil t)
          (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
        (goto-char (point-min))
        (while (search-forward "\\verse{" nil t)
          (replace-match "\\bibleref{" t t))
        (buffer-string))
    text))

(defun cdadar/org-latex-fix-quote-paragraph-spacing (text backend info)
  "Replace quote-internal blank paragraphs after LaTeX line breaks.
This keeps grouped quote lines visually separated in PDF without triggering
paragraph indentation inside quote environments.  INFO is ignored."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward "\\\\begin{quote}" nil t)
          (let ((quote-start (match-beginning 0)))
            (when (re-search-forward "\\\\end{quote}" nil t)
              (let ((quote-end-marker (copy-marker (match-end 0))))
                (save-restriction
                  (narrow-to-region quote-start quote-end-marker)
                  (goto-char (point-min))
                  (while (search-forward "\\\\\n\n" nil t)
                    (replace-match "\\\\[0.6\\baselineskip]\n" t t))
                  (widen))
                (goto-char quote-end-marker)
                (set-marker quote-end-marker nil)))))
        (buffer-string))
    text))

(defconst cdadar/org-latex-beamer-class
  '("beamer"
    "\\documentclass[presentation]{beamer}\n"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
  "Beamer class definition appended to `org-latex-classes'.")

(defun cdadar/org-latex-export-to-pdf-async ()
  "Asynchronously export the current Org buffer to PDF."
  (interactive)
  (org-latex-export-to-pdf t))

(defun cdadar/org-latex-fix-multicolumn-trailing-ampersand (text backend info)
  "Remove trailing `&' after `\\multicolumn' when it spans all columns.
Org pads single-cell rows to the full column count, adding an extra
`&' after `\\multicolumn{N}{...}{...}' which creates a column-count
mismatch in LaTeX."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     "\\\\multicolumn{\\([0-9]+\\)}{\\([^}]*\\)}{\\([^}]*\\)} & \\\\\\\\"
     "\\\\multicolumn{\\1}{\\2}{\\3} \\\\\\\\"
     text)))

(use-package ox
  :ensure nil
  :after org
  :config
  (require 'ox-md nil t)
  (require 'ox-latex nil t)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-use-english-main-font)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-inject-quote-cjk-font)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-global-inject-symbol-fallback)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-bibleref-compat)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-quote-paragraph-spacing)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-multicolumn-trailing-ampersand))

(use-package ox-latex
  :ensure nil
  :after ox
  :custom
  (org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -xelatex -interaction=nonstopmode -outdir=%o %f")
     '("xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f")))
  :config
  (add-to-list 'org-latex-classes cdadar/org-latex-beamer-class))

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

(defun cdadar/org-setup-refile ()
  (advice-add 'org-refile :after #'cdadar/org-save-all-org-buffers-after-refile))

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
  "Sum the efforts of scheduled entries up to LIMIT in the agenda buffer."
  (let (total)
    (save-excursion
      (while (< (point) limit)
        (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
          (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
        (forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity total))))))

(defun my/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos)
      (while (setq pos (text-property-any
                        (point) (point-max) 'org-agenda-date-header t))
        (goto-char pos)
        (end-of-line)
        (insert-and-inherit (concat " ("
                                    (cdadar/org-agenda-calculate-efforts
                                     (next-single-property-change (point) 'day))
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

;;; --- Related packages ---

(use-package writeroom-mode
  :if (locate-library "writeroom-mode")
  :diminish writeroom-mode)


(use-package grab-mac-link
  :if (and (bound-and-true-p *is-a-mac*)
           (locate-library "grab-mac-link"))
  :commands (grab-mac-link grab-mac-link-dwim)
  :bind (:map org-mode-map
              ("C-c g" . grab-mac-link)))

(use-package org-pomodoro
  :if (locate-library "org-pomodoro")
  :after org
  :bind
  ([(meta p)] . org-pomodoro)
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  :hook
  ((org-pomodoro-finished . cdadar/org-pomodoro-finished-notify)
   (org-pomodoro-break-finished . cdadar/org-pomodoro-break-finished-notify)
   (org-pomodoro-long-break-finished . cdadar/org-pomodoro-long-break-finished-notify)
   (org-pomodoro-killed . cdadar/org-pomodoro-killed-notify))
  :config
  (defun cdadar/org-pomodoro-notify (title message)
    (call-process "notify-send" nil 0 nil title message))
  (defun cdadar/org-pomodoro-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro completed!" "Time for a break."))
  (defun cdadar/org-pomodoro-break-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?"))
  (defun cdadar/org-pomodoro-long-break-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?"))
  (defun cdadar/org-pomodoro-killed-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))

;; Editing and UI extensions
(use-package org-cliplink
  :if (locate-library "org-cliplink")
  :after org
  :commands (org-cliplink org-cliplink-dwim)
  :bind (:map org-mode-map
              ("C-x p i" . org-cliplink)))

(use-package org-appear
  :if (locate-library "org-appear")
  :after org
  :hook (org-mode . org-appear-mode))

;; Export extensions
;; create ppt
(use-package ox-ioslide
  :if (locate-library "ox-ioslide")
  :after ox
  :commands (org-ioslide-export-as-html org-ioslide-export-to-html))

(use-package org-mind-map
  :if (locate-library "org-mind-map")
  :after ox
  :commands (org-mind-map-write
             org-mind-map-write-with-prompt
             org-mind-map-write-current-branch
             org-mind-map-write-current-tree)
  :custom
  (org-mind-map-engine "dot")
  :init
  (require 'ox-org nil t))

;; Knowledge and media extensions
(use-package org-brain
  :if (locate-library "org-brain")
  :after org
  :custom
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 12)
  (org-brain-include-file-entries nil)
  (org-brain-file-entries-use-title nil))

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-track-globally t)
  (org-id-locations-file (locate-user-emacs-file ".org-id-locations"))
  (org-id-link-to-org-use-id t))

(use-package org-download
  :if (locate-library "org-download")
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-dir "./img")
  (org-download-screenshot-method (if *is-a-mac* "pngpaste %s" "import %s"))
  (org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(use-package org-tree-slide
  :if (locate-library "org-tree-slide")
  :after org
  :bind
  (([(f8)] . org-tree-slide-mode)
   ([(shift f8)] . org-tree-slide-skip-done-toggle)
   :map org-tree-slide-mode-map
   ([(f9)] . org-tree-slide-move-previous-tree)
   ([(f10)] . org-tree-slide-move-next-tree)))

;; Roam and reference extensions
(use-package org-roam
  :if (locate-library "org-roam")
  :defer t
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n s" . org-roam-db-sync)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n T" . org-roam-tag-remove)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today))
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "Christian")
     ("ca" "基督学房" plain "%?"
      :target (file+head "christian/academyofchrist/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("cb" "个人" plain "%?"
      :target (file+head "christian/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("cB" "圣经" plain "%?"
      :target (file+head "christian/bible/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("ce" "例证" plain "%?"
      :target (file+head "christian/exmaple/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+tags: 例证")
      :unnarrowed t)
     ("co" "听道" plain "%?"
      :target (file+head "christian/other/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+tags: 听道")
      :unnarrowed t)
     ("cs" "查经" plain "%?"
      :target (file+head "christian/study/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+tags: 查经")
      :unnarrowed t)
     ("cS" "学道" plain "%?"
      :target (file+head "christian/word/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+tags: 学道")
      :unnarrowed t)
     ("cw" "忘记背后" plain "%?"
      :target (file+head "christian/wjbh/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+tags: 忘记背后")
      :unnarrowed t)
     ("cy" "以斯拉学习" plain "%?"
      :target (file+head "christian/yisila/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("cz" "栽培班" plain "%?"
      :target (file+head "christian/cultivation/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "program" plain "%?"
      :target (file+head "program/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("n" "Booknotes" plain "%?"
      :target (file+head "booknotes/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("i" "investment" plain "%?"
      :target (file+head "investment/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("o" "other" plain "%?"
      :target (file+head "other/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "work" plain "%?"
      :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("B" "blog" plain "%?"
      :target (file+head "blog/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :hook (after-init . org-roam-db-autosync-mode)
  :config
  ;; org-roam-db-autosync-mode is enabled via :hook above
  )

(use-package org-roam-ui
  :if (locate-library "org-roam-ui")
  :after org-roam
  :commands (org-roam-ui-mode org-roam-ui-open)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; Bibliography and journal extensions
(use-package ebib
  :if (locate-library "ebib")
  :defer t
  :commands (ebib)
  :custom
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (ebib-uniquify-keys t)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-index-window-size 10)
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-file-associations '(("pdf")))
  (ebib-use-timestamp t)
  (ebib-index-columns '(("Entry Key" 20 t)
                        ("Author/Editor" 40 nil)
                        ("Year" 6 t)
                        ("Title" 50 t)))
  (ebib-index-default-sort '("timestamp" . descend)))

(use-package org-ref
  :if (locate-library "org-ref")
  :after org
  :defer t
  :custom
  (org-ref-show-broken-links nil)
  (org-ref-default-ref-type "eqref")
  (org-ref-default-citation-link "citet"))

(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex))

(use-package org-journal
  :if (locate-library "org-journal")
  :defer t
  :custom
  (org-journal-prefix-key "C-c j")
  (org-journal-dir "~/org-mode/roam/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-roam-bibtex
  :if (locate-library "org-roam-bibtex")
  :after org-roam)

(use-package org-zettel-ref-mode
  :if (locate-library "org-zettel-ref-mode")
  :defer t
  :after org-roam
  :vc (:url "https://github.com/yibie/org-zettel-ref-mode" :rev :newest)
  :custom
  (org-zettel-ref-mode-type 'org-roam)
  (org-zettel-ref-quick-markup-key "C-c m")
  (org-zettel-ref-python-environment 'venv)
  (org-zettel-ref-python-env-name "org-zettel-ref"))

;; Review, math, and import extensions
(use-package org-review
  :if (locate-library "org-review")
  :bind (:map org-agenda-mode-map
              ("C-c C-r" . org-review-insert-last-review)))

(use-package org-fragtog
  :if (locate-library "org-fragtog")
  :hook ((org-mode . org-fragtog-mode)))


(use-package org-pandoc-import
  :if (locate-library "org-pandoc-import")
  :commands (org-pandoc-import-as-org
             org-pandoc-import-to-org
             org-pandoc-import-transient-mode)
  :vc (:url "https://github.com/tecosaur/org-pandoc-import"
            :rev :newest))

(provide 'init-org)
;;; init-org.el ends here
