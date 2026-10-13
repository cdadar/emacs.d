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
         ;; 标记类都在 org 里：C-c e 强调（org-emphasize，上游默认不绑键；C-c C-x C-e
         ;; 被 clock 的 org-clock-modify-effort-estimate 占了）、C-c - 列表项
         ;; （org-ctrl-c-minus）、C-c * 标题（org-ctrl-c-star），不另写命令。
         ("C-c e" . org-emphasize)
         ;; 只补已经写好、但标记紧贴汉字而失效的强调（C-c C-x F 上游空着）。
         ("C-c C-x F" . cdadar/org-fix-emphasis-boundaries)
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
          word-wrap t
          word-wrap-by-category t)
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
                word-wrap-by-category nil
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

  ;; `prose-mode' has no default binding; it lives in `org-mode-map', which
  ;; `cdadar/org-mode-setup' cannot bind because `prose-mode' is defined below.
  (define-key org-mode-map (kbd "C-c z") #'prose-mode)

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
    (define-key sanityinc/org-global-prefix-map (kbd "m") 'cdadar/md-clipboard-to-org)
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

  (defun cdadar/org-setup-refile ()
    (advice-add 'org-refile :after #'cdadar/org-save-all-org-buffers-after-refile))

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

(defconst cdadar/org-latex-font-fallbacks
  '(("思源宋体 CN" . "LXGW WenKai")
    ("思源宋体" . "LXGW WenKai")
    ("Noto Serif CJK SC" . "LXGW WenKai")
    ("Noto Serif SC" . "LXGW WenKai")
    ("思源黑体 CN" . "PingFang SC")
    ("楷体" . "Kaiti SC")
    ("Courier Std" . "Courier New"))
  "本机没装的字体 → 已装的替代字体。
每篇笔记仍然照旧写自己想要的字体；导出时那一行会被包进字体存在性判断，
本机装了真字体就自动用回真字体，不必改笔记。")

(defconst cdadar/org-latex-font-load-regexp
  (rx "\\"
      (or (seq "set" (optional "CJK") (or "main" "sans" "mono") "font")
          (seq "new" (optional "CJK") "fontfamily" "\\" (+ (any "A-Za-z@"))))
      (optional "[" (* (not (any "]"))) "]"))
  "匹配一条 LaTeX 字体加载命令（含可选参数）。")

(defun cdadar/org-latex-use-installed-fonts (text backend info)
  "本机没装的字体改用已装的字体，导出 LaTeX TEXT。
笔记照旧写自己想要的字体：这一行会被包进字体存在性判断，本机装了真字体
（比如以后补装了思源宋体）就自动用回真字体，不必改任何笔记。
BACKEND 与 INFO 遵循 Org 导出过滤器协议。"
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (let ((case-fold-search nil))
        (dolist (pair cdadar/org-latex-font-fallbacks text)
          (setq text
                (replace-regexp-in-string
                 (concat cdadar/org-latex-font-load-regexp
                         "{" (regexp-quote (car pair)) "}")
                 (lambda (match)
                   ;; MATCH 是「命令 + {字体名}」；直接去掉尾部字体名得出命令，
                   ;; 不依赖 match data（替换回调里 match data 不可靠）。
                   (let ((command (substring match 0
                                             (- (length match)
                                                (length (car pair)) 2))))
                     (concat "\\IfFontExistsTF{" (car pair) "}"
                             "{" command "{" (car pair) "}}"
                             "{" command "{" (cdr pair) "}}")))
                 text t t)))
        text)
    text))

(defconst cdadar/org-latex-cjk-char-regexp
  (format "[%c-%c%c-%c%c-%c%c-%c]"
          #x3000 #x303F #x3400 #x4DBF #x4E00 #x9FFF #xFF00 #xFFEF)
  "中日韩标点、汉字与全角字符，用来判断文档是否真需要 CJK 字体。")

(defconst cdadar/org-latex-default-cjk-font-marker "cdadar-default-cjk-font"
  "已注入默认 CJK 字体时留在导言区的标记，用于避免重复注入。")

(defun cdadar/org-latex-default-cjk-font-snippet ()
  "Return the default xeCJK setup for notes that declare no CJK font."
  (format "\\usepackage{xeCJK}
%% %s
\\setCJKmainfont[Scale=1.0]{%s}
\\setCJKsansfont[Scale=1.0]{%s}
\\setCJKmonofont[Scale=1.0]{%s}"
          cdadar/org-latex-default-cjk-font-marker
          cdadar/org-latex-cjk-main-font
          cdadar/org-latex-cjk-main-font
          cdadar/org-latex-cjk-mono-font))

(defun cdadar/org-latex-default-cjk-font (text backend info)
  "Give a note that declares no CJK font the default one.
A note without `\\setCJKmainfont' renders every Chinese character with the main
font, i.e. as nothing.  Notes that declare their own CJK font, or that contain no
CJK characters at all, are left alone.  BACKEND and INFO follow the Org export
filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "fontspec" text)
           (string-match-p cdadar/org-latex-cjk-char-regexp text)
           (not (string-match-p "\\\\setCJK\\(main\\|sans\\|mono\\)font" text))
           (not (string-match-p cdadar/org-latex-default-cjk-font-marker text))
           (string-match-p (regexp-quote "\\begin{document}") text))
      (replace-regexp-in-string
       (regexp-quote "\\begin{document}")
       (concat (cdadar/org-latex-default-cjk-font-snippet) "\n\\begin{document}")
       text t t)
    text))

(defconst cdadar/org-latex-glyph-fallback-ranges
  '((#x0250 #x02FF "\\cdadaripa")        ; 国际音标与修饰符
    (#x0370 #x03FF "\\cdadargreek")      ; 希腊文
    (#x1F00 #x1FFF "\\cdadargreek")      ; 希腊文扩展（带音）
    (#x0590 #x05FF "\\cdadarheb")        ; 希伯来文（字母、元音符号、读经记号）
    (#x2460 #x24FF "\\cdadarcjk")        ; 圈号 ①-⑳ ⑴-⒇
    (#x2500 #x257F "\\cdadarcjk")        ; 制表符
    (#x25A0 #x25FF "\\cdadarsym")        ; 几何图形 ▸ ●
    (#x2600 #x26FF "\\cdadarsym")        ; 杂项符号 ☆ ★ ☀
    (#x2700 #x27BF "\\cdadaremoji")      ; Dingbats ✅ ❤ ✂（Apple Symbols 无）
    (#x2B00 #x2BFF "\\cdadaremoji")      ; ⭐ ⭕ ⬛ ⬅
    (#x200D #x200D "\\cdadaremoji")      ; ZWJ（组合 emoji 序列）
    (#x1F000 #x1FAFF "\\cdadaremoji"))   ; 表情符号区 🔥 👍 🎉
  "字符区间 → 回退字体命令。
Times 系主字体没有国际音标、希伯来文、带音希腊文、圈号、制表符、几何符号与杂项
符号，XeTeX 又不像 LuaTeX 那样自动回退，那些字符会变成方块。这里的分配是：音标用
Lato，希腊文用笔记自己的 SBL Greek，希伯来文（含元音符号 niqqud，笔记里常它直接
写在中文句子里）用 SBL Hebrew，CJK 语境的圈号/制表符用 CJK 字体，文字类符号用系统
符号字体，emoji 与 Dingbats 用单色 emoji 字体（安装见
`~/project/private/script/local/install_emoji_fonts.sh'）。

区间互不重叠，`cdadar/org-latex-glyph-fallback-chars' 按列表顺序取第一个命中的
区间，所以重叠会让结果依赖顺序。

全角中点 ・(U+30FB) 与变体选择符 VS15/VS16(U+FE0E/FE0F) 这类缺字不在这里：
它们被 xeCJK 归入 MiddlePunct / CM 类，由 `cdadar/org-latex-cjk-autofallback'
打开 xeCJK 自带的回退（并把 emoji 字体接到回退链上）顶替，写成 `\\newunicodechar'
对它们无效。")

(defconst cdadar/org-latex-glyph-fallback-marker "cdadar-glyph-fallback"
  "已注入回退字体设置时留在导言区的标记，用于避免重复注入。")

(defun cdadar/org-latex-unicodechar-mapped-p (text char)
  "Return non-nil when TEXT already maps CHAR with `\\newunicodechar'.
Redefining such a character puts an active copy of it inside its own replacement
text, and newunicodechar then expands it forever."
  (string-match-p (concat (regexp-quote "\\newunicodechar{")
                          (regexp-quote (string char))
                          "}")
                  text))

(defun cdadar/org-latex-glyph-fallback-chars (text)
  "Return the characters in TEXT that need a fallback font.
Each element is (CHARACTER . FONT-COMMAND-NAME).  Characters the note maps
itself with `\\newunicodechar' are left to the note."
  (let (found)
    (dolist (char (delete-dups (string-to-list text)))
      (dolist (range cdadar/org-latex-glyph-fallback-ranges)
        (when (and (<= (nth 0 range) char (nth 1 range))
                   (not (assq char found))
                   (not (cdadar/org-latex-unicodechar-mapped-p text char)))
          (push (cons char (nth 2 range)) found))))
    (sort found #'car-less-than-car)))

(defun cdadar/org-latex-glyph-fallback-snippet (text)
  "Return LaTeX fallback-font setup for the characters in TEXT, or nil."
  (let ((chars (cdadar/org-latex-glyph-fallback-chars text)))
    (when chars
      (concat
       "\\usepackage{newunicodechar}\n"
       (format "%% %s\n" cdadar/org-latex-glyph-fallback-marker)
       "\\IfFontExistsTF{SBL Greek}"
       "{\\providefontfamily\\cdadargreek{SBL Greek}}"
       "{\\providefontfamily\\cdadargreek{DejaVu Sans}}\n"
       "\\IfFontExistsTF{SBL Hebrew}"
       "{\\providefontfamily\\cdadarheb[Script=Hebrew]{SBL Hebrew}}"
       "{\\providefontfamily\\cdadarheb[Script=Hebrew]{DejaVu Sans}}\n"
       (format "\\providefontfamily\\cdadarcjk{%s}\n" cdadar/org-latex-cjk-main-font)
       "\\IfFontExistsTF{Apple Symbols}"
       "{\\providefontfamily\\cdadarsym{Apple Symbols}}"
       "{\\providefontfamily\\cdadarsym{Arial Unicode MS}}\n"
       "\\IfFontExistsTF{Lato}"
       "{\\providefontfamily\\cdadaripa{Lato}}"
       "{\\providefontfamily\\cdadaripa{DejaVu Sans}}\n"
       "\\IfFontExistsTF{Noto Emoji}"
       "{\\providefontfamily\\cdadaremoji{Noto Emoji}}"
       "{\\providefontfamily\\cdadaremoji{Symbola}}\n"
       (mapconcat (lambda (pair)
                    (format "\\newunicodechar{%c}{{%s %c}}"
                            (car pair) (cdr pair) (car pair)))
                  chars "\n")))))

(defun cdadar/org-latex-add-glyph-fallback (text backend info)
  "Redirect characters the main font lacks to a font that has them.
The Times-like main font has no accented Greek, circled numbers, box drawing
characters or geometric shapes, and XeTeX has no automatic font fallback, so
they would come out blank; only the characters actually present in TEXT are
remapped.  Requires fontspec, hence the guard.  BACKEND and INFO follow the Org
export filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "fontspec" text)
           (not (string-match-p cdadar/org-latex-glyph-fallback-marker text)))
      (let ((snippet (cdadar/org-latex-glyph-fallback-snippet text)))
        (if snippet
            (replace-regexp-in-string
             (regexp-quote "\\begin{document}") (concat snippet "\n\\begin{document}")
             text t t)
          text))
    text))

(defconst cdadar/org-latex-cjk-autofallback-marker "cdadar-cjk-autofallback"
  "标记 xeCJK 缺字回退那行由导出过滤器注入，便于在导出的 .tex 里找到它。")

(defun cdadar/org-latex-cjk-autofallback-snippet (text)
  "Return the xeCJK fallback setup for TEXT.
AutoFallBack 让当前 CJK 字体族缺字时换用回退字体；两个
`\\setCJKfallbackfamilyfont' 把 emoji 字体接到回退链上——变体选择符 U+FE00-FE0F
被 xeCJK 归入 CM 类，`\\newunicodechar' 对它无效，只能靠字体回退。
TEXT 里出现 quotecjkfont（quote 用楷体）时额外给那个字体族也接上。"
  (concat
   (format "%% %s\n" cdadar/org-latex-cjk-autofallback-marker)
   "\\xeCJKsetup{AutoFallBack=true}\n"
   "\\IfFontExistsTF{Noto Emoji}"
   "{\\setCJKfallbackfamilyfont{\\CJKrmdefault}{Noto Emoji}}"
   "{\\setCJKfallbackfamilyfont{\\CJKrmdefault}{Symbola}}\n"
   (when (string-match-p "quotecjkfont" text)
     (concat "\\IfFontExistsTF{Noto Emoji}"
             "{\\setCJKfallbackfamilyfont{quotecjkfont}{Noto Emoji}}"
             "{\\setCJKfallbackfamilyfont{quotecjkfont}{Symbola}}\n"))))

(defconst cdadar/org-latex-variation-selectors
  "[\uFE00-\uFE0F\U000E0100-\U000E01EF]"
  "变体选择符（VS1-VS16 与补充区）：只影响 emoji 的呈现，排版时直接删掉。
它们被 xeCJK 归入 CM（组合标记）类，`\\newunicodechar' 对它无效；删掉既不用
依赖某个字体恰好有这几个零宽字形，也不影响字形（emoji 本身由 cdadaremoji 映射
负责）。")

(defun cdadar/org-latex-add-cjk-autofallback (text backend info)
  "Let xeCJK substitute glyphs missing from the CJK family in use.
Quote environments switch to Kaiti SC (see `cdadar/org-latex-cjk-quote-font'),
and Kaiti SC has no glyph for the full-width middle dot ・(U+30FB) -- the frame
in such a note then shows a tofu box.  Turning on xeCJK's own fallback replaces
such glyphs with the note's body CJK font.  A `\\newunicodechar' mapping cannot
fix this: the middle dot is an xeCJK MiddlePunct character, so xeCJK picks the font again
inside the replacement text.  BACKEND and INFO follow the Org export filter
protocol."
  (ignore info)
  (if (not (org-export-derived-backend-p backend 'latex))
      text
    ;; 变体选择符先删（CM 类字符只能靠字体回退，而回退字体未必有这两个零宽字形）
    (let ((text (replace-regexp-in-string cdadar/org-latex-variation-selectors
                                          "" text t t)))
      (if (and (string-match-p "xeCJK" text)
               ;; 笔记自己的 preamble（模板 snippet）可能已经写了这一行
               (not (string-match-p "AutoFallBack" text))
               (string-match-p (regexp-quote "\\begin{document}") text))
          (replace-regexp-in-string
           (regexp-quote "\\begin{document}")
           (concat (cdadar/org-latex-cjk-autofallback-snippet text)
                   "\n\\begin{document}")
           text t t)
        text))))

(defconst cdadar/org-circled-number-regexp
  (format "[%c-%c]" #x2460 #x2473)
  "匹配 ①..⑳（U+2460..U+2473）。")

(defun cdadar/org-replace-circled-numbers ()
  "Replace ①..⑳ in the current Org buffer with `\\textcircled{N}'.
笔记里本来就到处用 `\\textcircled{1}'，这个命令把从别处粘来的圈号字符统一成同一写法。
跳过 `#' 开头的行（关键字、注释、块分隔符；笔记 header 可能用 `\\newunicodechar'
映射过这个字符，改了会破坏映射）与 src / example 块内容；整体算一次 undo。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((count 0))
    (atomic-change-group
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cdadar/org-circled-number-regexp nil t)
          (let ((char (char-after (match-beginning 0))))
            (unless (or (save-excursion (forward-line 0)
                                        (looking-at-p "[[:blank:]]*#"))
                        (org-in-block-p '("src" "example")))
              (replace-match (format "\\textcircled{%d}" (- char #x245F)) t t)
              (setq count (1+ count)))))))
    (message "圈号 → \\textcircled{}：替换 %d 处" count)
    count))

(defconst cdadar/org-hebrew-char-regexp
  (format "[%c-%c]" #x0590 #x05FF)
  "匹配希伯来文块：字母、元音符号 niqqud（U+05B0-05BD 等）与读经记号。")

(defconst cdadar/org-greek-char-regexp
  (format "[%c-%c%c-%c]" #x0370 #x03FF #x1F00 #x1FFF)
  "匹配希腊文与扩展区（多音符号，如 ἀγαπάω、ἐκκλησία）。")

(defun cdadar/org-script-run-regexp (char-regexp)
  "Return a regexp matching runs of CHAR-REGEXP separated by blanks.
一段 = 连续的该文种字符，可跨空白（不跨换行）继续，让 `בָּרוּךְ אַתָּה'
与 `μακάριοι οἱ καθαροί' 包成同一个宏参数；首尾空白留在匹配之外，不会吃掉
后面中文。"
  (format "\\(?:%s\\)\\(?:[[:blank:]]*\\(?:%s\\)\\)*" char-regexp char-regexp))

(defun cdadar/org-script-run-wrapped-p (macros)
  "Return non-nil when the current match already sits inside MACROS{...}.
MACROS 是该文种可接受的宏名（笔记自家简写与 polyglossia 写法），已包过的
不再叠一层；命令要能反复跑（笔记是一直加内容的）。
先取下 match 两端：`looking-back' 自己会写 match data，之后再取 match-end 就是
它自己那个匹配的了。"
  (save-match-data
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (open (concat "\\(?:" (regexp-opt macros) "\\){")))
      (and (save-excursion
             (goto-char beg)
             (looking-back open (max (point-min) (- beg 16))))
           (save-excursion
             (goto-char end)
             (looking-at-p "}"))))))

(defun cdadar/org-script-macro-for (shorthand)
  "Return the macro to wrap a script with in the current buffer, or nil.
前言里定义了 SHORTHAND（`\\heb' 这个简写，模板都这么写）才用它；没定义就返回
nil——这类老笔记里既没有简写也没有 polyglossia，硬包上去导出必报
`Undefined control sequence'（实测 3 篇如此；它们的希伯来文/希腊文靠缺字回退
照常渲染），由调用者跳过并在 message 里报出来。
判断不写反斜杠正则：`{\\heb}' 这个字面串只在定义处出现，用 `regexp-quote' 拼。"
  (when (string-match-p (concat "{" (regexp-quote shorthand) "}") (buffer-string))
    shorthand))

(defun cdadar/org-wrap-script-runs (char-regexp macros)
  "Wrap runs of CHAR-REGEXP with the first of MACROS in the current buffer.
MACROS 是同一文种的可接受宏名（自己的简写在前，polyglossia 写法在后）：已经在
其中任何一个里的段落不再包。跳过 `#' 开头的行与 src / example / export 块。
起点/终点先取出来再替换：中间那些检查会跑搜索，`replace-match' 靠的 match
data 靠不住。返回包的个数。"
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (let ((re (cdadar/org-script-run-regexp char-regexp)))
        (while (re-search-forward re nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (run (match-string 0)))
            (unless (or (save-excursion (forward-line 0)
                                        (looking-at-p "[[:blank:]]*#"))
                        (org-in-block-p '("src" "example" "export"))
                        (cdadar/org-script-run-wrapped-p macros))
              (delete-region beg end)
              (insert (car macros) "{" run "}")
              (setq count (1+ count)))))))
    count))

(defun cdadar/org-wrap-hebrew-greek ()
  "Wrap every Hebrew and Greek run in the current Org buffer with `\\heb{}' / `\\gr{}'.
从别处粘来的经文常常是裸字符（没包宏），导出时只能落在主字体上，靠
`cdadar/org-latex-add-glyph-fallback' 的缺字回退兜底。这个命令把正文里的
希伯来文/希腊文按文种包成笔记自家前言的 `\\heb{}' / `\\gr{}'（= `\\texthebrew' /
`\\textgreek'）：字体、RTL、字体缩放都交给 polyglossia，源码也不再依赖导出过滤器。
前言里没有 `\\newcommand{\\heb}' / `\\newcommand{\\gr}' 时跳过该文种并在 message 里
报出来：那类笔记没有可用的宏，硬包只会让导出报 Undefined control sequence。
跳过 `#' 开头的行（关键字、注释、块分隔符；header 里就是 `\\heb{}' 的定义与例子）、
src / example / export 块，以及已经包过的（含 `\\hebl{}' / `\\grl{}' /
`\\texthebrew{}' / `\\textgreek{}'）；整体算一次 undo，返回包的个数。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((count 0) (skipped nil))
    (atomic-change-group
      (dolist (spec (list (list cdadar/org-hebrew-char-regexp "希伯来文" "\\heb"
                                '("\\heb" "\\hebl" "\\texthebrew"))
                          (list cdadar/org-greek-char-regexp "希腊文" "\\gr"
                                '("\\gr" "\\grl" "\\textgreek"))))
        (let ((macro (cdadar/org-script-macro-for (nth 2 spec))))
          (if macro
              (setq count (+ count (cdadar/org-wrap-script-runs
                                    (nth 0 spec) (cons macro (nth 3 spec)))))
            (push (nth 1 spec) skipped)))))
    (message "希伯来文/希腊文 → \\heb{} / \\gr{}：包了 %d 处%s" count
             (if skipped
                 (format "；%s 跳过（前言没有对应的 \\heb / \\gr 定义）"
                         (mapconcat #'identity (nreverse skipped) "、"))
               ""))
    count))

(defun cdadar/org-latex-drop-duplicate-documentclass (text backend info)
  "Drop the extra `\\documentclass' lines notes add on top of their class.
Notes written before `#+LATEX_CLASS_OPTIONS' habitually declare the class again
in a `#+LATEX_HEADER' line; LaTeX rejects that with \"Two \\documentclass or
\\documentstyle commands\" and then produces no PDF.  The declaration Org emits
from `org-latex-classes' is kept, and later declarations of the same class are
dropped, options included -- class options belong in `#+LATEX_CLASS_OPTIONS'.
BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point-min)
             (or (and (re-search-forward (regexp-quote "\\begin{document}") nil t)
                      (match-beginning 0))
                 (point-max)))
            (goto-char (point-min))
            (let ((re (concat (regexp-quote "\\documentclass")
                              "\\[[^]]*\\]?" "{\\([^}]*\\)}")))
              (when (re-search-forward re nil t)
                (let ((class (match-string 1)))
                  (while (re-search-forward re nil t)
                    (when (string= class (match-string 1))
                      (delete-region (line-beginning-position)
                                     (min (point-max) (1+ (line-end-position)))))))))))
        (buffer-string))
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
  "Normalize line breaks inside LaTeX quote environments in exported TEXT.
A blank line after an Org line break becomes an explicit vertical skip, and the
line break on the last quote line is dropped: the quote environment already
ends the paragraph, so keeping it only adds an empty line (the old
negative-vspace workaround over-corrected that into overlapping text).  Notes
that still carry that workaround in their own quote environment redefinition
have it dropped from the preamble, so they stop overlapping without edits.
INFO is ignored."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point-min)
             (or (and (re-search-forward (regexp-quote "\\begin{document}") nil t)
                      (match-beginning 0))
                 (point-max)))
            (goto-char (point-min))
            (let ((re (concat "\\(" (regexp-quote "\\endlist") "\\)"
                              (regexp-quote "\\vspace")
                              "{[-][[:blank:]]*"
                              (regexp-quote "\\baselineskip")
                              "[[:blank:]]*}")))
              (while (re-search-forward re nil t)
                (replace-match "\\1" t)))))
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote "\\begin{quote}") nil t)
          (let ((quote-start (match-beginning 0)))
            (when (re-search-forward (regexp-quote "\\end{quote}") nil t)
              (let ((quote-end-marker (copy-marker (match-end 0))))
                (save-restriction
                  (narrow-to-region quote-start quote-end-marker)
                  (goto-char (point-min))
                  (while (search-forward "\\\\\n\n" nil t)
                    (replace-match "\\\\[0.6\\baselineskip]\n" t t))
                  (goto-char (point-max))
                  (when (re-search-backward
                         (concat (regexp-quote "\\\\") "[ \t]*\n[ \t]*"
                                 (regexp-quote "\\end{quote}"))
                         nil t)
                    (replace-match "\n\\end{quote}" t t))
                  (widen))
                (goto-char quote-end-marker)
                (set-marker quote-end-marker nil)))))
        (buffer-string))
    text))

(defun cdadar/org-beamer-headline-as-text (orig-fun headline contents info)
  "Beamer 导出：把 frame level 以下的标题输出为带层级的粗体文本，替代不可跨页的 block 环境。

一级小节标题（frame-level + 1，如 H:1 时的二级、H:2 时的三级）用 \\large 粗体，
更深级标题用正常字号粗体，从而保留标题层级视觉差异；同时 `allowframebreaks' 可以
在段落间正常分页，内容不会被裁剪隐藏。
以 :around advice 包裹 `org-beamer-headline'，参数遵循其 transcoder 协议。"
  (if (not (org-export-derived-backend-p (plist-get info :back-end) 'beamer))
      (funcall orig-fun headline contents info)
    (let* ((level (org-export-get-relative-level headline info))
           (frame-level (org-beamer--frame-level headline info)))
      (if (<= level frame-level)
          (funcall orig-fun headline contents info)
        (let ((title (org-export-data (org-element-property :title headline) info)))
          (if (= level (1+ frame-level))
              (format "\n\\par\\medskip\\textbf{\\large %s}\\par\n\n%s" title (or contents ""))
            (format "\n\\par\\smallskip\\textbf{%s}\\par\n\n%s" title (or contents ""))))))))

(defun cdadar/org-beamer-document-p ()
  "Return non-nil when the current buffer is a Beamer presentation.

Ask the `latex' backend for the document's class on purpose: the beamer
backend defaults `:latex-class' to the beamer class name, which would report
every plain note that declares no class at all as a presentation."
  (and (require 'ox-beamer nil t)
       (string-prefix-p "beamer"
                        (or (plist-get (org-export-get-environment 'latex)
                                       :latex-class)
                            ""))))

(defun cdadar/org-latex-export-to-pdf-route (orig-fun &rest args)
  "Export a Beamer presentation with the Beamer backend, else call ORIG-FUN.

Frames only exist in the Beamer backend.  Exporting a presentation through
the plain LaTeX backend silently turns every frame into `\\section*', which
drops the slide structure and breaks the metropolis theme."
  (if (cdadar/org-beamer-document-p)
      (apply #'org-beamer-export-to-pdf args)
    (apply orig-fun args)))

(defun cdadar/org-latex-export-to-latex-route (orig-fun &rest args)
  "Export a Beamer presentation as Beamer LaTeX, else call ORIG-FUN."
  (if (cdadar/org-beamer-document-p)
      (apply #'org-beamer-export-to-latex args)
    (apply orig-fun args)))

(defun cdadar/org-latex-export-to-pdf-async ()
  "Asynchronously export the current Org buffer to PDF.
`org-latex-export-to-pdf' is routed to the Beamer backend for presentations
by `cdadar/org-latex-export-to-pdf-route'."
  (interactive)
  (org-latex-export-to-pdf t))

(defun cdadar/org-latex-fix-multicolumn-trailing-ampersand (text backend info)
  "Remove trailing `&' after `\\multicolumn' when it spans all columns.
Org pads single-cell rows to the full column count, adding an extra
`&' after `\\multicolumn{N}{...}{...}' which creates a column-count
mismatch in LaTeX.  INFO is ignored."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\multicolumn{\\([0-9]+\\)}{\\([^}]*\\)}{\\([^}]*\\)} & \\\\\\\\"
       "\\\\multicolumn{\\1}{\\2}{\\3} \\\\\\\\"
       text)
    text))

(use-package ox
  :ensure nil
  :after org
  :config
  (require 'ox-md nil t)
  (require 'ox-latex nil t)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-use-english-main-font)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-use-installed-fonts)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-default-cjk-font)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-add-glyph-fallback)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-add-cjk-autofallback)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-drop-duplicate-documentclass)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-inject-quote-cjk-font)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-bibleref-compat)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-quote-paragraph-spacing)
  (add-to-list 'org-export-filter-final-output-functions
               #'cdadar/org-latex-fix-multicolumn-trailing-ampersand)
  (advice-add 'org-latex-export-to-pdf :around
              #'cdadar/org-latex-export-to-pdf-route)
  (advice-add 'org-latex-export-to-latex :around
              #'cdadar/org-latex-export-to-latex-route)
  (with-eval-after-load 'ox-beamer
    (setq org-beamer-frame-default-options "allowframebreaks")
    (advice-add 'org-beamer-headline :around #'cdadar/org-beamer-headline-as-text)))

(use-package ox-latex
  :ensure nil
  :after ox
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -xelatex -interaction=nonstopmode -outdir=%o %f")
     '("xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"))))

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

;; org 的强调边界写死在 `org-element--parse-generic-emphasis'（org 9.8）里：开标记前
;; 只能是空白、`-'、ASCII 括号与引号、`{' 或行首，闭标记后只能是空白、`-'、ASCII
;; 句读与括号或行尾。中文笔记里的 `说*重点*好啊'、`2. *结论*：…' 紧贴汉字，org 既不
;; 加粗也不导出；`org-emphasize'（C-c e）只补它自己新插入的标记，补不了写好的文字。
(defconst cdadar/org-emphasis-markers "*_=~"
  "`cdadar/org-fix-emphasis-boundaries' 处理的强调标记。
`/' 不在内：中文笔记里 `/` 几乎都是路径与并列分隔符（`图像/视频'、`视频_素材/杂项'），
按斜体处理会大量误判。
`+' 也不在内，这是实测的：近 120 天笔记里成对且紧贴汉字的 `+' 只有 4 处，全部是
「加号 / 连接符」而不是删除线（`.rtf 14.9万+.doc 11.8万'、`（杂项+待定+按格式_PDF）'），
补空格会把这些文字真的变成删除线；同一批语料里 `*' 命中 261 行、`=' 312 行、`~' 5 行，
命中的全是紧贴全角标点的强调。要用 `+' 就把它加回这个字符串。")

(defconst cdadar/org-emphasis-opening-chars "-('\"{"
  "开标记前 org 认的 ASCII 边界字符（其余合法边界是空白与行首）。")

(defconst cdadar/org-emphasis-closing-chars "-.,;:!?'\")}\\["
  "闭标记后 org 认的 ASCII 边界字符（其余合法边界是空白与行尾）。")

(defconst cdadar/org-emphasis-repairable-types
  '(paragraph headline table-cell item quote-block center-block verse-block)
  "org 会解析强调对象的节点类型。
其余节点里的标记不是强调或不能动：src / example / export 块、comment、fixed-width
行、链接（含中文文件路径）、verbatim / code、LaTeX 片段、`#+NAME:' 这类关键字。")

(defun cdadar/org-emphasis-boundary-p (char chars)
  "Return non-nil when CHAR is a valid org emphasis border.
CHARS are the ASCII characters org accepts on that side; every other valid
border is whitespace -- including U+3000, which Emacs gives whitespace syntax
-- or the beginning / end of the buffer."
  (or (null char)
      (memq char (string-to-list chars))
      (eq (char-syntax char) ?\s)))       ; ?\s 就是空格字符，U+3000 也是这个语法类

(defun cdadar/org-emphasis-repairable-p (marker content before after bad-left bad-right)
  "Return non-nil when a broken marker pair is emphasis worth a space.
MARKER is the emphasis character, CONTENT what sits between the two markers,
BEFORE / AFTER the characters around the pair, BAD-LEFT / BAD-RIGHT whether
that side fails `cdadar/org-emphasis-boundary-p'.  Point must be at the first
character of CONTENT.
Only a border blocked by a non-ASCII character counts as Chinese emphasis:
all-ASCII pairs (`a*b*c', `sdk=[34,35] ... sdk=34') stay untouched, as do
marker runs (`**bold**', which org parses on its own)."
  (and (or bad-left bad-right)
       (or (and bad-left before (> before 127))
           (and bad-right after (> after 127)))
       (not (memq marker (list before after)))
       (not (memq marker (string-to-list content)))
       (memq (org-element-type (org-element-context))
             cdadar/org-emphasis-repairable-types)))

(defun cdadar/org-fix-emphasis-boundaries (beg end)
  "给因缺少边界空格而失效的 org 强调标记补空格。
org 的强调边界写死在 `org-element--parse-generic-emphasis' 里，中文笔记里紧贴汉字的
标记（`说*重点*好啊'、`—*结论*：…'）既不显示也不导出为粗体，而 `org-emphasize'
（C-c e）只补它自己新插入的标记。在 BEG..END（有选区时是选区，否则整个 buffer）
内找这种标记，在紧贴非 ASCII 字符的那一侧补一个空格，与 `org-emphasize' 补的一致。
只动 org 会解析强调的地方，且只动中文相关的边界：`a*b*c'、`sdk=[34,35] … sdk=34' 这类全
ASCII 的算式与标识符不动，src / example / export 块、链接路径、verbatim / code、LaTeX
片段、`#+NAME:' 这类关键字里的标记也不动。整条命令算一次 undo，可以反复跑（修好的不再
是候选）；处理哪几个标记见 `cdadar/org-emphasis-markers'。返回补过的标记对数。"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((regexp (format "\\([%s]\\)\\([^[:space:]][^\n]*?[^[:space:]]\\)\\1"
                        cdadar/org-emphasis-markers))
        (end (copy-marker end t))
        (pairs 0)
        (spaces 0))
    (atomic-change-group
      (save-excursion
        (goto-char beg)
        (while (re-search-forward regexp end t)
          (let* ((marker (string-to-char (match-string-no-properties 1)))
                 (content (match-string-no-properties 2))
                 (open-beg (match-beginning 1))
                 (before (char-before open-beg))
                 (after (char-after (match-end 0)))
                 (bad-left (not (cdadar/org-emphasis-boundary-p
                                 before cdadar/org-emphasis-opening-chars)))
                 (bad-right (not (cdadar/org-emphasis-boundary-p
                                  after cdadar/org-emphasis-closing-chars)))
                 ;; 搜索点跟着插入的文本往后走，保证循环推进；`org-element-context'
                 ;; 会冲掉 match data，所以位置先存下来。
                 (resume (copy-marker (match-end 0) t)))
            (when (save-excursion
                    (goto-char (1+ open-beg))
                    (cdadar/org-emphasis-repairable-p
                     marker content before after bad-left bad-right))
              ;; 先补右边界再补左边界，前面的位置不受影响。
              (when bad-right
                (goto-char resume)
                (insert " ")
                (setq spaces (1+ spaces)))
              (when bad-left
                (goto-char open-beg)
                (insert " ")
                (setq spaces (1+ spaces)))
              (setq pairs (1+ pairs)))
            (goto-char resume)
            (set-marker resume nil)))
        (set-marker end nil)))
    (message "强调边界补空格：%d 处标记、%d 个空格" pairs spaces)
    pairs))

;;; --- Related packages ---

(use-package writeroom-mode
  :if (locate-library "writeroom-mode"))


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

(use-package org-count-words
  :vc (:url "https://github.com/Elilif/org-count-words" :rev :newest)
  :after org
  :commands (org-count-words-buffer org-count-words-region org-count-words-subtree org-count-words-mode)
  :config
  ;; 统计口径改为「非空格字符数」（同 advance-words-count 的 Ns），
  ;; 这样 1:1 算 3 个字符而非 1 个词。
  (defun cdadar/org-count-words-chars (start end)
    "Count non-space characters between START and END."
    (let ((count 0))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (and (< (point) end)
                      (re-search-forward "[^[:space:]]" end t))
            (setq count (1+ count)))))
      count))
  (setq org-count-words-function #'cdadar/org-count-words-chars)
  ;; latex-fragment：只数宏参数内的字符（\gr{...} 算 … 内的内容），
  ;; 宏名与括号不计。段落已统计整段，这里用负偏移抵消宏名/括号部分。
  (defun cdadar/org-count-words-latex-fragment (element)
    "Count non-space chars in the first {...} argument of a LaTeX fragment,
offsetting the macro-name/brace chars already counted by the paragraph."
    (let* ((beg (org-element-property :begin element))
           (end (org-element-property :end element))
           (text (buffer-substring-no-properties beg end))
           (total (cdadar/org-count-words-chars beg end)))
      (if (string-match "{\\([^}]*\\)}" text)
          (- (cdadar/org-count-words-chars (+ beg (match-beginning 1))
                                           (+ beg (match-end 1)))
             total)
        (- total))))
  (setf (alist-get 'latex-fragment org-count-words-element-functions)
        #'cdadar/org-count-words-latex-fragment)
  ;; heading 是否计入可切换：有时要标题文字，有时不要。
  (defvar cdadar/org-count-words-include-headings t
    "Non-nil counts headline text in org-count-words.")
  (defun cdadar/org-count-words-set-headings (include)
    "Add or remove `headline' from `org-count-words-elemnts' for INCLUDE."
    (if include
        (add-to-list 'org-count-words-elemnts 'headline)
      (setq org-count-words-elemnts (delq 'headline org-count-words-elemnts))))
  (defun cdadar/org-count-words-toggle-headings ()
    "Toggle whether org-count-words counts headline text."
    (interactive)
    (setq cdadar/org-count-words-include-headings
          (not cdadar/org-count-words-include-headings))
    (cdadar/org-count-words-set-headings cdadar/org-count-words-include-headings)
    (message "org-count-words: %s headings"
             (if cdadar/org-count-words-include-headings "counting" "ignoring")))
  ;; 标题统计从星号之后开始，结构性的 `*` 不计入。
  (defun cdadar/org-count-words-headline (element)
    "Count chars in headline ELEMENT text, excluding leading stars/spaces."
    (let* ((beg (org-element-property :begin element))
           (end (or (org-element-property :contents-begin element)
                    (org-element-property :end element))))
      (save-excursion
        (goto-char beg)
        (skip-chars-forward "* \t")
        (when (< (point) end)
          (cdadar/org-count-words-chars (point) end)))))
  (setf (alist-get 'headline org-count-words-element-functions)
        #'cdadar/org-count-words-headline))

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
;; Loaded at top level (not inside use-package :config) so it is defined
;; before `org-roam-capture-templates' evaluates below.
(defun cdadar/org-capture-snippet-content (name)
  "Return the body of yasnippet NAME (org-mode) for use in capture templates.

Reads the snippet file from `user-emacs-directory'/snippets/org-mode/, strips
the metadata header (everything before the `# --' separator) and yas
`$[0-9]+' field/exit markers.  `%' chars are left untouched: org-capture
only treats `%' followed by a placeholder letter (or `(', `[', `<') as
escape sequences, and this snippet's `%'s are all LaTeX comment markers
followed by space/CJK text."
  (let ((file (expand-file-name
               name
               (expand-file-name "org-mode"
                                 (expand-file-name "snippets" user-emacs-directory)))))
    (unless (file-exists-p file)
      (user-error "yasnippet not found: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^# --$" nil t)
        (delete-region (point-min) (point)))
      (let ((s (buffer-string)))
        (setq s (replace-regexp-in-string "\\$[0-9]+\\b" "" s))
        (setq s (string-trim-left s "\n"))
        s))))

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
     ("cs" "查经" plain (function (lambda () (concat "%?\n" (cdadar/org-capture-snippet-content "bible study"))))
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
  :bind (:map org-agenda-mode-map
              ("C-c C-r" . org-review-insert-last-review)))

(use-package org-fragtog
  :hook ((org-mode . org-fragtog-mode)))


(use-package org-pandoc-import
  :commands (org-pandoc-import-as-org
             org-pandoc-import-to-org
             org-pandoc-import-transient-mode)
  :vc (:url "https://github.com/tecosaur/org-pandoc-import"
            :rev :newest))


;; === Markdown → Org from clipboard ===
(defun cdadar/md-clipboard-to-org (&optional insert-p)
  "Convert markdown text to Org, replacing the clipboard with the result.

Source of the markdown:
- If the region is active, the region's text.
- Otherwise, the clipboard (pasteboard on macOS).

The converted Org text replaces the clipboard (and primary selection);
with a prefix argument (or INSERT-P non-nil) it is also inserted at
point in the current buffer.

Uses pandoc:  pandoc -f markdown -t org --wrap=preserve
When org-pandoc-import is installed, its `_simple-headers.lua' filter is
reused to drop pandoc's CUSTOM_ID properties and rewrite internal links."
  (interactive "P")
  (unless (executable-find "pandoc")
    (user-error "pandoc is not installed (brew install pandoc)"))
  (let* ((opi-el (locate-library "org-pandoc-import"))
         (simple-headers (and opi-el
                              (expand-file-name "filters/_simple-headers.lua"
                                                (file-name-directory opi-el))))
         (pandoc-args (append (list "-f" "markdown" "-t" "org" "--wrap=preserve")
                              (when (and simple-headers (file-exists-p simple-headers))
                                (list "--lua-filter" simple-headers))))
         (input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (gui-get-selection 'CLIPBOARD)))
         (org-text nil))
    (unless (and input (string-match-p "[^[:space:]]" input))
      (user-error "No markdown text found in region or clipboard"))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (unless (zerop (apply #'call-process-region (point-min) (point-max)
                            "pandoc" t t nil pandoc-args))
        (error "pandoc conversion failed"))
      (setq org-text (string-trim-right (buffer-string) "\n")))
    (if (display-graphic-p)
        (progn
          (gui-set-selection 'CLIPBOARD org-text)
          (gui-set-selection 'PRIMARY org-text))
      (kill-new org-text))
    (when insert-p
      (insert org-text))
    (message "markdown → org: %d chars, clipboard updated" (length org-text))))

;; GitHub-flavored Markdown export (`C-c C-e g g').
;; The dispatch menu is built from `org-export-registered-backends', so
;; loading the backend is all that is needed: putting `gfm' into
;; `org-export-backends' would run its Customize `:set', which drops every
;; registered backend that is not named in the list (org-ref's, for one).
(use-package ox-gfm
  :demand t
  :after org)

;; Keep the table of contents under a `:TOC:'-tagged heading up to date on save
(use-package toc-org
  :hook org-mode)

;; Variable pitch for prose, fixed pitch for code and tables in one buffer.
;; Complements `prose-mode', which already centers the text through
;; `writeroom-mode' (so `olivetti' is intentionally not used here).
(use-package mixed-pitch
  :hook (prose-mode . mixed-pitch-mode))

(provide 'init-org)
;;; init-org.el ends here
