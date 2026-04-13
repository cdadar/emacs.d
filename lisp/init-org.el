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
         :map org-mode-map
         ("C-M-<up>" . org-up-element)
         ("C-c s c" . cdadar/org-screenshot))
  :mode ("\\.\\(org\\|org_archive\\)\\'" . org-mode)
  :hook ((org-mode . cdadar/org-mode-setup)
         (org-agenda-mode . hl-line-mode)
         (org-agenda-after-show . org-show-entry)
         (org-agenda-finalize . my/org-agenda-insert-efforts))
  :custom
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-export-allow-bind-keywords t)
  (org-enforce-todo-dependencies nil)
  (org-enforce-todo-checkbox-dependencies nil)
  (org-image-actual-width '(400))
  (org-tags-column 80)
  (org-deadline-warning-days 30)
  (org-lowest-priority 68)
  (org-support-shift-select t)
  (org-refile-use-cache nil)
  (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  (org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                           "xelatex -interaction nonstopmode -output-directory %o %f"
                           "xelatex -interaction nonstopmode -output-directory %o %f"))
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
  :config
  (defun cdadar/org-mode-setup ()
    (setq truncate-lines t))

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
          (set (make-local-variable 'blink-cursor-interval) 0.6)
          (set (make-local-variable 'show-trailing-whitespace) nil)
          (set (make-local-variable 'line-spacing) 0.2)
          (set (make-local-variable 'electric-pair-mode) nil)
          (ignore-errors (flyspell-mode 1))
          (when (fboundp 'visual-line-mode)
            (visual-line-mode 1)))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (when (fboundp 'visual-line-mode)
        (visual-line-mode -1))
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0))))

  (defun cdadar/org-screenshot ()
    "Take a screenshot into a unique-named file and insert an Org file link."
    (interactive)
    (org-display-inline-images)
    (let ((filename
           (concat
            (make-temp-name
             (concat (file-name-nondirectory (buffer-file-name))
                     "_imgs/"
                     (format-time-string "%Y%m%d_%H%M%S_")))
            ".png")))
      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename) t))
      (make-frame-invisible nil t)
      (when (and (boundp '*is-a-mac*) *is-a-mac*)
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\""))
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"") (concat "\"" filename "\"")))
      (when (and (boundp '*linux*) *linux*)
        (call-process "import" nil nil nil filename))
      (make-frame-visible)
      (when (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
      (org-display-inline-images)))

  (defun cdadar/org-setup-agenda-window-hooks ()
    (with-eval-after-load 'org-agenda
      (add-hook 'org-agenda-mode-hook
                (lambda ()
                  (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))))

  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (defun cdadar/org-setup-clocking ()
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (org-clock-persistence-insinuate)
    (with-eval-after-load 'org-clock
      (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
      (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))
    ;; (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
    ;; (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
    ;; (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)
    (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
      (add-hook 'org-clock-in-hook
                (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                         (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
      (add-hook 'org-clock-out-hook
                (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                         "tell application \"org-clock-statusbar\" to clock out")))))

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

  (defun cdadar/org-setup-crypt ()
    (require 'org-crypt nil t)
    (when (featurep 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-crypt-tag-matcher "secret"
            org-tags-exclude-from-inheritance '("secret")
            org-crypt-key "6DF1ABB0"
            epg-pinentry-mode 'loopback)
      ;; 有解密条目的 buffer 跳过自动保存，避免 super-save 立刻重新加密
      (defvar-local cdadar/org-crypt-decrypting nil
        "Non-nil when an org-crypt entry has been decrypted in this buffer.")

      (defadvice org-decrypt-entry (after cdadar/org-decrypt-mark activate)
        (setq-local cdadar/org-crypt-decrypting t))

      (defadvice org-encrypt-entry (after cdadar/org-encrypt-unmark activate)
        (unless (save-excursion
                  (goto-char (point-min))
                  (re-search-forward
                   (concat "^\\*+[ \t].*:" org-crypt-tag-matcher ":") nil t))
          (kill-local-variable 'cdadar/org-crypt-decrypting)))))

  (defun cdadar/org-setup-export ()
    (with-eval-after-load 'ox
      (require 'ox-md nil t)
      (require 'ox-latex nil t))
    (with-eval-after-load 'ox-latex
      (add-to-list 'org-latex-classes
                   '("beamer"
                     "\\documentclass[presentation]{beamer}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

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

  (defun cdadar/org-setup-refile ()
    (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

  (defun cdadar/org-archive-done-tasks ()
    "archive of DNONE AND CANCELLED in current buffer"
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

  (defun cdadar/org-setup-agenda ()
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                  (sequence "WAITING(w@/!)" "TESTING(T!)" "PUBLISH(P!)"  "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
          org-todo-repeat-to-state "NEXT"
          org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
          org-todo-keyword-faces
          (quote (("NEXT" :inherit warning)
                  ("PROJECT" :inherit font-lock-string-face)))
          org-tag-alist
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
            ("book" . ?B))
          org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          org-agenda-skip-scheduled-delay-if-deadline t
          org-agenda-sorting-strategy
          '((agenda habit-down time-up deadline-up scheduled-up effort-up category-keep)
            (todo priority-down category-up effort-up)
            (tags priority-down category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
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
                                 (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
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
            ("cv" "video" tags-todo "video"))
          org-archive-mark-done nil
          org-archive-location "%s_archive::datetree/"))

  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

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

  (defun cdadar/org-setup-global-prefix ()
    (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
    (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
    (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
    (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
    (define-key sanityinc/org-global-prefix-map (kbd "r") 'org-clock-report)
    (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)
    (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)
                  org-stuck-projects '("-INBOX/PROJECT" ("NEXT"))))

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
           h m file item prompt donesomething)
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
    (when (and (boundp '*is-a-mac*) *is-a-mac*)
      (define-key org-mode-map (kbd "M-h") nil)))

  (cdadar/org-setup-agenda-window-hooks)
  (cdadar/org-setup-clocking)
  (cdadar/org-setup-babel)
  (cdadar/org-setup-crypt)
  (cdadar/org-setup-export)
  (cdadar/org-setup-refile)
  (cdadar/org-setup-agenda)
  (cdadar/org-setup-global-prefix)
  (cdadar/org-setup-capture)
  (cdadar/org-setup-ui))

(use-package writeroom-mode
  :diminish writeroom-mode)


(use-package grab-mac-link
  :if (bound-and-true-p *is-a-mac*)
  :commands (grab-mac-link grab-mac-link-dwim)
  :bind (:map org-mode-map
              ("C-c g" . grab-mac-link)))

(use-package org-pomodoro
  :after org
  :bind
  ([(meta p)] . org-pomodoro)
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  :hook
  ((org-pomodoro-finished . (lambda () (cdadar/org-pomodoro-notify "Pomodoro completed!" "Time for a break.")))
   (org-pomodoro-break-finished . (lambda () (cdadar/org-pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?")))
   (org-pomodoro-long-break-finished . (lambda () (cdadar/org-pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?")))
   (org-pomodoro-killed . (lambda () (cdadar/org-pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!"))))
  :config
  (defun cdadar/org-pomodoro-notify (title message)
    (call-process "notify-send" nil 0 nil title message)))

(use-package org-cliplink
  :after org
  :bind
  (("C-c l" . org-store-link) ("C-c a" . org-agenda)))

;; create ppt
(use-package ox-ioslide
  :commands (org-ioslide-export-as-html org-ioslide-export-to-html))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-mind-map
  :commands (org-mind-map-write
             org-mind-map-write-with-prompt
             org-mind-map-write-current-branch
             org-mind-map-write-current-tree)
  :init
  (setq org-mind-map-engine "dot") ; Default. Directed Graph
  :config
  (defun cdadar/org-mind-map-setup ()
    (require 'ox-org)
    ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
    ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
    ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
    ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
    ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
    ;; (setq org-mind-map-engine "circo")  ; Circular Layout
    )
  (cdadar/org-mind-map-setup))

(use-package org-brain
  :after org
  :custom
  (org-id-track-globally t)
  (org-id-locations-file (locate-user-emacs-file ".org-id-locations"))
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 12)
  (org-brain-include-file-entries nil)
  (org-brain-file-entries-use-title nil))

(use-package org-download
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-dir "./img")
  (org-download-screenshot-method "pngpaste %s")
  (org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(use-package org-tree-slide
  :after org
  :bind
  (([(f8)] . org-tree-slide-mode)
   ([(shift f8)] . org-tree-slide-skip-done-toggle))
  :config
  (defun cdadar/org-tree-slide-setup ()
    (define-key org-tree-slide-mode-map [(f9)] 'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map [(f10)] 'org-tree-slide-move-next-tree))
  (cdadar/org-tree-slide-setup))

(use-package org-roam
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
  (org-roam-v2-ack t)
  (org-roam-database-connector 'sqlite-builtin)
  (org-id-link-to-org-use-id t)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (when (fboundp 'org-roam-db-autosync-mode)
    (org-roam-db-autosync-mode 1))
  (setq org-roam-capture-templates
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
           :unnarrowed t))))

(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode org-roam-ui-open)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package ebib
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
  :after org
  :defer t
  :custom
  (bibtex-dialect 'biblatex)
  (org-ref-show-broken-links nil)
  (org-ref-default-ref-type "eqref")
  (org-ref-default-citation-link "citet"))

(use-package org-journal
  :defer t
  :custom
  (org-journal-prefix-key "C-c j")
  (org-journal-dir "~/org-mode/roam/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-roam-bibtex
  :after org-roam)

(use-package org-zettel-ref-mode
  :defer t
  :after org-roam
  :vc (:url "https://github.com/yibie/org-zettel-ref-mode" :rev :newest)
  :custom
  (org-zettel-ref-mode-type 'org-roam)
  (org-zettel-ref-quick-markup-key "C-c m")
  (org-zettel-ref-python-environment 'venv)
  (org-zettel-ref-python-env-name "org-zettel-ref"))

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

(provide 'init-org)
;;; init-org.el ends here
