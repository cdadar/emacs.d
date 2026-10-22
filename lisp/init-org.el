;;; Init-Org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; Org 的核心配置：键位、`org-mode' 设置、babel/crypt、全局前缀键。
;; 其余按关注点分在同目录的其他模块里：init-org-gtd.el（GTD/agenda/capture）、
;; init-org-text-tools.el、init-org-latex-*.el、init-org-roam.el、init-org-extras.el。

;;; Code:

;; 所有 Org 路径的基准目录。这是**默认值**：`lisp/init-local.el'（不入库，个人
;; 配置）会用 `setq' 覆盖它。用 `defvar' 而不是 `setq'，就是为了让先加载的
;; init-local.el 赢，同时保证仓库单独 checkout 也能起来。
(defvar org-directory (expand-file-name "~/org/")
  "Directory in which Org files live.  `lisp/init-local.el' overrides this.")

;; `prose-mode' lives at top level, not inside `use-package org :config':
;; it is a generic prose-editing mode (writeroom + mixed-pitch + bar cursor)
;; that other packages hook onto, so it must exist whether or not Org has been
;; loaded yet.
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

;; `prose-mode' has no default binding.  Wait for `org-mode-map' to exist
;; instead of relying on the deferred `:config' body having run.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c z") #'prose-mode))

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
         (org-agenda-finalize . cdadar/org-agenda-insert-efforts)
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
  ;; `C-c o' 是那些「偶尔手动跑一次」的 Org 命令的统一入口：它们除了 M-x 没有别的
  ;; 入口，不绑在这里就发现不了。分两组：clocking（上游命令）与自己写的维护命令。
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (defun cdadar/org-setup-global-prefix ()
    ;; clocking
    (define-key sanityinc/org-global-prefix-map (kbd "j") #'org-clock-goto)
    (define-key sanityinc/org-global-prefix-map (kbd "l") #'org-clock-in-last)
    (define-key sanityinc/org-global-prefix-map (kbd "i") #'org-clock-in)
    (define-key sanityinc/org-global-prefix-map (kbd "o") #'org-clock-out)
    (define-key sanityinc/org-global-prefix-map (kbd "r") #'org-clock-report)
    ;; 维护命令
    (define-key sanityinc/org-global-prefix-map (kbd "m") #'cdadar/md-clipboard-to-org)
    (define-key sanityinc/org-global-prefix-map (kbd "a") #'cdadar/org-archive-done-tasks)
    (define-key sanityinc/org-global-prefix-map (kbd "t") #'cdadar/org-clock-sum-today-by-tags)
    (define-key sanityinc/org-global-prefix-map (kbd "n") #'cdadar/org-set-monthly-nth-weekday)
    (define-key sanityinc/org-global-prefix-map (kbd "k") #'cdadar/org-kill-link-at-point)
    (define-key sanityinc/org-global-prefix-map (kbd "f") #'sanityinc/org-refile-anywhere)
    (define-key sanityinc/org-global-prefix-map (kbd "F") #'sanityinc/org-agenda-refile-anywhere)
    (define-key sanityinc/org-global-prefix-map (kbd "c") #'cdadar/org-replace-circled-numbers)
    (define-key sanityinc/org-global-prefix-map (kbd "h") #'cdadar/org-wrap-hebrew-greek)
    (define-key sanityinc/org-global-prefix-map (kbd "p") #'cdadar/org-latex-export-to-pdf-async)
    (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map))

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

(provide 'init-org)
;;; init-org.el ends here
