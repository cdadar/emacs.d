;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(use-package org
  :bind
  ((("C-c c" . org-capture)))
  :mode ("\\.\\(org\\|org_archive\\)\\'" . org-mode)
  :hook (org-mode . (lambda () (setq toggle-truncate-lines t)))
  :config
  (progn
    (when *is-a-mac*
      (use-package grab-mac-link))



    (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
      "A keymap for handy global access to org helpers, particularly clocking.")

    (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
    (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
    (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
    (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
    (define-key sanityinc/org-global-prefix-map (kbd "r") 'org-clock-report)
    (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


    ;; Various preferences
    (setq org-log-done t
          org-edit-timestamp-down-means-later t
          org-hide-emphasis-markers t
          org-catch-invisible-edits 'show
          org-export-coding-system 'utf-8
          org-fast-tag-selection-single-key 'expert
          org-html-validation-link nil
          org-export-kill-product-buffer-when-displayed t
          org-export-allow-bind-keywords t
          org-enforce-todo-dependencies nil
          org-enforce-todo-checkbox-dependencies nil
          org-image-actual-width '(400)
          org-tags-column 80
          org-deadline-warning-days 30
          org-lowest-priority 68)


    ;; Lots of stuff from http://doc.norang.ca/org-mode.html

    ;; TODO: fail gracefully
    (defun sanityinc/grab-ditaa (url jar-name)
      "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
      ;; TODO: handle errors
      (message "Grabbing %s for org." jar-name)
      (let ((zip-temp (make-temp-name "emacs-ditaa")))
        (unwind-protect
            (progn
              (when (executable-find "unzip")
                (url-copy-file url zip-temp)
                (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                       " " (shell-quote-argument jar-name) " > "
                                       (shell-quote-argument org-ditaa-jar-path)))))
          (when (file-exists-p zip-temp)
            (delete-file zip-temp)))))

    (with-eval-after-load 'ob-ditaa
      (unless (and (boundp 'org-ditaa-jar-path)
                   (file-exists-p org-ditaa-jar-path))
        (let ((jar-name "ditaa0_9.jar")
              (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
          (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
          (unless (file-exists-p org-ditaa-jar-path)
            (sanityinc/grab-ditaa url jar-name)))))

    (with-eval-after-load 'ob-plantuml
      (let ((jar-name "plantuml.jar")
            (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
        (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
        (unless (file-exists-p org-plantuml-jar-path)
          (url-copy-file url org-plantuml-jar-path))))

    ;; Re-align tags when window shape changes
    (with-eval-after-load 'org-agenda
      (add-hook 'org-agenda-mode-hook
                (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


    (defvar dynamic-agenda-files nil
      "dynamic generate agenda files list when changing org state")

    (defun update-dynamic-agenda-hook ()
      (let ((done (or (not org-state) ;; nil when no TODO list
                      (member org-state org-done-keywords)))
            (file (buffer-file-name))
            (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
        (unless (member file agenda)
          (if done
              (save-excursion
                (goto-char (point-min))
                ;; Delete file from dynamic files when all TODO entry changed to DONE
                (unless (search-forward-regexp org-not-done-heading-regexp nil t)
                  (customize-save-variable
                   'dynamic-agenda-files
                   (cl-delete-if (lambda (k) (string= k file))
                                 dynamic-agenda-files))))
            ;; Add this file to dynamic agenda files
            (unless (member file dynamic-agenda-files)
              (customize-save-variable 'dynamic-agenda-files
                                       (add-to-list 'dynamic-agenda-files file)))))))

    (defun dynamic-agenda-files-advice (orig-val)
      (union orig-val dynamic-agenda-files :test #'equal))

    ;; (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
    ;; (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t)

    

    (use-package writeroom-mode)

    (define-minor-mode prose-mode
      "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
      :init-value nil :lighter " Prose" :keymap nil
      (if prose-mode
          (progn
            (when (fboundp 'writeroom-mode)
              (writeroom-mode 1))
            (setq truncate-lines nil)
            (setq word-wrap t)
            (setq cursor-type 'bar)
            (when (eq major-mode 'org)
              (kill-local-variable 'buffer-face-mode-face))
            (buffer-face-mode 1)
            ;;(delete-selection-mode 1)
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
        ;; (delete-selection-mode -1)
        (flyspell-mode -1)
        (visual-line-mode -1)
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 0))))

    ;;(add-hook 'org-mode-hook 'buffer-face-mode)


    (setq org-support-shift-select t)
    
;;; Capturing
    (defun get-year-and-month ()
      (list (format-time-string "%Y 年") (format-time-string "%m 月")))


    (defun find-month-tree ()
      (let* ((path (get-year-and-month))
             (level 1)
             end)
        (unless (derived-mode-p 'org-mode)
          (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
        (goto-char (point-min))          ;移动到 buffer 的开始位置
        ;; 先定位表示年份的 headline，再定位表示月份的 headline
        (dolist (heading path)
          (let ((re (format org-complex-heading-regexp-format
                            (regexp-quote heading)))
                (cnt 0))
            (if (re-search-forward re end t)
                (goto-char (point-at-bol)) ;如果找到了 headline 就移动到对应的位置
              (progn                       ;否则就新建一个 headline
                (or (bolp) (insert "\n"))
                (if (/= (point) (point-min)) (org-end-of-subtree t t))
                (insert (make-string level ?*) " " heading "\n"))))
          (setq level (1+ level))
          (setq end (save-excursion (org-end-of-subtree t t))))
        (org-end-of-subtree)))

    (defun org-capture-template-goto-link ()
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

    (defun generate-anki-note-body ()
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

    (require 'org-protocol)



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
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1)
            ("v" "Vocabulary" entry
             (file+headline org-capture-anki "Vocabulary")
             ,(concat "* %^{heading} :note:\n"
                      "%(generate-anki-note-body)\n"))
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
             (file+function org-capture-web-bookmarks org-capture-template-goto-link)
             "  %U - %?\n\n  %:initial" :empty-lines 1)))



    (defun cdadar/org-kill-link-at-point ()
      (interactive)
      (when (eq major-mode 'org-mode)
        (let* ((context (org-element-context))
               (type (org-element-type context))
               (beg (org-element-property :begin context))
               (end (org-element-property :end context)))
          (when (eq type 'link)
            (kill-region beg end)))))

    
;;; Refiling

    (setq org-refile-use-cache nil)

    ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
    (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

    (with-eval-after-load 'org-agenda
      (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

    (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

    ;; Exclude DONE state tasks from refile targets
    (defun sanityinc/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets."
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

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

    ;; Targets start with the file name - allows creating level 1 tasks
    ;;(setq org-refile-use-outline-path (quote file))
    (setq org-refile-use-outline-path t)
    (setq org-outline-path-complete-in-steps nil)

    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    
;;; To-do settings

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                  (sequence "WAITING(w@/!)" "TESTING(T!)" "PUBLISH(P!)"  "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
          org-todo-repeat-to-state "NEXT")

    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

    (setq org-todo-keyword-faces
          (quote (("NEXT" :inherit warning)
                  ("PROJECT" :inherit font-lock-string-face))))


    
;;; Agenda views

    (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


    (let ((active-project-match "-INBOX/PROJECT"))

      (setq org-stuck-projects
            `(,active-project-match ("NEXT")))

      (setq org-tag-alist
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

      (setq org-agenda-compact-blocks t
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
            `(("N" "Notes" tags "NOTE"
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
                             '(lambda ()
                                (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                    (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down priority-down effort-up category-keep))))
                (tags-todo ,active-project-match
                           ((org-agenda-overriding-header "Projects")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-INBOX/-NEXT"
                           ((org-agenda-overriding-header "Orphaned Tasks")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-skip-function
                             '(lambda ()
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
                             '(lambda ()
                                (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                    (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                ;; (tags-todo "-NEXT"
                ;;            ((org-agenda-overriding-header "All other TODOs")
                ;;             (org-match-list-sublevels t)))
                ))

              ;;An entry without a cookie is treated just like priority ' B '.
              ;;So when create new task, they are default 重要且紧急
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
               ((stuck "")           ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))
              ("c" . "特定标签")
              ("co" "At the office" tags-todo "@office")
              ("ch" "At the home" tags-todo "@home")
              ("ct" "At the travelling" tags-todo "@travelling")
              ("cw" "At the way" tags-todo "@way")
              ("cc" "At the computer" tags-todo "@computer")
              ("cp" "At the phone" tags-todo "@phone")
              ("ck" "At the kindle" tags-todo "@kindle")
              ("cb" "bug" tags-todo "bug" )
              ("cd" "demand" tags-todo "demand")
              ("cB" "book" tags-todo "book")
              ("cv" "video" tags-todo "video"))))


    (add-hook 'org-agenda-mode-hook 'hl-line-mode)


    (defun my/org-agenda-calculate-efforts (limit)
      "Sum the efforts of scheduled entries up to LIMIT in the
        agenda buffer."
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
                                        (my/org-agenda-calculate-efforts
                                         (next-single-property-change (point) 'day))
                                        ")"))
            (forward-line)))))

    (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)

    
;;; Org clock

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (org-clock-persistence-insinuate)
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)

    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Save state changes in the LOGBOOK drawer
    (setq org-log-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    (setq org-clock-clocked-in-display 'mode-line)
    (setq org-clock-mode-line-total 'today)
    ;; Show clock sums as hours and minutes, not "n days" etc.
    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


    
;;; Show the clocked-in task - if any - in the header line
    (defun sanityinc/show-org-clock-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun sanityinc/hide-org-clock-from-header-line ()
      (setq-default header-line-format nil))

    ;; (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
    ;; (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
    ;; (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

    (with-eval-after-load 'org-clock
      (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
      (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


    
    (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
      (add-hook 'org-clock-in-hook
                (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                         (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
      (add-hook 'org-clock-out-hook
                (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                         "tell application \"org-clock-statusbar\" to clock out"))))


    
    ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
    ;; TODO: nested projects!


    
;;; Archiving

    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::datetree/")

    (defun org-archive-done-tasks ()
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

    

    ;; ;; Show iCal calendars in the org agenda
    ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
    ;;   (setq org-agenda-include-diary t
    ;;         org-agenda-custom-commands
    ;;         '(("I" "Import diary from iCal" agenda ""
    ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

    ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
    ;;             (lambda ()
    ;;               (goto-char (point-min))
    ;;               (save-excursion
    ;;                 (while (re-search-forward "^[a-z]" nil t)
    ;;                   (goto-char (match-beginning 0))
    ;;                   (insert "0:00-24:00 ")))
    ;;               (while (re-search-forward "^ [a-z]" nil t)
    ;;                 (goto-char (match-beginning 0))
    ;;                 (save-excursion
    ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
    ;;                 (insert (match-string 0))))))




    (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
    (when *is-a-mac*
      (define-key org-mode-map (kbd "M-h") nil)
      (define-key org-mode-map (kbd "C-c g") 'grab-mac-link))

    ;; org-latex
    (require 'ox-md)
    (require 'ox-latex)

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f")) ;; org v8
    ;; }}

    (eval-after-load "ox-latex"

      ;; update the list of LaTeX classes and associated header (encoding, etc.)
      ;; and structure
      '(add-to-list 'org-latex-classes
                    `("beamer"
                      ,(concat "\\documentclass[presentation]{beamer}\n"
                               "[DEFAULT-PACKAGES]"
                               "[PACKAGES]"
                               "[EXTRA]\n")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))




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
        (sh . t) ;; obsolete
        (shell . t)
        (sql . t)
        (sqlite . t))))

    (require 'org-crypt)
    ;; org-mode 設定
    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)
    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")
    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key "6DF1ABB0")


    (defun +org-screenshot ()
      "Take a screenshot into a unique-named file in the current buffer file
 directory and insert a link to this file."
      (interactive)
      (org-display-inline-images)

      (setq filename
            (concat
             (make-temp-name
              (concat (file-name-nondirectory (buffer-file-name))
                      "_imgs/"
                      (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename)))
                                        ; take screenshot
      (make-frame-invisible nil t)
      (when *is-a-mac*
        (progn
          (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
                                                                              "\"" filename "\"" ))
          (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
          ))
      (when *linux*
        (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
      (make-frame-visible)
      (if (file-exists-p filename)
          (insert (concat "[[file:" filename "]]")))
      (org-display-inline-images))

    (define-key org-mode-map (kbd "C-c s c") '+org-screenshot)




    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags (current-tag)
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             ;;(include-tags '("PROG" "READING" "NOTE" "OTHER" "@Work" "@Self" "MEETING" "LEARN"))
             ;;                         "LEARNING" "OUTPUT" "OTHER"))
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
              (org-clock-sum tstart tend #'(lambda() (filter-by-tags current-tag)))
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
        output-string))))

(use-package org-pomodoro
  :after org
  :bind
  (:map org-agenda-mode-map
        (("M-p" . org-pomodoro))
        :map org-mode-map
        (("M-p" . org-pomodoro)))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (defun notify-send (title message)
    (call-process "notify-send"
                  nil 0 nil
                  title
                  message))
  :hook
  ((org-pomodoro-finished . (lambda ()
                              (notify-send "Pomodoro completed!" "Time for a break.")))
   (org-pomodoro-break-finished . (lambda ()
                                    (notify-send "Pomodoro Short Break Finished" "Ready for Another?")))
   (org-pomodoro-long-break-finished . (lambda ()
                                         (notify-send "Pomodoro Long Break Finished" "Ready for Another?")))
   (org-pomodoro-killed . (lambda ()
                            (notify-send "Pomodoro Killed" "One does not simply kill a pomodoro!")))))

(use-package org-cliplink
  :after org
  :bind
  (("C-c l" . org-store-link) ("C-c a" . org-agenda)))

;; create ppt
(use-package ox-ioslide
  :after org
  :config)

;; org-appear
(use-package org-appear
  :after org
  :hook
  (org-mode . org-appear-mode))

;; generate mind map
(use-package org-mind-map
  :after org
  :config
  (require 'ox-org)
  (setq org-mind-map-engine "dot")   ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )


(use-package org-brain
  :after org
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

(use-package org-download
  :after org
  :demand t
  :hook
  (dired-mode-hook . org-download-enable)
  (org-mode-hook . org-download-enable)
  :config
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "./img"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(use-package org-tree-slide
  :after org
  :bind
  (:map
   org-tree-slide-mode-map
   ("<f9>" . org-tree-slide-move-previous-tree)
   ("<f10>" . org-tree-slide-move-next-tree)
   :map org-mode-map
   ("<f8>" . org-tree-slide-mode)
   ("S-<f8>" . org-tree-slide-skip-done-toggle)))

(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n s" . org-roam-db-sync)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n T" . org-roam-tag-remove)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today)
   )
  :hook
  (after-init . org-roam-db-autosync-mode)
  :config
  (progn
    (setq org-roam-v2-ack t)
    (setq org-roam-database-connector 'sqlite-builtin)
    (setq org-id-link-to-org-use-id t)
    (setq org-roam-completion-everywhere t)
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
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
            ("co" "听道" plain "%?"
             :target (file+head "christian/other/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+tags: 听道")
             :unnarrowed t)
            ("cy" "以斯拉学习" plain "%?"
             :target (file+head "christian/yisila/%<%Y%m%d%H%M%S>-${slug}.org"
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
            ("cs" "查经" plain "%?"
             :target (file+head "christian/study/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+tags: 查经")
             :unnarrowed t)
            ("cS" "学道" plain "%?"
             :target (file+head "christian/word/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+tags: 学道")
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

    (use-package org-roam-ui
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))))

(use-package org-ql)

;; (use-package org-super-links
;;   :bind (("C-c s s" . org-super-links-link)
;;         ("C-c s l" . org-super-links-store-link)
;;         ("C-c s C-l" . org-super-links-insert-link)))


(use-package ox-epub
  :after org
  :config
  (require 'ox-epub))

(use-package ox-pandoc
  :after org
  :config
  (require 'ox-pandoc))

(use-package ebib
  :custom
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (ebib-uniquify-keys t)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-index-window-size 10)
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-file-associations '(("pdf")) "using Emacs to open pdf")
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-index-columns '(("Entry Key" 20 t)
                        ("Author/Editor" 40 nil)
                        ("Year" 6 t)
                        ("Title" 50 t)))
  (ebib-index-default-sort '("timestamp" . descend)))

(use-package org-ref
  :custom
  (bibtex-dialect 'biblatex)
  (org-ref-show-broken-links nil)
  (org-ref-default-ref-type "eqref")
  (org-ref-default-citation-link "citet"))

(use-package org-journal
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org-mode/roam/journal/"
        org-journal-date-format "%A, %d %B %Y"))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


(use-package org-zettel-ref-mode
  :vc(:url "https://github.com/yibie/org-zettel-ref-mode" :rev :newest)
  :config
  (setq org-zettel-ref-mode-type 'org-roam)
  (setq org-zettel-ref-quick-markup-key "C-c m")
  (setq org-zettel-ref-python-environment 'venv)  ; 或 'system, 'venv
  (setq org-zettel-ref-python-env-name "org-zettel-ref")  ; 如果使用 Conda 或 venv
  (setq org-zettel-ref-python-file (expand-file-name "org-zettel-ref-mode/convert-to-org.py" package-user-dir)))

(use-package org-review
  :bind (:map org-agenda-mode-map
              ("C-c C-r" . org-review-insert-last-review))
  :config
  (setq org-agenda-custom-commands
        '(("R" "Review projects" tags-todo "-CANCELLED/"
           ((org-agenda-overriding-header "Reviews Scheduled")
            (org-agenda-skip-function 'org-review-agenda-skip)
            (org-agenda-cmp-user-defined 'org-review-compare)
            (org-agenda-sorting-strategy '(user-defined-down)))))))


(use-package org-fragtog
  :hook ((org-mode . org-fragtog-mode)))

(provide 'init-org)
;;; init-org.el ends here
