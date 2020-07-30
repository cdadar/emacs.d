;; org setting
(setq org-directory "~/org-mode/")


;; define the refile targets
(setq org-agenda-file-note (expand-file-name "notes.org" org-directory))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
(setq org-agenda-file-work (expand-file-name "work.org" org-directory))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-directory))
(setq org-default-notes-file (expand-file-name "gtd.org" org-directory))
(setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-directory))
(setq org-agenda-files (list org-directory))

;; exec-path setting
(setq exec-path-from-shell-arguments '("-i"))

;; org mobile
;;(setq org-mobile-files (list "~/org-mode/plan.org"))
(setq org-mobile-inbox-for-pull "~/MobileOrg/index.org")
(setq org-mobile-directory "~/MobileOrg")

(setq rmh-elfeed-org-file "~/org-mode/elfeed.org")

(setq deft-directory "~/org-mode/notes")

(setq org-brain-path "~/org-mode/brain")


;; python
;; (setq python-shell-interpreter "python3")

;; erc autojoin
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lisp" "#opensuse-cn" "#fedora-zh" "#linuxba" "#emacs" "#java" "#javascript" "#python" "#python.cn" "#ruby")))

(with-eval-after-load 'company-c-headers
  (setq  company-c-headers-path-system '("."
                                         "/usr/include/"
                                         "/usr/local/include/"
                                         "/usr/include/c++/4.8/"
                                         "/usr/include/c++/4.8/x86_64-suse-linux"
                                         "/usr/include/c++/4.8/backward"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/include"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/include-fixed"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../../x86_64-suse-linux/include")))


(with-eval-after-load 'elfeed
  (setq elfeed-db-directory "~/Nutstore/.elfeed"))
(with-eval-after-load 'elfeed-org
  (setq rmh-elfeed-org-files (list rmh-elfeed-org-file)))

(custom-set-variables
 '(markdown-command "pandoc")
 '(delete-selection-mode t)
 '(scroll-bar-mode nil)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 10))))




(provide 'init-local)
