;; org setting
(setq org-default-notes-file "~/org-mode/capture.org"
      org-default-journal-file "~/org-mode/journal.org"
      org-directory "~/org-mode/")

;; exec-path setting
(setq exec-path-from-shell-arguments '("-i"))

;; org mobile
;;(setq org-mobile-files (list "~/org-mode/plan.org"))
(setq org-mobile-inbox-for-pull "~/MobileOrg/index.org")
(setq org-mobile-directory "~/MobileOrg")

(setq rmh-elfeed-org-file "~/org-mode/elfeed.org")

(setq deft-directory "~/org-mode/notes")

;; python
;; (setq python-shell-interpreter "python3")

;; erc autojoin
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lisp" "#opensuse-cn" "#fedora-zh" "#linuxba" "#emacs" "#java" "#javascript" "#python" "#python.cn" "#ruby")))

(after-load 'company-c-headers
  (setq  company-c-headers-path-system '("."
                                         "/usr/include/"
                                         "/usr/local/include/"
                                         "/usr/include/c++/4.8/"
                                         "/usr/include/c++/4.8/x86_64-suse-linux"
                                         "/usr/include/c++/4.8/backward"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/include"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/include-fixed"
                                         "/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../../x86_64-suse-linux/include")))


(after-load 'elfeed
  (setq elfeed-db-directory "~/Nutstore/.elfeed"))
(after-load 'elfeed-org
  (setq rmh-elfeed-org-files (list rmh-elfeed-org-file)))

(provide 'init-local)
