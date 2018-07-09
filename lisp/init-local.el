;; org setting
(setq org-default-notes-file "~/org-model/capture.org"
      org-default-journal-file "~/org-model/journal.org"
      org-directory "~/org-model/")

;; exec-path setting
(setq exec-path-from-shell-arguments '("-i"))

;; org mobile
;;(setq org-mobile-files (list "~/org-model/plan.org"))
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


;; python
;; (setq python-shell-interpreter "python3")

;; erc autojoin
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lisp" "#opensuse-cn" "#fedora-zh" "#emacs" "#linuxba")))

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

(provide 'init-local)
