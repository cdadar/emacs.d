;; org setting
(setq org-directory "~/org-mode/")


;; define the refile targets
(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-directory))
(setq org-agenda-file-note (expand-file-name "notes.org" org-directory))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
(setq org-agenda-file-work (expand-file-name "work.org" org-directory))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
(setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-directory))
(setq org-capture-web-bookmarks (expand-file-name "web.org" org-directory))
(setq org-capture-anki (expand-file-name "anki.org" org-directory))
(setq org-agenda-files (list org-directory))

;; exec-path setting
(setq exec-path-from-shell-arguments '("-i"))

;; org mobile
;;(setq org-mobile-files (list "~/org-mode/plan.org"))
(setq org-mobile-inbox-for-pull "~/MobileOrg/index.org")
(setq org-mobile-directory "~/MobileOrg")

(setq rmh-elfeed-org-file "~/org-mode/elfeed.org")

(setq deft-directory "~/org-mode/roam")

(setq org-brain-path "~/org-mode/brain")


;; python
;; (setq python-shell-interpreter "python3")

;; erc autojoin
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lisp" "#opensuse-cn" "#fedora-zh" "#linuxba" "#emacs" "#java" "#javascript" "#python" "#python.cn" "#ruby")))

(with-eval-after-load 'company-c-headers
  (setq company-c-headers-path-system
        (append company-c-headers-path-system '("/usr/include/c++/7/"
                                                "/usr/include/c++/7/x86_64-suse-linux"
                                                "/usr/include/c++/7/backward"
                                                "/usr/lib64/gcc/x86_64-suse-linux/7/include"
                                                "/usr/lib64/gcc/x86_64-suse-linux/7/include-fixed"
                                                "/usr/lib64/gcc/x86_64-suse-linux/7/../../../../x86_64-suse-linux/include"))))

(with-eval-after-load 'org-ref
  (setq reftex-default-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Nutstore Files/Nutstore/bibliography/notes.org"
        org-ref-default-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib")
        org-ref-pdf-directory "~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs/")

  (setq bibtex-completion-bibliography "~/Nutstore Files/Nutstore/bibliography/references.bib"
        bibtex-completion-library-path "~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs"
        bibtex-completion-notes-path "~/Nutstore Files/Nutstore/bibliography/helm-bibtex-notes")

  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))

  ;; alternative
  ;; (setq bibtex-completion-pdf-open-function 'org-open-file)

  )

(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/org-mode/roam"))

(with-eval-after-load 'elfeed
  (setq elfeed-db-directory "~/Nutstore Files/Nutstore/.elfeed"))
(with-eval-after-load 'elfeed-org
  (setq rmh-elfeed-org-files (list rmh-elfeed-org-file)))

(custom-set-variables
 '(markdown-command "pandoc")
 '(delete-selection-mode t)
 '(scroll-bar-mode nil)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 10)))
 '(enable-recursive-minibuffers t))




(provide 'init-local)
