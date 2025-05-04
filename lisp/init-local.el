;; org setting
(setq org-directory "~/org-mode/")

(setq system-time-locale "en_US")

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
;; (setq org-agenda-files (list org-directory))

;; exec-path setting
(setq exec-path-from-shell-arguments '("-i"))

;; org mobile
;;(setq org-mobile-files (list "~/org-mode/plan.org"))
(setq org-mobile-inbox-for-pull "~/MobileOrg/index.org")
(setq org-mobile-directory "~/MobileOrg")




(setq org-roam-directory "~/org-mode/roam")

(setq deft-directory "~/org-mode/roam")

(setq org-brain-path "~/org-mode/brain")



;; python
;; (setq python-shell-interpreter "python3")

(setq source-directory
      (file-name-directory
       (shell-command-to-string
        (concat "locate --limit 1 emacs-" emacs-version "/src"))))

(setq find-function-C-source-directory
      (concat source-directory "src/"))

;; erc autojoin
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lisp" "#opensuse-cn" "#fedora-zh" "#linuxba" "#emacs" "#java" "#javascript" "#python" "#python.cn" "#ruby")))

(setq reftex-default-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Nutstore Files/Nutstore/bibliography/notes.org"
      org-ref-default-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib")
      org-ref-pdf-directory "~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs/")


(setq
 ebib-preload-bib-files '("~/Nutstore Files/Nutstore/bibliography/references.bib")
 ebib-notes-use-single-file "~/Nutstore Files/Nutstore/bibliography/Notes.org"
 ebib-file-search-dirs '("~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs/")
 ebib-reading-list-file "~/Nutstore Files/Nutstore/bibliography/ReadingList.org"
 ebib-keywords-file "~/Nutstore Files/Nutstore/bibliography/ebib-keywords.txt")

(setq bibtex-completion-bibliography "~/Nutstore Files/Nutstore/bibliography/references.bib"
      bibtex-completion-library-path "~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Nutstore Files/Nutstore/bibliography/helm-bibtex-notes")


(setq citar-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib"))
(setq org-cite-global-bibliography '("~/Nutstore Files/Nutstore/bibliography/references.bib"))

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)


(setq org-roam-directory "~/org-mode/roam")
(setq elfeed-db-directory "~/Nutstore Files/Nutstore/.elfeed")
(setq rmh-elfeed-org-file "~/org-mode/elfeed.org")
(setq bibtex-capf-bibliography "~/Nutstore Files/Nutstore/bibliography/references.bib")
(setq org-zettel-ref-overview-directory "~/Nutstore Files/Nutstore/org-zettel/overviews/")
(setq org-zettel-ref-temp-folder "~/Nutstore Files/Nutstore/org-zettel/temp_convert/") ; 该文件夹用于存放等待转换的文档
(setq org-zettel-ref-reference-folder "~/Nutstore Files/Nutstore/org-zettel/ref/") ; 该文件夹用于存放转换后的参考资料
(setq org-zettel-ref-archive-folder "~/Nutstore Files/Nutstore/org-zettel/archives/") ; 该文件夹用于存放转换后的归档文件

(setq elpamr-default-output-directory "~/Nutstore Files/Nutstore/emacs/epla-mirror")
(add-to-list 'package-archives '("epla-mirror" . "~/Nutstore Files/Nutstore/emacs/epla-mirror"))

(custom-set-variables
 '(markdown-command "pandoc")
 '(delete-selection-mode t)
 '(scroll-bar-mode nil)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 10)))
 '(enable-recursive-minibuffers t)
 '(text-mode-ispell-word-completion nil))

(provide 'init-local)
