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

(setq bibtex-completion-bibliography "~/Nutstore Files/Nutstore/bibliography/references.bib"
      bibtex-completion-library-path "~/Nutstore Files/Nutstore/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Nutstore Files/Nutstore/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)



(setq org-roam-directory "~/org-mode/roam")
(setq elfeed-db-directory "~/Nutstore Files/Nutstore/.elfeed")
(setq rmh-elfeed-org-file "~/org-mode/elfeed.org")


(custom-set-variables
 '(markdown-command "pandoc")
 '(delete-selection-mode t)
 '(scroll-bar-mode nil)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 10)))
 '(enable-recursive-minibuffers t)
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
      (plain-tex-mode latex-mode doctex-mode context-mode texinfo-mode ams-tex-mode)
      :help "Run BibTeX")
     ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run Biber")
     ("Texindex" "texindex %s.??" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texindex")
     ("Texi2dvi" "%(PDF)texi2dvi %t" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texi2dvi or Texi2pdf")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx -o %(O?pdf) %d" TeX-run-dvipdfmx nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %(d-dir) %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeglossaries to create glossary file")
     ("Index" "makeindex %(O?idx)" TeX-run-index nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeindex to create index file")
     ("upMendex" "upmendex %(O?idx)" TeX-run-index t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("xelatex" "%`xelatex%(mode)% %t" TeX-run-TeX nil t))))


(provide 'init-local)
