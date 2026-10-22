;;; init-org-roam.el --- 笔记库、文献与期刊 -*- lexical-binding: t -*-
;;; Commentary:
;; org-roam 及其 capture 模板、org-roam-ui、org-brain/org-id 之外的
;; ebib / org-ref / bibtex / org-journal / org-roam-bibtex / zettel，
;; 以及 review、fragtog、pandoc-import。
;;; Code:

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

(provide 'init-org-roam)
;;; init-org-roam.el ends here
