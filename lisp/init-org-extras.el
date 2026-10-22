;;; init-org-extras.el --- Org 周边小包 -*- lexical-binding: t -*-
;;; Commentary:
;; 与 org 核心、GTD 流程、导出后端都无关的那批小包：抓链接与剪贴板、
;; 图片下载、幻灯片、思维导图、番茄钟、词数统计、org-gfm 导出与目录/混排。
;;; Code:

;;; --- Related packages ---

(use-package writeroom-mode
  :if (locate-library "writeroom-mode"))


(use-package grab-mac-link
  :if (and (bound-and-true-p *is-a-mac*)
           (locate-library "grab-mac-link"))
  :commands (grab-mac-link grab-mac-link-dwim)
  :bind (:map org-mode-map
              ("C-c g" . grab-mac-link)))

(use-package org-pomodoro
  :if (locate-library "org-pomodoro")
  :after org
  :bind
  ([(meta p)] . org-pomodoro)
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  :hook
  ((org-pomodoro-finished . cdadar/org-pomodoro-finished-notify)
   (org-pomodoro-break-finished . cdadar/org-pomodoro-break-finished-notify)
   (org-pomodoro-long-break-finished . cdadar/org-pomodoro-long-break-finished-notify)
   (org-pomodoro-killed . cdadar/org-pomodoro-killed-notify))
  :config
  (defun cdadar/org-pomodoro-notify (title message)
    (call-process "notify-send" nil 0 nil title message))
  (defun cdadar/org-pomodoro-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro completed!" "Time for a break."))
  (defun cdadar/org-pomodoro-break-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?"))
  (defun cdadar/org-pomodoro-long-break-finished-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?"))
  (defun cdadar/org-pomodoro-killed-notify ()
    (cdadar/org-pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))

;; Editing and UI extensions
(use-package org-cliplink
  :if (locate-library "org-cliplink")
  :after org
  :commands (org-cliplink org-cliplink-dwim)
  :bind (:map org-mode-map
              ("C-x p i" . org-cliplink)))

(use-package org-appear
  :if (locate-library "org-appear")
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-count-words
  :vc (:url "https://github.com/Elilif/org-count-words" :rev :newest)
  :after org
  :commands (org-count-words-buffer org-count-words-region org-count-words-subtree org-count-words-mode)
  :config
  ;; 统计口径改为「非空格字符数」（同 advance-words-count 的 Ns），
  ;; 这样 1:1 算 3 个字符而非 1 个词。
  (defun cdadar/org-count-words-chars (start end)
    "Count non-space characters between START and END."
    (let ((count 0))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (and (< (point) end)
                      (re-search-forward "[^[:space:]]" end t))
            (setq count (1+ count)))))
      count))
  (setq org-count-words-function #'cdadar/org-count-words-chars)
  ;; latex-fragment：只数宏参数内的字符（\gr{...} 算 … 内的内容），
  ;; 宏名与括号不计。段落已统计整段，这里用负偏移抵消宏名/括号部分。
  (defun cdadar/org-count-words-latex-fragment (element)
    "Count non-space chars in the first {...} argument of a LaTeX fragment,
offsetting the macro-name/brace chars already counted by the paragraph."
    (let* ((beg (org-element-property :begin element))
           (end (org-element-property :end element))
           (text (buffer-substring-no-properties beg end))
           (total (cdadar/org-count-words-chars beg end)))
      (if (string-match "{\\([^}]*\\)}" text)
          (- (cdadar/org-count-words-chars (+ beg (match-beginning 1))
                                           (+ beg (match-end 1)))
             total)
        (- total))))
  (setf (alist-get 'latex-fragment org-count-words-element-functions)
        #'cdadar/org-count-words-latex-fragment)
  ;; heading 是否计入可切换：有时要标题文字，有时不要。
  (defvar cdadar/org-count-words-include-headings t
    "Non-nil counts headline text in org-count-words.")
  (defun cdadar/org-count-words-set-headings (include)
    "Add or remove `headline' from `org-count-words-elemnts' for INCLUDE."
    (if include
        (add-to-list 'org-count-words-elemnts 'headline)
      (setq org-count-words-elemnts (delq 'headline org-count-words-elemnts))))
  (defun cdadar/org-count-words-toggle-headings ()
    "Toggle whether org-count-words counts headline text."
    (interactive)
    (setq cdadar/org-count-words-include-headings
          (not cdadar/org-count-words-include-headings))
    (cdadar/org-count-words-set-headings cdadar/org-count-words-include-headings)
    (message "org-count-words: %s headings"
             (if cdadar/org-count-words-include-headings "counting" "ignoring")))
  ;; 标题统计从星号之后开始，结构性的 `*` 不计入。
  (defun cdadar/org-count-words-headline (element)
    "Count chars in headline ELEMENT text, excluding leading stars/spaces."
    (let* ((beg (org-element-property :begin element))
           (end (or (org-element-property :contents-begin element)
                    (org-element-property :end element))))
      (save-excursion
        (goto-char beg)
        (skip-chars-forward "* \t")
        (when (< (point) end)
          (cdadar/org-count-words-chars (point) end)))))
  (setf (alist-get 'headline org-count-words-element-functions)
        #'cdadar/org-count-words-headline))

;; Export extensions
;; create ppt
(use-package ox-ioslide
  :if (locate-library "ox-ioslide")
  :after ox
  :commands (org-ioslide-export-as-html org-ioslide-export-to-html))

(use-package org-mind-map
  :if (locate-library "org-mind-map")
  :after ox
  :commands (org-mind-map-write
             org-mind-map-write-with-prompt
             org-mind-map-write-current-branch
             org-mind-map-write-current-tree)
  :custom
  (org-mind-map-engine "dot")
  :init
  (require 'ox-org nil t))

;; Knowledge and media extensions
(use-package org-brain
  :if (locate-library "org-brain")
  :after org
  :custom
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 12)
  (org-brain-include-file-entries nil)
  (org-brain-file-entries-use-title nil))

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-track-globally t)
  (org-id-locations-file (locate-user-emacs-file ".org-id-locations"))
  (org-id-link-to-org-use-id t))

(use-package org-download
  :if (locate-library "org-download")
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-dir "./img")
  (org-download-screenshot-method (if *is-a-mac* "pngpaste %s" "import %s"))
  (org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(use-package org-tree-slide
  :if (locate-library "org-tree-slide")
  :after org
  :bind
  (([(f8)] . org-tree-slide-mode)
   ([(shift f8)] . org-tree-slide-skip-done-toggle)
   :map org-tree-slide-mode-map
   ([(f9)] . org-tree-slide-move-previous-tree)
   ([(f10)] . org-tree-slide-move-next-tree)))

;; GitHub-flavored Markdown export (`C-c C-e g g').
;; The dispatch menu is built from `org-export-registered-backends', so
;; loading the backend is all that is needed: putting `gfm' into
;; `org-export-backends' would run its Customize `:set', which drops every
;; registered backend that is not named in the list (org-ref's, for one).
(use-package ox-gfm
  :demand t
  :after org)

;; Keep the table of contents under a `:TOC:'-tagged heading up to date on save
(use-package toc-org
  :hook org-mode)

;; Variable pitch for prose, fixed pitch for code and tables in one buffer.
;; Complements `prose-mode', which already centers the text through
;; `writeroom-mode' (so `olivetti' is intentionally not used here).
(use-package mixed-pitch
  :hook (prose-mode . mixed-pitch-mode))

(provide 'init-org-extras)
;;; init-org-extras.el ends here
