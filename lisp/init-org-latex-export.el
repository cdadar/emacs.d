;;; init-org-latex-export.el --- Org LaTeX 导出的后处理与后端配置 -*- lexical-binding: t -*-
;;; Commentary:
;; 导出 .tex 的其余后处理（重复 documentclass、quote 字体与段距、bibleref 兼容、
;; Beamer 标题转文本、multicolumn 尾随 &）以及 ox / ox-latex 本身的配置。
;; 注意 `org-export-filter-final-output-functions' 由 `add-to-list' 前插，
;; 所以下面注册的**书写顺序与实际执行顺序相反**（见该处的说明）。
;;; Code:

(defun cdadar/org-latex-drop-duplicate-documentclass (text backend info)
  "Drop the extra `\\documentclass' lines notes add on top of their class.
Notes written before `#+LATEX_CLASS_OPTIONS' habitually declare the class again
in a `#+LATEX_HEADER' line; LaTeX rejects that with \"Two \\documentclass or
\\documentstyle commands\" and then produces no PDF.  The declaration Org emits
from `org-latex-classes' is kept, and later declarations of the same class are
dropped, options included -- class options belong in `#+LATEX_CLASS_OPTIONS'.
BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point-min)
             (or (and (re-search-forward (regexp-quote "\\begin{document}") nil t)
                      (match-beginning 0))
                 (point-max)))
            (goto-char (point-min))
            ;; 选项组必须整体可选：写成 "\\[[^]]*\\]?" 时 `?' 只作用于 `]'，
            ;; `[' 仍是必需的，于是 `\documentclass{article}'(无选项，最常见的
            ;; 重复写法) 永远匹配不到，LaTeX 照样报 "Two \documentclass"。
            (let ((re (concat (regexp-quote "\\documentclass")
                              "\\(?:\\[[^]]*\\]\\)?" "{\\([^}]*\\)}")))
              (when (re-search-forward re nil t)
                (let ((class (match-string 1)))
                  (while (re-search-forward re nil t)
                    (when (string= class (match-string 1))
                      (delete-region (line-beginning-position)
                                     (min (point-max) (1+ (line-end-position)))))))))))
        (buffer-string))
    text))

(defun cdadar/org-latex-quote-cjk-font-snippet ()
  "Return LaTeX setup for the preferred CJK quote font."
  (format "\\usepackage{etoolbox}
\\newCJKfontfamily\\quotecjkfont[Scale=1.0]{%s}
\\AtBeginEnvironment{quote}{\\quotecjkfont}"
          cdadar/org-latex-cjk-quote-font))

(defun cdadar/org-latex-needs-quote-cjk-font-p (text)
  "Return non-nil when exported LaTeX TEXT should get quote CJK font setup."
  (and (string-match-p "\\\\begin{quote}" text)
       (or (string-match-p "\\\\usepackage{xeCJK}" text)
           (string-match-p "\\\\setCJKmainfont" text))
       (not (string-match-p "\\\\newCJKfontfamily\\\\quotecjkfont" text))
       (not (string-match-p "\\\\AtBeginEnvironment{quote}{\\\\quotecjkfont}" text))))

(defun cdadar/org-latex-inject-quote-cjk-font (text backend info)
  "Inject preferred CJK quote font setup into exported LaTeX TEXT.
BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (cdadar/org-latex-needs-quote-cjk-font-p text))
      (replace-regexp-in-string
       "\\\\begin{document}"
       (concat (cdadar/org-latex-quote-cjk-font-snippet) "\n\\begin{document}")
       text t t)
    text))

(defun cdadar/org-latex-fix-bibleref-compat (text backend info)
  "Normalize older bibleref snippets in exported LaTeX TEXT.
Some notes still use `\\usepackage[style=default]{bibleref}', but recent
bibleref releases reject that option.  Also rewrite the custom `\\verse{...}'
shortcut to `\\bibleref{...}' instead of redefining LaTeX's built-in verse
environment command.  BACKEND and INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "\\usepackage[style=default]{bibleref}" nil t)
          (replace-match "\\usepackage{bibleref}" t t))
        (goto-char (point-min))
        (while (search-forward "\\newcommand{\\verse}[1]{\\bibleref{#1}}" nil t)
          (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
        (goto-char (point-min))
        (while (search-forward "\\verse{" nil t)
          (replace-match "\\bibleref{" t t))
        (buffer-string))
    text))

(defun cdadar/org-latex-fix-quote-paragraph-spacing (text backend info)
  "Normalize line breaks inside LaTeX quote environments in exported TEXT.
A blank line after an Org line break becomes an explicit vertical skip, and the
line break on the last quote line is dropped: the quote environment already
ends the paragraph, so keeping it only adds an empty line (the old
negative-vspace workaround over-corrected that into overlapping text).  Notes
that still carry that workaround in their own quote environment redefinition
have it dropped from the preamble, so they stop overlapping without edits.
INFO is ignored."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point-min)
             (or (and (re-search-forward (regexp-quote "\\begin{document}") nil t)
                      (match-beginning 0))
                 (point-max)))
            (goto-char (point-min))
            (let ((re (concat "\\(" (regexp-quote "\\endlist") "\\)"
                              (regexp-quote "\\vspace")
                              "{[-][[:blank:]]*"
                              (regexp-quote "\\baselineskip")
                              "[[:blank:]]*}")))
              (while (re-search-forward re nil t)
                (replace-match "\\1" t)))))
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote "\\begin{quote}") nil t)
          (let ((quote-start (match-beginning 0)))
            (when (re-search-forward (regexp-quote "\\end{quote}") nil t)
              (let ((quote-end-marker (copy-marker (match-end 0))))
                (save-restriction
                  (narrow-to-region quote-start quote-end-marker)
                  (goto-char (point-min))
                  (while (search-forward "\\\\\n\n" nil t)
                    (replace-match "\\\\[0.6\\baselineskip]\n" t t))
                  (goto-char (point-max))
                  (when (re-search-backward
                         (concat (regexp-quote "\\\\") "[ \t]*\n[ \t]*"
                                 (regexp-quote "\\end{quote}"))
                         nil t)
                    (replace-match "\n\\end{quote}" t t))
                  (widen))
                (goto-char quote-end-marker)
                (set-marker quote-end-marker nil)))))
        (buffer-string))
    text))

(defun cdadar/org-beamer-headline-as-text (orig-fun headline contents info)
  "Beamer 导出：把 frame level 以下的标题输出为带层级的粗体文本，替代不可跨页的 block 环境。

一级小节标题（frame-level + 1，如 H:1 时的二级、H:2 时的三级）用 \\large 粗体，
更深级标题用正常字号粗体，从而保留标题层级视觉差异；同时 `allowframebreaks' 可以
在段落间正常分页，内容不会被裁剪隐藏。
以 :around advice 包裹 `org-beamer-headline'，参数遵循其 transcoder 协议。"
  (if (not (org-export-derived-backend-p (plist-get info :back-end) 'beamer))
      (funcall orig-fun headline contents info)
    (let* ((level (org-export-get-relative-level headline info))
           (frame-level (org-beamer--frame-level headline info)))
      (if (<= level frame-level)
          (funcall orig-fun headline contents info)
        (let ((title (org-export-data (org-element-property :title headline) info)))
          (if (= level (1+ frame-level))
              (format "\n\\par\\medskip\\textbf{\\large %s}\\par\n\n%s" title (or contents ""))
            (format "\n\\par\\smallskip\\textbf{%s}\\par\n\n%s" title (or contents ""))))))))

(defun cdadar/org-beamer-document-p ()
  "Return non-nil when the current buffer is a Beamer presentation.

Ask the `latex' backend for the document's class on purpose: the beamer
backend defaults `:latex-class' to the beamer class name, which would report
every plain note that declares no class at all as a presentation."
  (and (require 'ox-beamer nil t)
       (string-prefix-p "beamer"
                        (or (plist-get (org-export-get-environment 'latex)
                                       :latex-class)
                            ""))))

(defun cdadar/org-latex-export-to-pdf-route (orig-fun &rest args)
  "Export a Beamer presentation with the Beamer backend, else call ORIG-FUN.

Frames only exist in the Beamer backend.  Exporting a presentation through
the plain LaTeX backend silently turns every frame into `\\section*', which
drops the slide structure and breaks the metropolis theme."
  (if (cdadar/org-beamer-document-p)
      (apply #'org-beamer-export-to-pdf args)
    (apply orig-fun args)))

(defun cdadar/org-latex-export-to-latex-route (orig-fun &rest args)
  "Export a Beamer presentation as Beamer LaTeX, else call ORIG-FUN."
  (if (cdadar/org-beamer-document-p)
      (apply #'org-beamer-export-to-latex args)
    (apply orig-fun args)))

(defun cdadar/org-latex-export-to-pdf-async ()
  "Asynchronously export the current Org buffer to PDF.
`org-latex-export-to-pdf' is routed to the Beamer backend for presentations
by `cdadar/org-latex-export-to-pdf-route'."
  (interactive)
  (org-latex-export-to-pdf t))

(defun cdadar/org-latex-fix-multicolumn-trailing-ampersand (text backend info)
  "Remove trailing `&' after `\\multicolumn' when it spans all columns.
Org pads single-cell rows to the full column count, adding an extra
`&' after `\\multicolumn{N}{...}{...}' which creates a column-count
mismatch in LaTeX.  INFO is ignored."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\multicolumn{\\([0-9]+\\)}{\\([^}]*\\)}{\\([^}]*\\)} & \\\\\\\\"
       "\\\\multicolumn{\\1}{\\2}{\\3} \\\\\\\\"
       text)
    text))

(defconst cdadar/org-latex-final-output-filters
  '(;; 先换字体、再给缺字包 fallback、最后才在没声明字体时注入默认 CJK 字体。
    cdadar/org-latex-use-english-main-font
    cdadar/org-latex-use-installed-fonts
    cdadar/org-latex-default-cjk-font
    ;; 下面两个都要在文本里已经有 xeCJK 时才动手，所以必须排在上一行之后。
    cdadar/org-latex-inject-quote-cjk-font
    cdadar/org-latex-add-cjk-autofallback
    cdadar/org-latex-add-glyph-fallback
    ;; 与字体无关的收尾修饰，顺序不敏感。
    cdadar/org-latex-drop-duplicate-documentclass
    cdadar/org-latex-fix-bibleref-compat
    cdadar/org-latex-fix-quote-paragraph-spacing
    cdadar/org-latex-fix-multicolumn-trailing-ampersand)
  "导出 LaTeX 时依次调用的 final-output 过滤器，**按执行先后**排列。
`org-export-filter-apply-functions' 按列表从前到后调用，而 `add-to-list' 是前插，
所以注册时倒着 add（见下），列表里的顺序才是执行顺序。
之前是把十个 `add-to-list' 依次写下来，实际执行顺序与阅读顺序相反：
`inject-quote-cjk-font' 与 `add-cjk-autofallback' 都要求文本里已有 xeCJK，
却排在注入 xeCJK 的 `default-cjk-font' 之前，于是对“没自己声明 CJK 字体”的笔记
双双静默跳过。")

(use-package ox
  :ensure nil
  :after org
  :config
  (require 'ox-md nil t)
  (require 'ox-latex nil t)
  (dolist (filter (reverse cdadar/org-latex-final-output-filters))
    (add-to-list 'org-export-filter-final-output-functions filter))
  (advice-add 'org-latex-export-to-pdf :around
              #'cdadar/org-latex-export-to-pdf-route)
  (advice-add 'org-latex-export-to-latex :around
              #'cdadar/org-latex-export-to-latex-route)
  (with-eval-after-load 'ox-beamer
    (setq org-beamer-frame-default-options "allowframebreaks")
    (advice-add 'org-beamer-headline :around #'cdadar/org-beamer-headline-as-text)))

(use-package ox-latex
  :ensure nil
  :after ox
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -xelatex -interaction=nonstopmode -outdir=%o %f")
     '("xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"
       "xelatex -interaction nonstopmode -output-directory %o %f"))))

(provide 'init-org-latex-export)
;;; init-org-latex-export.el ends here
