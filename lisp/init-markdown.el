;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom cdadar/markdown-pdf-cjk-font "PingFang SC"
  "CJK font used when exporting markdown to PDF."
  :type 'string
  :group 'markdown)

(defcustom cdadar/markdown-pdf-chrome
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  "Chrome binary used to print the intermediate HTML to PDF."
  :type 'string
  :group 'markdown)

(defconst cdadar/markdown-pdf-css
  "@page { size: A4; margin: 16mm 14mm; }
html { -webkit-print-color-adjust: exact; }
body {
  font-family: \"__CJK_FONT__\", \"Hiragino Sans GB\", \"Microsoft YaHei\", sans-serif;
  font-size: 10.5pt;
  line-height: 1.55;
  color: #222;
  overflow-wrap: anywhere;
}
h1 { font-size: 18pt; margin: 0 0 10pt; }
h2 { font-size: 14pt; margin: 14pt 0 6pt; border-bottom: 1px solid #ddd; padding-bottom: 3pt; }
h3 { font-size: 12pt; margin: 10pt 0 4pt; }
p, li { orphans: 2; widows: 2; }
table {
  width: 100%;
  border-collapse: collapse;
  table-layout: fixed;
  margin: 6pt 0 10pt;
  font-size: 8.5pt;
}
th, td {
  border: 1px solid #bbb;
  padding: 3pt 4pt;
  vertical-align: top;
  text-align: left;
  overflow-wrap: anywhere;
  word-break: break-word;
}
th { background: #f2f2f2; }
thead { display: table-header-group; }
tr { break-inside: avoid; }
pre {
  white-space: pre-wrap;
  overflow-wrap: anywhere;
  background: #f6f6f6;
  padding: 6pt;
  border-radius: 3pt;
  font-size: 8.5pt;
}
code { font-size: 0.92em; overflow-wrap: anywhere; }
img { max-width: 100%; height: auto; }
blockquote { margin: 6pt 0; padding-left: 10pt; border-left: 3px solid #ccc; color: #555; }"
  "CSS for the intermediate HTML before printing to PDF.")

(defun cdadar/markdown-export-pdf ()
  "Export the current markdown buffer to a PDF next to the file, then open it.

Uses pandoc to render standalone HTML and Chrome's headless printer for PDF.
LaTeX is avoided because its longtable output overlaps on wide tables with
long unbreakable tokens (paths, version strings, CJK text)."
  (interactive)
  (let* ((in (buffer-file-name))
         (out (and in (concat (file-name-sans-extension in) ".pdf")))
         (title (and in (file-name-base in)))
         (html (make-temp-file "markdown-pdf-" nil ".html"))
         (css (make-temp-file "markdown-pdf-" nil ".css")))
    (unless in
      (user-error "Buffer is not visiting a file"))
    (unless (file-exists-p cdadar/markdown-pdf-chrome)
      (user-error "Chrome not found: %s" cdadar/markdown-pdf-chrome))
    (unwind-protect
        (progn
          (with-temp-file css
            (insert (replace-regexp-in-string
                     "__CJK_FONT__" cdadar/markdown-pdf-cjk-font
                     cdadar/markdown-pdf-css)))
          (with-temp-buffer
            (unless (zerop (call-process "pandoc" nil t nil
                                         in "-o" html "--standalone"
                                         "--embed-resources"
                                         "--css" css
                                         "--metadata" (concat "title=" title)))
              (user-error "pandoc failed:\n%s" (buffer-string))))
          (with-temp-buffer
            (unless (zerop (call-process cdadar/markdown-pdf-chrome nil t nil
                                         "--headless=new" "--disable-gpu"
                                         "--no-pdf-header-footer"
                                         (concat "--print-to-pdf=" out)
                                         (concat "file://" html)))
              (user-error "Chrome failed:\n%s" (buffer-string))))
          (unless (file-exists-p out)
            (user-error "PDF export failed, no output at %s" out))
          (message "Exported %s" out)
          (browse-url-of-file out))
      (delete-file html)
      (delete-file css))))

(use-package markdown-mode
  :bind (:map markdown-mode-command-map
         ("P" . cdadar/markdown-export-pdf))
  :config
  (setq markdown-command "pandoc")
  (add-auto-mode 'markdown-mode "\\.md\\..markdown\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
