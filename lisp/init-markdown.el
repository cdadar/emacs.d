;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom cdadar/markdown-pdf-cjk-font "PingFang SC"
  "CJK font used when exporting markdown to PDF (pandoc xelatex engine)."
  :type 'string
  :group 'markdown)

(defun cdadar/markdown-export-pdf ()
  "Export the current markdown buffer to a PDF next to the file, then open it."
  (interactive)
  (let* ((in (buffer-file-name))
         (out (and in (concat (file-name-sans-extension in) ".pdf")))
         (cmd (format "pandoc %s -o %s --pdf-engine=xelatex -V CJKmainfont=%s"
                      (shell-quote-argument in)
                      (shell-quote-argument out)
                      (shell-quote-argument cdadar/markdown-pdf-cjk-font))))
    (unless in (user-error "Buffer is not visiting a file"))
    (shell-command cmd)
    (unless (file-exists-p out)
      (user-error "PDF export failed, no output at %s" out))
    (browse-url-of-file out)))

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
