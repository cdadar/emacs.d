;;; init-nxml.el --- Support for editing XML with NXML -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nxml-mode
  :ensure nil
  :mode (("\\.\\(xml\\|xsd\\|sch\\|rng\\|xslt\\|svg\\|rss\\|gpx\\|tcx\\|plist\\)\\'" . nxml-mode))
  :magic ("<\\?xml " . nxml-mode)
  :init
  (setq nxml-slash-auto-complete-flag t)
  (fset 'xml-mode 'nxml-mode)
  ;; Keep plain XML files in `nxml-mode' even after `init-web-mode' registers
  ;; web-mode's broader "\\.xml?\\'" pattern later during startup.
  (with-eval-after-load 'init-web-mode
    (add-auto-mode 'nxml-mode "\\.xml\\'")))


;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (beg end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) beg)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
      (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region beg end)))


;; Integration with tidy for html + xml

(defun sanityinc/tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))


(provide 'init-nxml)
;;; init-nxml.el ends here
