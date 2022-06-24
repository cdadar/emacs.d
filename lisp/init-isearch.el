;;; init-isearch.el --- isearch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Show number of matches while searching
(use-package anzu
  :hook
  (after-init . global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  :bind
  (([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace)))


(use-package isearch
  :ensure nil
  :commands (isearch-occur)
  :config
  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (if sym
          (progn
            (setq isearch-regexp t
                  isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
        (ding)))
    (isearch-search-and-update))
  (defun sanityinc/isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  :bind
  (:map isearch-mode-map
        (([remap isearch-delete-char] . isearch-del-char)
         ("C-c C-o" . isearch-occur)
         ("C-M-w" . isearch-yank-symbol)
         ([(control return)] . sanityinc/isearch-exit-other-end))))

(provide 'init-isearch)
;;; init-isearch.el ends here
