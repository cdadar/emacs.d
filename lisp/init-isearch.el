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
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (search-whitespace-regexp "[[:space:]]+")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (lazy-highlight-cleanup nil)
  (isearch-wrap-pause 'no)
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll 'unlimited)
  (search-default-mode #'char-fold-to-regexp)
  :config
  (defun sanityinc/isearch-occur ()
    "Invoke `consult-line' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))

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

  (defun sanityinc/isearch-dwim ()
    "Start `isearch-forward' with the active region or symbol at point."
    (interactive)
    (let ((input (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (when (use-region-p)
        (deactivate-mark))
      (isearch-forward nil 1)
      (when (and input (not (string-empty-p input)))
        (isearch-yank-string input))))

  (defun sanityinc/isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  :bind
  (("C-s" . sanityinc/isearch-dwim)
   ("C-r" . isearch-backward)
   :map isearch-mode-map
   ([remap isearch-delete-char] . isearch-del-char)
   ("C-o" . sanityinc/isearch-occur)
   ("C-c C-o" . sanityinc/isearch-occur)
   ("C-M-w" . isearch-yank-symbol)
   ("C-<return>" . sanityinc/isearch-exit-other-end)
   ("C-<backspace>" . isearch-del-char)
   ("<backspace>" . isearch-del-char)
   ("DEL" . isearch-del-char)
   ("<escape>" . isearch-cancel)))

(provide 'init-isearch)
;;; init-isearch.el ends here
