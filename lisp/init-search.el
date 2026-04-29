;;; init-search.el --- copy prelude-search  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-


;; 从 prelude 中弄出来搜索的功能
(defun prelude-search (query-url prompt)
  "Open QUERY-URL with a query read using PROMPT or the active region."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Install an interactive command for SEARCH-ENGINE-NAME.
SEARCH-ENGINE-URL is the query URL prefix.  SEARCH-ENGINE-PROMPT is
used when reading a query from the minibuffer."
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")
(prelude-install-search-engine "codeif" "https://unbug.github.io/codelf/#"              "Codeif: ")



(use-package emacs
  :ensure nil
  :bind (("C-c s g" . prelude-google)
         ("C-c s G" . prelude-github)
         ("C-c s E" . prelude-codeif)
         ;; ("C-c y" . prelude-youtube)
         ("C-c s U" . prelude-duckduckgo)))

(use-package p-search
  :vc (:url "https://github.com/zkry/p-search" :rev :newest)
  :commands (p-search))



(provide 'init-search)
;;; init-search.el ends here
