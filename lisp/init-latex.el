;;; init-latex.el ---  Support for latex and derivatives  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package cdlatex)
(use-package auctex
  :hook
  ;; Going to see if we actually need these
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . abbrev-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . (lambda ()
                       (LaTeX-add-environments
                        '("theorem" LaTeX-env-label)
                        '("proposition" LaTeX-env-label)
                        '("lemma" LaTeX-env-label)
                        '("definition" LaTeX-env-label)
                        '("example" LaTeX-env-label)
                        '("remark" LaTeX-env-label))))
  (LaTeX-mode . (lambda ()
                       (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                       (setq TeX-command-default "XeLaTeX")
                       (setq TeX-save-query  nil )
                       (setq TeX-show-compilation t)))
  :config
  (when (locate-library "auctex")
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-close-quote "")
    (setq TeX-open-quote "")

    (setq reftex-plug-into-AUCTeX t)
    (setq reftex-use-external-file-finders t)
    (setq reftex-external-file-finders
          '(("tex" . "kpsewhich -format=.tex %f")
            ("bib" . "kpsewhich -format=.bib %f")))

    ;; Bibretrieve
    (setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5)))))


(provide 'init-latex)
;;; init-latex.el ends here
