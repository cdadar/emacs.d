;;; init-latex.el ---  Support for latex and derivatives  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(use-package cdlatex)
(use-package auctex
  :ensure nil
  :preface
  (defun cdadar/latex-add-common-environments ()
    (LaTeX-add-environments
     '("theorem" LaTeX-env-label)
     '("proposition" LaTeX-env-label)
     '("lemma" LaTeX-env-label)
     '("definition" LaTeX-env-label)
     '("example" LaTeX-env-label)
     '("remark" LaTeX-env-label)))

  (defun cdadar/latex-add-xelatex-command ()
    (add-to-list 'TeX-command-list
                 '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
  :hook
  ;; Going to see if we actually need these
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . abbrev-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . cdadar/latex-add-common-environments)
  (LaTeX-mode . cdadar/latex-add-xelatex-command)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-close-quote "")
  (TeX-open-quote "")
  (TeX-command-default "XeLaTeX")
  (TeX-save-query nil)
  (TeX-show-compilation t)
  (reftex-plug-into-AUCTeX t)
  (reftex-use-external-file-finders t)
  (reftex-external-file-finders
   '(("tex" . "kpsewhich -format=.tex %f")
     ("bib" . "kpsewhich -format=.bib %f")))
  (bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5))))


(provide 'init-latex)
;;; init-latex.el ends here
