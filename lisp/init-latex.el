;;; init-latex.el ---  Support for latex and derivatives  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(maybe-require-package 'cdlatex)
(when (maybe-require-package 'auctex)
  (when (locate-library "auctex")
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-close-quote "")
    (setq TeX-open-quote "")
    ;; Going to see if we actually need these
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
    ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'abbrev-mode)

    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (LaTeX-add-environments
                 '("theorem" LaTeX-env-label)
                 '("proposition" LaTeX-env-label)
                 '("lemma" LaTeX-env-label)
                 '("definition" LaTeX-env-label)
                 '("example" LaTeX-env-label)
                 '("remark" LaTeX-env-label))))

    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                (setq TeX-command-default "XeLaTeX")
                (setq TeX-save-query  nil )
                (setq TeX-show-compilation t)))

    (setq reftex-plug-into-AUCTeX t)
    (setq reftex-use-external-file-finders t)
    (setq reftex-external-file-finders
          '(("tex" . "kpsewhich -format=.tex %f")
            ("bib" . "kpsewhich -format=.bib %f")))

    ;; Company
    (with-eval-after-load 'company
      (when (maybe-require-package 'company-auctex)
        (add-to-list 'company-backends 'company-auctex))

      (when (maybe-require-package 'company-bibtex)
        (add-to-list 'company-backends 'company-bibtex))

      (when (maybe-require-package 'company-reftex)
        (add-to-list 'company-backends 'company-reftex))

      (when (maybe-require-package 'company-math)
        (add-to-list 'company-backends 'company-math)))

    ;; Bibretrieve
    (setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5)))))

(provide 'init-latex)
;;; init-latex.el ends here
