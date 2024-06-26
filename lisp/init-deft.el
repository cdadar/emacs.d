;;; init-deft.el --- deft -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package deft
  :config
  (setq deft-extensions '("md" "org")
        deft-default-extension "org"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-recursive t
        deft-file-naming-rules '((nospace . "-"))
        deft-org-mode-title-prefix t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-strip-title-regexp (concat
                                 "\\(?:^%+\\|^#\\+TITLE: *\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|^Title:[	 ]*\\|#+$\\)"
                                 ":PROPERTIES:\n\\(.+\n\\)+:END:"
                                 ))
  :bind
  (("<f8>" . deft)
   ("C-<f8>" . deft-find-file)))

(provide 'init-deft)
;;; init-deft.el ends her
