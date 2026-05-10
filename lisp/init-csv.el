;;; init-csv.el --- CSV files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :custom
  (csv-separators '("," ";" "|" " "))
  :config
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'"))

(provide 'init-csv)
;;; init-csv.el ends here
