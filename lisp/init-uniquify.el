;;; init-uniquify.el --- Configure uniquification of buffer names -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Nicer naming of buffers for files with identical names
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))


(provide 'init-uniquify)
;;; init-uniquify.el ends here
