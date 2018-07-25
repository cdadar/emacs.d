(when (maybe-require-package 'vimrc-mode)
  (add-to-list 'auto-mode-alist '("\\.vimrc\\'" . vimrc-mode)))

(provide 'init-vimrc)
