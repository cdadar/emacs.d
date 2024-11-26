;;; init-zig.el --- Support for the Zig language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package zig-ts-mode
  :if (and  (fboundp 'treesit-ready-p) (treesit-ready-p 'zig))
  :mode ("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode)
  :config
  (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")))))

(use-package zig-mode)


(provide 'init-zig)
;;; init-zig.el ends here
