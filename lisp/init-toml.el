;;; init-toml.el --- Support TOML files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package toml-mode
  :hook
  (toml-mode . goto-address-prog-mode))

(provide 'init-toml)
;;; init-toml.el ends here
