;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package origami
  :bind
  (:map origami-mode-map
        ("C-c f" . origami-recursively-toggle-node)
        ("C-c F" . origami-toggle-all-nodes)))

(provide 'init-folding)
;;; init-folding.el ends here
