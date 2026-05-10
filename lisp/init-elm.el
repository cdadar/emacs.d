;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :if (executable-find "elm-format")
  :custom
  (elm-format-on-save t))

(use-package elm-test-runner)

(provide 'init-elm)
;;; init-elm.el ends here
