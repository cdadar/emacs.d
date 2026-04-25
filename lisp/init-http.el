;;; init-http.el --- Work with HTTP APIs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package httprepl
  :preface
  (defun sanityinc/httprepl-render-image (buffer)
    "Render HTTPREPL image response in BUFFER using `image-mode'."
    (with-current-buffer buffer
      (image-mode)
      buffer))
  :config
  (push '("image" . image) httprepl-content-type-alist)
  (push `(image . (,#'sanityinc/httprepl-render-image))
        httprepl-content-type-middleware-alist))

(use-package restclient
  :preface
  (defun sanityinc/restclient ()
    "Work with `rest' in the *restclient* buffer."
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer))))
  :mode ("\\.rest\\'" . restclient-mode))

(provide 'init-http)
;;; init-http.el ends here
