;;; init-docker.el --- Work with Docker and its tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :config
  (sanityinc/fullframe-mode 'docker-images-mode)
  (sanityinc/fullframe-mode 'docker-machines-mode)
  (sanityinc/fullframe-mode 'docker-volumes-mode)
  (sanityinc/fullframe-mode 'docker-networks-mode)
  (sanityinc/fullframe-mode 'docker-containers-mode))

(use-package dockerfile-mode)
(use-package docker-compose-mode)


(provide 'init-docker)
;;; init-docker.el ends here
