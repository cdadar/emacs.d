;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Terraform
(use-package terraform-mode)

(with-eval-after-load 'eglot
  (push `((terraform-mode)
          . ,(eglot-alternatives
              '(("terraform-ls" "serve")
                ("tofu-ls" "serve"))))
        eglot-server-programs))

(provide 'init-terraform)
;;; init-terraform.el ends here
