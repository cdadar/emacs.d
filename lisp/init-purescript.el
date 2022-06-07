;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package purescript-mode
  :bind
  (:map purescript-mode-map ("C-o" . open-line))
  :hook
  ((purescript-mode . turn-on-purescript-indentation)
   (purescript-mode . (lambda ()
                        (add-hook 'before-save-hook 'purescript-sort-imports nil t)))
   (purescript-mode . (lambda () (apply-partially 'prettify-symbols-mode -1))))
  :config
  (use-package reformatter
    :config
    (reformatter-define purty
      :program "purty" :lighter " purty"))

  (use-package psc-ide
    :hook
    (purescript-mode . psc-ide-mode)
    :config
    (defun psc-ide-foreign-js-after-save-handler ()
      "Call `psc-ide-rebuild' in any neighbouring purescript file buffer, if `psc-ide-rebuild-on-save' is set.
This is a little magical because it only works if the
corresponding .purs file is open."
      (let ((js-path (buffer-file-name)))
        (when js-path
          (let* ((purs-path (concat (file-name-sans-extension js-path) ".purs"))
                 (purs-buf (get-file-buffer purs-path)))
            (when purs-buf
              (with-current-buffer purs-buf
                (when psc-ide-mode
                  (cond
                   (psc-ide-rebuild-on-save
                    (message "Triggering rebuild of %s" purs-path)
                    (psc-ide-rebuild))))))))))

    (define-minor-mode psc-ide-foreign-js-mode
      "Rebuild corresponding purescript file."
      :init-value nil
      :lighter " PursJS"
      :global nil
      (if psc-ide-foreign-js-mode
          (add-hook 'after-save-hook 'psc-ide-foreign-js-after-save-handler nil t)
        (remove-hook 'after-save-hook 'psc-ide-foreign-js-after-save-handler t))))

  (use-package psci
    :hook
    (purescript-mode . inferior-psci-mode)
    :config
    (use-package add-node-modules-path
      :hook
      (purescript-mode . add-node-modules-path)
      :config
      (advice-add 'psci :around (lambda (oldfun &rest args)
                                  (let ((psci/purs-path (or (executable-find "purs")
                                                            psci/purs-path)))
                                    (apply oldfun args)))))))

(provide 'init-purescript)
;;; init-purescript.el ends here
