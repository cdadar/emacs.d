;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package eat
  :init
  (defun sanityinc/on-eat-exit (process)
    (when (zerop (process-exit-status process))
      (kill-buffer)
      (unless (eq (selected-window) (next-window))
        (delete-window))))
  (defcustom sanityinc/eat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "t") 'eat-other-window)
      map)
    "Prefix map for commands that create and manipulate eat buffers.")
  :hook (eat-exit . sanityinc/on-eat-exit)
  :bind (:map global-map ("C-c t" . sanityinc/eat-map))
  :config
  (custom-set-variables
   `(eat-semi-char-non-bound-keys
     (quote ,(cons [?\e ?w] (cl-remove [?\e ?w] eat-semi-char-non-bound-keys :test 'equal)))))
  (eat-update-semi-char-mode-map)
  (eat-reload))

;; (when (maybe-require-package 'eat)
;;   (defun sanityinc/on-eat-exit (process)
;;     (when (zerop (process-exit-status process))
;;       (kill-buffer)
;;       (unless (eq (selected-window) (next-window))
;;         (delete-window))))
;;   (add-hook 'eat-exit-hook 'sanityinc/on-eat-exit)

;;   (defcustom sanityinc/eat-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "t") 'eat-other-window)
;;       map)
;;     "Prefix map for commands that create and manipulate eat buffers.")
;;   (fset 'sanityinc/eat-map sanityinc/eat-map)

;;   (global-set-key (kbd "C-c t") 'sanityinc/eat-map))



(provide 'init-terminals)
;;; init-terminals.el ends here
