;;; init-frame-hooks.el --- Provide specific hooks for GUI/TTY frame creation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(defconst sanityinc/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(defun sanityinc/run-after-make-initial-frame-hooks ()
  "Run frame creation hooks for `sanityinc/initial-frame'."
  (when sanityinc/initial-frame
    (run-after-make-frame-hooks sanityinc/initial-frame)))

(use-package emacs
  :ensure nil
  :init
  (add-hook 'after-make-frame-functions #'run-after-make-frame-hooks)
  (add-hook 'after-init-hook #'sanityinc/run-after-make-initial-frame-hooks))


(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
