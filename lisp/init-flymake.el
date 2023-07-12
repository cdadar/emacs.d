;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :diminish
  :hook
  ((prog-mode text-mode) . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! c" . flymake-start))
  :config
  (unless (version< emacs-version "28.1")
    (setq eldoc-documentation-function 'eldoc-documentation-compose)

    (add-hook 'flymake-mode-hook
            (lambda ()
              (setq eldoc-documentation-functions
                    (cons 'flymake-eldoc-function
                          (delq 'flymake-eldoc-function eldoc-documentation-functions))))))

  (use-package flymake-collection
    :hook
    (after-init  . flymake-collection-hook-setup)
    (python-mode . python-mode-setup-flymake)
    :config
    (defun python-mode-setup-flymake ()
      (add-hook 'flymake-diagnostic-functions 'flymake-collection-pycodestyle nil t)
      (flymake-mode +1))))


(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (when (and (childframe-workable-p)
             (require 'posframe nil t))
    (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
      "Name of the flymake posframe buffer.")
    (defun flymake-diagnostic-at-point-display-posframe (text)
      "Display the flymake diagnostic TEXT inside a child frame."
      (posframe-show
       flymake-posframe-buffer
       :string (propertize
                (concat flymake-diagnostic-at-point-error-prefix text)
                'face (if-let ((type (get-char-property (point) 'flymake-diagnostic)))
                          (pcase (flymake--diag-type type)
                            (:error 'error)
                            (:warning 'warning)
                            (:note 'success)
                            (_ 'default))
                        'default))
           :left-fringe 4
           :right-fringe 4
       :max-width (round (* (frame-width) 0.62))
       :max-height (round (* (frame-height) 0.62))
       :internal-border-width 1
       :internal-border-color (face-background 'posframe-border nil t)
       :background-color (face-background 'tooltip nil t)
       :hidehandler #'flymake-posframe-hidehandler)
      (unwind-protect
          (push (read-event) unread-command-events)
        (progn
          (posframe-hide flymake-posframe-buffer)
          (other-frame 0))))
    (setq flymake-diagnostic-at-point-display-diagnostic-function
          #'flymake-diagnostic-at-point-display-posframe)))

(provide 'init-flymake)
;;; init-flymake.el ends here
