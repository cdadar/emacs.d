;;; init-compile.el --- Helpers for M-x compile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(defun sanityinc/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

(use-package compile
  :ensure nil
  :bind ([f6] . recompile)
  :custom
  (compilation-scroll-output t)
  :config
  (defun sanityinc/save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer))
  (advice-add 'compilation-start :after #'sanityinc/save-compilation-buffer)

  (defun sanityinc/find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))
  (advice-add 'recompile :around #'sanityinc/find-prev-compilation)

  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'sanityinc/colourise-compilation-buffer)

  (defun endless/send-input (input &optional nl)
    "Send INPUT to the current process.
Interactively also sends a terminating newline."
    (interactive "MInput: \nd")
    (let ((string (concat input (if nl "\n"))))
      (let ((inhibit-read-only t))
        (insert-before-markers string))
      (process-send-string
       (get-buffer-process (current-buffer))
       string)))

  (defun endless/send-self ()
    "Send the pressed key to the current process."
    (interactive)
    (endless/send-input
     (apply #'string
            (append (this-command-keys-vector) nil))))

  (define-key compilation-mode-map (kbd "C-c i") #'endless/send-input)
  (dolist (key '("\C-d" "\C-j" "y" "n"))
    (define-key compilation-mode-map key #'endless/send-self)))

(advice-add 'shell-command-on-region :after #'sanityinc/shell-command-in-view-mode)

(use-package alert
  :config
  ;; Customize `alert-default-style' to get messages after compilation
  (defun sanityinc/alert-after-compilation-finish (buf result)
    "Use `alert' to report compilation RESULT if BUF is hidden."
    (when (buffer-live-p buf)
      (unless (catch 'is-visible
                (walk-windows (lambda (w)
                                (when (eq (window-buffer w) buf)
                                  (throw 'is-visible t))))
                nil)
        (alert (concat "Compilation " result)
               :buffer buf
               :category 'compilation))))
  (add-hook 'compilation-finish-functions #'sanityinc/alert-after-compilation-finish))

(provide 'init-compile)
;;; init-compile.el ends here
