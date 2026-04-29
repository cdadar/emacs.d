;;; init-gud.el --- Configure GUD debugging helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar gud-overlay
  (let ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defun cdadar/gud-highlight-current-line (true-file line &optional _column)
  "Highlight LINE in TRUE-FILE after `gud-display-line'."
  (let ((ov gud-overlay)
        (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                      (current-buffer))))))

(defun cdadar/gud-delete-overlay-on-buffer-kill ()
  "Delete `gud-overlay' when killing a GUD buffer."
  (when (derived-mode-p 'gud-mode)
    (delete-overlay gud-overlay)))

;; {{ hack buffer
;; move the cursor to the end of last line if it's gud-mode
(defun cdadar/gud-move-to-buffer-end ()
  "Move point to the end of the buffer when the current buffer is a GUD buffer."
  (when (derived-mode-p 'gud-mode)
    (goto-char (point-max))))

(defun cdadar/gud-move-to-buffer-end-after (&rest _)
  "Run `cdadar/gud-move-to-buffer-end' after window or buffer selection."
  (cdadar/gud-move-to-buffer-end))
;; }}

(defun gud-cls (&optional num)
  "Clear the GUD screen.
With prefix NUM greater than 1, recenter twice."
  (interactive "p")
  (let ((old-window (selected-window)))
    (save-excursion
      (cond
       ((buffer-live-p (get-buffer "*gud-main*"))
        (select-window (get-buffer-window "*gud-main*"))
        (goto-char (point-max))
        (recenter-top-bottom)
        (when (> num 1)
          (recenter-top-bottom))
        (select-window old-window))
       (t (error "GUD buffer doesn't exist!"))))))

(defun gud-kill-yes ()
  "Run `gud-kill' in the main GUD buffer and confirm with y."
  (interactive)
  (let ((old-window (selected-window)))
    (save-excursion
      (cond
       ((buffer-live-p (get-buffer "*gud-main*"))
        (gud-kill nil)
        (select-window (get-buffer-window "*gud-main*"))
        (insert "y")
        (comint-send-input)
        (recenter-top-bottom)
        (select-window old-window))
       (t (error "GUD buffer doesn't exist!"))))))

(use-package emacs
  :ensure nil
  :hook (kill-buffer . cdadar/gud-delete-overlay-on-buffer-kill)
  :config
  (advice-add 'switch-to-buffer :after #'cdadar/gud-move-to-buffer-end-after)
  (advice-add 'select-window-by-number :after #'cdadar/gud-move-to-buffer-end-after)
  ;; windmove-do-window-select is from windmove.el
  (advice-add 'windmove-do-window-select :after #'cdadar/gud-move-to-buffer-end-after))

(use-package gud
  :ensure nil
  :bind ("C-x C-a C-g" . gud-run)
  :config
  (advice-add 'gud-display-line :after #'cdadar/gud-highlight-current-line)
  (gud-def gud-kill "kill" "\C-k" "Kill the debugee")
  ;; `gud' reinitializes the C-x C-a prefix map when loaded, so keep the
  ;; binding installed after the package is required too.
  (global-set-key (kbd "C-x C-a C-g") #'gud-run))

(provide 'init-gud)
;;; init-gud.el ends here
