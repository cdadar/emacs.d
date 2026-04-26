;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;; Navigate window layouts with "C-c <left>" and "C-c <right>"

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))


;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-timeout nil)
  :bind
  (("C-x o" . switch-window)))


;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defun sanityinc/split-window-below-and-focus (&optional arg)
  "Split the current window below and focus the new window unless ARG is provided."
  (interactive "P")
  (funcall (split-window-func-with-other-buffer #'split-window-vertically) arg))

(defun sanityinc/split-window-right-and-focus (&optional arg)
  "Split the current window right and focus the new window unless ARG is provided."
  (interactive "P")
  (funcall (split-window-func-with-other-buffer #'split-window-horizontally) arg))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(use-package window
  :ensure nil
  :bind (("C-x 2" . sanityinc/split-window-below-and-focus)
         ("C-x 3" . sanityinc/split-window-right-and-focus)
         ("C-x 1" . sanityinc/toggle-delete-other-windows)
         ("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ("<f7>" . sanityinc/split-window)
         ("C-c <down>" . sanityinc/toggle-current-window-dedication)))



;; Rearrange split windows

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))



;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs

(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))





(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


(unless (memq window-system '(nt w32))
  (use-package windswap
    :config
    (windmove-default-keybindings 'control)
    (windswap-default-keybindings 'shift 'control)))


(use-package pulsar
  :custom
  (pulsar-pulse-region-functions nil)
  :config
  (pulsar-global-mode t))


(provide 'init-windows)
;;; init-windows.el ends here
