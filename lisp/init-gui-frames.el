;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Stop C-z from minimizing windows under OS X

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(use-package frame
  :ensure nil
  :bind (("C-z" . sanityinc/maybe-suspend-frame))
  :custom
  (use-file-dialog nil)
  (use-dialog-box nil)
  (inhibit-startup-screen t)
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
  (menu-bar-mode -1)
  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border)))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(defun sanityinc/decrease-opacity ()
  "Decrease the current frame opacity slightly."
  (interactive)
  (sanityinc/adjust-opacity nil -2))

(defun sanityinc/increase-opacity ()
  "Increase the current frame opacity slightly."
  (interactive)
  (sanityinc/adjust-opacity nil 2))

(defun sanityinc/reset-opacity ()
  "Reset the current frame opacity to 100."
  (interactive)
  (modify-frame-parameters nil '((alpha . 100))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  (global-set-key (kbd "M-ƒ") #'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "C-M-8") #'sanityinc/decrease-opacity)
(global-set-key (kbd "C-M-9") #'sanityinc/increase-opacity)
(global-set-key (kbd "C-M-7") #'sanityinc/reset-opacity)


(when *is-a-mac*
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode)))


(use-package frame
  :ensure nil
  :custom
  (frame-title-format
   '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
              "%b")))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (setq line-spacing 0))))


;; Change global font size easily

(use-package default-text-scale
  :hook
  (after-init . default-text-scale-mode))


(use-package disable-mouse)


(use-package pixel-scroll
  :ensure nil
  :if (fboundp 'pixel-scroll-precision-mode)
  :config
  (pixel-scroll-precision-mode))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
