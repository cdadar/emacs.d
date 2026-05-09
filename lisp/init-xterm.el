;;; init-xterm.el --- Integrate with terminals such as xterm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-frame-hooks)

(defun sanityinc/scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun sanityinc/scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(use-package mwheel
  :ensure nil
  :commands (mwheel-install))

(use-package xt-mouse
  :ensure nil
  :commands (xterm-mouse-mode))

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))



(use-package emacs
  :ensure nil
  :bind (([mouse-4] . sanityinc/scroll-down-one-line)
         ([mouse-5] . sanityinc/scroll-up-one-line))
  :init
  (add-hook 'after-make-console-frame-hooks #'sanityinc/console-frame-setup))

(provide 'init-xterm)
;;; init-xterm.el ends here
