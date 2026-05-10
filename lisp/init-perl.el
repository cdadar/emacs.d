;;; init-perl.el --- perl config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cperl-mode
  :ensure nil
  :init
  (defalias 'perl-mode 'cperl-mode)
  :bind (:map help-command ("P" . cperl-perldoc))
  :custom
  (cperl-indent-level 4)
  (cperl-continued-statement-offset 8)
  (cperl-font-lock t)
  (cperl-electric-lbrace-space t)
  (cperl-electric-parens nil)
  (cperl-electric-linefeed nil)
  (cperl-electric-keywords nil)
  (cperl-info-on-command-no-prompt t)
  (cperl-clobber-lisp-bindings t)
  (cperl-lazy-help-time 3)
  (cperl-invalid-face nil)
  :hook
  (cperl-mode . (lambda ()
                   (set-face-background 'cperl-array-face nil)
                   (set-face-background 'cperl-hash-face nil))))

(provide 'init-perl)
;;; init-perl.el ends here
