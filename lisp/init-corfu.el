;;; init-corfu.el --- Completion with corfu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(when (maybe-require-package 'corfu)
  (add-hook 'after-init-hook 'corfu-global-mode)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  ;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
  ;; in the Consult wiki for an advanced Orderless style dispatcher.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (with-eval-after-load 'orderless
    (setq completion-styles '(orderless partial-completion)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion)))))))


(with-eval-after-load 'corfu
  (when (maybe-require-package 'cape)
    (add-hook 'after-init-hook
              '(lambda ()
                 (progn
                   ;; Add `completion-at-point-functions', used by `completion-at-point'.
                   (add-to-list 'completion-at-point-functions #'cape-file)
                   (add-to-list 'completion-at-point-functions #'cape-tex)
                   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
                   (add-to-list 'completion-at-point-functions #'cape-keyword)
                   (add-to-list 'completion-at-point-functions #'cape-symbol)
                   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
                   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
                   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
                   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
                   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
                   ;;(add-to-list 'completion-at-point-functions #'cape-line)
                   )))))

(with-eval-after-load 'corfu
  (when (maybe-require-package 'corfu-doc)
    (add-hook 'corfu-mode-hook #'corfu-doc-mode)
    (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
    (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
    ))

(provide 'init-corfu)
;;; init-corfu.el ends here
