;;; init-corfu.el --- Completion with corfu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun cdadar/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun cdadar/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'cdadar/orderless-dispatch-flex-first nil 'local))


(use-package corfu
  :init
  (setq corfu-cycle t)
  (setq corfu-auto t)

  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 80)
  (setq corfu-max-width 100)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (global-corfu-mode)
  :hook (prog-mode . cdadar/setup-corfu)
  :config
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
  ;;     (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (toggle-chinese-search)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (define-key corfu-map (kbd "C-j") 'corfu-next)
  (define-key corfu-map (kbd "C-k") 'corfu-previous))

(use-package corfu-doc
  :init
  ;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "s-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "s-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "s-n") #'corfu-doc-scroll-up)   ;; corfu-previous
  )

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (setq cape-dabbrev-min-length 3)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (setq cape-dabbrev-check-other-buffers nil)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

(with-eval-after-load 'corfu
  (require 'corfu-english-helper)
  (global-set-key (kbd "C-c e E") 'corfu-english-helper-search))

;; (with-eval-after-load 'corfu
;;   (use-package kind-icon
;;     :config
;;     (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;     (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(provide 'init-corfu)
;;; init-corfu.el ends here
