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
  ;; TODO: https://github.com/jdtsmith/kind-icon

  (corfu-history-mode)
  (corfu-popupinfo-mode)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (toggle-chinese-search)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (define-key corfu-map (kbd "C-j") 'corfu-next)
  (define-key corfu-map (kbd "C-k") 'corfu-previous))

 ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
(use-package corfu-terminal
  :after corfu
  :config
  (corfu-terminal-mode))

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
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p" . cape-prefix-map))
  :init
  (setq cape-dabbrev-min-length 3)
 ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'cape-sgml)
  ;; (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-hook 'completion-at-point-functions #'cape-line)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq cape-dabbrev-check-other-buffers nil))

(use-package yasnippet-capf
  :after cape
  :init
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package corfu-english-helper
  :after corfu
  :vc(:url "https://github.com/manateelazycat/corfu-english-helper" :rev :newest)
  :bind
  (("C-c p E" . corfu-english-helper-search)))

(provide 'init-corfu)
;;; init-corfu.el ends here
