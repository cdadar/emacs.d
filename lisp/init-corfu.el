;;; init-corfu.el --- Completion with corfu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (read-extended-command-predicate
  ;;  #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete))

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("M-m" . corfu-move-to-minibuffer)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :hook (prog-mode . cdadar/setup-corfu)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-min-width 80)
  (corfu-max-width 100)
  :init
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  :preface
  (defun cdadar/orderless-dispatch-flex-first (_pattern index _total)
    "orderless-flex for corfu."
    (and (eq index 0) 'orderless-flex))

  (defun cdadar/setup-corfu ()
    "Setup corfu."
    (setq-local orderless-matching-styles '(orderless-flex)
                orderless-style-dispatchers nil)
    (add-hook 'orderless-style-dispatchers #'cdadar/orderless-dispatch-flex-first nil 'local))

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (when (fboundp 'toggle-chinese-search)
        (toggle-chinese-search))
      (require 'consult)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :config
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
  ;;     (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; TODO: https://github.com/jdtsmith/kind-icon

  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
(use-package corfu-terminal
  :after corfu
  :demand t
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; Use dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

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

  ;; Make these capfs composable.
  (unless (advice-member-p #'cape-wrap-noninterruptible 'lsp-completion-at-point)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible))
  (unless (advice-member-p #'cape-wrap-nonexclusive 'lsp-completion-at-point)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive))
  (unless (advice-member-p #'cape-wrap-nonexclusive 'comint-completion-at-point)
    (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive))
  (unless (advice-member-p #'cape-wrap-buster 'eglot-completion-at-point)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
  (unless (advice-member-p #'cape-wrap-nonexclusive 'eglot-completion-at-point)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))
  (unless (advice-member-p #'cape-wrap-nonexclusive 'pcomplete-completions-at-point)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))
  (setq cape-dabbrev-check-other-buffers nil))

(use-package yasnippet-capf
  :commands (yasnippet-capf)
  :init
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package corfu-english-helper
  :after corfu
  :vc (:url "https://github.com/manateelazycat/corfu-english-helper" :rev :newest)
  :bind
  (("C-c p E" . corfu-english-helper-search)))



(provide 'init-corfu)
;;; init-corfu.el ends here
