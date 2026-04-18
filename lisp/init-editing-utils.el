;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package unfill)

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package electric
  :ensure nil
  :hook (after-init . electric-indent-mode))

(use-package list-unicode-display)


;;; Some basic preferences

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 fill-column 120
 truncate-lines nil
 truncate-partial-width-windows nil
 warning-minimum-level :error)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(use-package simple
  :ensure nil
  :hook ((after-init . transient-mark-mode)
         (after-init . size-indication-mode))
  :init
  (setq column-number-mode t))



;; Huge files

(use-package so-long
  :ensure nil
  :hook (after-init . so-long-enable))

(use-package vlf
  :config
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))




;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :hook
  (after-init . mode-line-bell-mode))


;;; Newline behaviour (see also electric-indent-mode, enabled above)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-width 3)
  :hook
  (prog-mode . display-line-numbers-mode)
  (yaml-mode . display-line-numbers-mode)
  (yaml-ts-mode . display-line-numbers-mode))



(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (indicate-buffer-boundaries 'left)
  (display-fill-column-indicator-character ?┊)
  :hook (prog-mode . display-fill-column-indicator-mode))



(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package symbol-overlay
  :hook
  ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind
  (:map symbol-overlay-mode-map
        ("M-i" . symbol-overlay-put)
        ("M-I" . symbol-overlay-remove-all)
        ("M-n" . symbol-overlay-jump-next)
        ("M-p" . symbol-overlay-jump-prev)))


(use-package undo-tree
  :diminish
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil))





;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-separator "\f")
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))
  :bind
  (("M-Y" . browse-kill-ring)
   :map browse-kill-ring-mode-map
   ("C-g" . browse-kill-ring-quit)
   ("M-n" . browse-kill-ring-forward)
   ("M-p" . browse-kill-ring-previous)))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Show matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

;;; Expand region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-|" . er/contract-region)))


;;; Handy key bindings
(use-package help
  :ensure nil
  :bind (:map help-map
              ("A" . describe-face)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(use-package avy
  :bind
  (("C-;" . avy-goto-char-timer)))

(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

(use-package multiple-cursors
  :bind (
         ;; multiple-cursors
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-+" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ;; From active region to multiple cursors:
         ("C-c M r" . set-rectangular-region-anchor)
         ("C-c M c" . mc/edit-lines)
         ("C-c M e" . mc/edit-ends-of-lines)
         ("C-c M a" . mc/edit-beginnings-of-lines)))

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)



;;; Page break lines
(use-package page-break-lines
  :diminish
  :hook
  (after-init . global-page-break-lines-mode))



;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(use-package move-dup
  :config
  (move-dup-mode)
  :bind
  (([M-S-up] . move-dup-move-lines-up)
   ([M-S-down] . move-dup-move-lines-down)
   ("C-c d" . move-dup-duplicate-down)
   ("C-c u" . move-dup-duplicate-up)))



;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up



;;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :diminish
  :hook
  (after-init . whole-line-or-region-global-mode))



;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)


;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (_ _) (eq (random 2) 0)))))))



(use-package highlight-escape-sequences
  :hook
  (after-init . hes-mode))


(use-package which-key
  :hook
  (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq-default which-key-idle-delay 1.5))


(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


(use-package imenu-anywhere
  :bind
  (("M-s '" . imenu-anywhere)))

(use-package string-inflection
  :bind
  (("C-c C-u" . string-inflection-all-cycle))
  :hook
  (ruby-mode . (lambda ()
                 (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
  (java-mode . (lambda ()
                 (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))
  (python-mode . (lambda ()
                   (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle))))

(use-package deadgrep
  :if (executable-find "rg"))

;; (use-package rime
;;   :config
;;   (setq default-input-method "rime")
;;   )

(use-package comment-dwim-2
  :bind
  (("M-;" . comment-dwim-2)))

(use-package comment-tags
  :config
  (setq comment-tags-keymap-prefix (kbd "C-c #"))
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  :hook
  (prog-mode . comment-tags-mode)
  )

(use-package iedit)

;; 删除多余空白插件
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package define-word
  :bind
  (("C-c C-d" . define-word-at-point)
   ("C-c C-S-d" . define-word)))


(use-package super-save
  :diminish
  :hook
  (after-init . super-save-mode)
  :config
  (setq-default auto-save-default nil)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-hook-triggers
        '(focus-out-hook find-file-hook))
  (setq super-save-triggers
        '(other-window
          ace-window
          balance-windows
          next-buffer
          previous-buffer
          org-babel-execute-src-block
          split-window-below
          split-window-horizontally
          start-process-shell-command
          switch-to-buffer
          windmove-down
          windmove-left
          windmove-right
          windmove-up
          magit-status))

  (defun save-all-buffers ()
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (and buffer-file-name
                   (buffer-modified-p (current-buffer))
                   (file-writable-p buffer-file-name)
                   (if (file-remote-p buffer-file-name) super-save-remote-files t)
                   ;; 跳过有解密条目的 org buffer，避免自动重新加密
                   (not (bound-and-true-p cdadar/org-crypt-decrypting)))
          (save-buffer)))))

  (advice-add 'super-save-command :override 'save-all-buffers))

(use-package hl-todo
  :config
  (global-hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("C-c M-p" . hl-todo-previous)
        ("C-c M-n" . hl-todo-next)
        ("C-c M-o" . hl-todo-occur)))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t)
  :bind
  (:map grep-mode-map
        ("C-c C-q" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-q" . wgrep-change-to-wgrep-mode)))

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

(use-package insert-translated-name
  :vc(:url "https://github.com/manateelazycat/insert-translated-name" :rev :newest)
  :commands insert-translated-name-insert)



(use-package clip2org
  :vc(:url "https://github.com/thamer/clip2org" :rev :newest))




(use-package advance-words-count
  :ensure nil
  :load-path "site-lisp/advance-works-count"
  :init
  (require 'advance-wc-mode))


(use-package delete-block
  :vc(:url "https://github.com/manateelazycat/delete-block" :rev :newest))


(use-package color-rg
  :vc(:url "https://github.com/manateelazycat/color-rg" :rev :newest)
  :bind
  (("M-s M-r" . color-rg-search-input)
   :map isearch-mode-map ("M-s M-s" . isearch-toggle-color-rg)))



(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive
   (list (expand-file-name
          (if buffer-file-name
              (read-file-name "Move file to: ")
            (read-file-name "Move file to: "
                            default-directory
                            (expand-file-name (file-name-nondirectory (buffer-name))
                                              default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(use-package restart-emacs)

(use-package pangu-spacing)

(use-package opencc)



;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(use-package thing-edit
  :vc (:url "https://github.com/manateelazycat/thing-edit" :rev :newest))




(use-package apheleia
  :init (apheleia-global-mode))


(use-package completion-preview
  :ensure nil
  :if (and (fboundp 'completion-preview-mode)
           (boundp 'completion-preview-active-mode-map)
           (boundp 'completion-in-region-mode-map))
  :hook ((prog-mode . completion-preview-mode) )
  :bind
  ( :map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate))
  :config
  (define-key completion-in-region-mode-map (kbd "M-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "M-p") 'minibuffer-previous-completion)
  )

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
