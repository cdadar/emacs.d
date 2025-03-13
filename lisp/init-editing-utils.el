;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

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
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)



;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

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



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-ts-mode-hook 'display-line-numbers-mode))



(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))



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
(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'repeat-mode)
  (add-hook 'after-init-hook 'repeat-mode))

;;; Expand region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))


;;; Handy key bindings
(with-eval-after-load 'help
  (define-key help-map "A" 'describe-face))

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
                   (lambda (s1 s2) (eq (random 2) 0)))))))



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

(when (executable-find "rg") (use-package deadgrep))

;; (use-package rime
;;   :config
;;   (setq default-input-method "rime")
;;   )

(when (use-package comment-dwim-2
        :bind
        (("M-;" . comment-dwim-2))))

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

;;禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 将所选区域缩小到其先前的带大小的快捷键
(with-eval-after-load 'expand-region
  (global-set-key (kbd "C-|") 'er/contract-region))

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
                   (if (file-remote-p buffer-file-name) super-save-remote-files t))
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


(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t)
  :bind
  (:map grep-mode-map
   ("C-c C-q" . wgrep-change-to-wgrep-mode)
   :map occur-mode-map
   ("C-c C-q" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-auto-save-buffer t))

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


(defun remove-it (it list)
  (if (null list) 0
    (if (= it (car list))
        (delete (car list))
      (remove-it (cdr list)))))


(defun add-space-between-chinese-and-english ()
  "在中英文之间自动添加空格。"
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point)))))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; 检查前一个字符不是空格
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))))

(defun is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                )))

(defun delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(define-minor-mode auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))


(use-package restart-emacs)

(use-package pangu-spacing)


;; (use-package wakatime-mode
;;   :init
;;   (global-wakatime-mode))

(use-package opencc)



;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))





(defun process-pasted-text (text prev-char next-char)
  "Process pasted TEXT to add spaces between Chinese and English characters, considering PREV-CHAR and NEXT-CHAR."
  (with-temp-buffer
    (insert (if prev-char (concat (char-to-string prev-char) text) text))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-char (char-after))
            (next-char-internal (char-after (1+ (point)))))
        (when (and current-char next-char-internal
                   (should-insert-space current-char next-char-internal)
                   (not (eq (char-after) ?\s)))
          (save-excursion
            (goto-char (1+ (point)))
            (insert " "))))
      (forward-char))
    (let ((buffer-content (buffer-string)))
      (if prev-char
          (setq buffer-content (substring buffer-content 1)))
      ;; Add space between the last char of pasted text and next-char
      (setq buffer-content (insert-space-if-needed
                            (aref buffer-content (1- (length buffer-content)))
                            next-char
                            buffer-content))
      buffer-content)))


(use-package posframe)



;; (defun auto-space-yank-advice (orig-fun &rest args)
;;   "Advice to automatically add spaces between Chinese and English characters after yanking."
;;   (let ((beg (point))
;;         (prev-char (char-before)))
;;     (apply orig-fun args)
;;     (let ((end (point))
;;           (next-char (char-after)))
;;       (let ((pasted-text (buffer-substring-no-properties beg end)))
;;         (delete-region beg end)
;;         (insert (process-pasted-text pasted-text prev-char next-char))))))

;; (advice-add 'yank :around #'auto-space-yank-advice)
;; (advice-add 'yank-pop :around #'auto-space-yank-advice)

;; (defun add-space-between-chinese-and-english ()
;;   "Automatically add a space between Chinese and English characters."
;;   (let ((current-char (char-before))
;;         (prev-char (char-before (1- (point))))
;;         (next-char (char-after)))
;;     (when (and current-char prev-char
;;                (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
;;                    (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
;;                (not (eq prev-char ?\s))) ; Check if the previous character is a space
;;       (save-excursion
;;         (goto-char (1- (point)))
;;         (insert " ")))
;;     (when (and current-char next-char
;;                (or (and (is-chinese-character current-char) (is-halfwidth-character next-char))
;;                    (and (is-halfwidth-character current-char) (is-chinese-character next-char)))
;;                (not (eq current-char ?\s))) ; Check if the current character is a space
;;       (save-excursion
;;         (goto-char (point))
;;         (insert " ")))))




(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
