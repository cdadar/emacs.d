;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; {{ flyspell setup for web-mode
(defun web-mode-flyspell-verify ()
  (let* ((f (get-text-property (max (point-min) (1- (point))) 'face))
         rlt)
    (cond
     ;; Check the words with these font faces, possibly.
     ;; This *blacklist* will be tweaked in next condition
     ((not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face)))
      (setq rlt t))
     ;; check attribute value under certain conditions
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
        (when (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                  (thing-at-point 'symbol))))))
     ;; finalize the blacklist
     (t
      (setq rlt nil)))
    rlt))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
;; }}

;; {{ flyspell setup for js2-mode
(defun js-flyspell-verify ()
  (let* ((f (get-text-property (max (point-min) (1- (point))) 'face)))
    ;; *whitelist*
    ;; only words with following font face will be checked
    (memq f '(js2-function-call
              js2-function-param
              js2-object-property
              font-lock-variable-name-face
              font-lock-string-face
              font-lock-function-name-face
              font-lock-builtin-face
              rjsx-tag
              rjsx-attr))))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)
;; }}

(use-package flyspell-lazy
  :after flyspell
  :config
  (flyspell-lazy-mode 1))

;; {{ ispell configuration
(defun flyspell-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER will make aspell less capable. So it should only be used in prog-mode-hook."
  (let (args)
    (when ispell-program-name
      (cond
       ((string-match "aspell$" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=en_US"))
        (when run-together
          (setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2")))))
       ((string-match "hunspell$" ispell-program-name)
        (setq args nil))))
    args))

;; Aspell Setup (recommended):
;; Skipped because it's easy.
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;; the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;; save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;; setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;; variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'

(use-package ispell
  :ensure nil
  :demand t
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
   (t (setq ispell-program-name nil)
      (message "You need install either aspell or hunspell for ispell")))

  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))

  (defun cdadar/with-standard-ispell-args (orig-fun &rest args)
    "Run ORIG-FUN with standard ispell arguments, then restore custom args."
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (unwind-protect
          (progn
            ;; use emacs original arguments
            (setq ispell-extra-args (flyspell-detect-ispell-args))
            (apply orig-fun args))
        ;; restore our own ispell arguments
        (setq ispell-extra-args old-ispell-extra-args)
        (ispell-kill-ispell t))))

  (unless (advice-member-p #'cdadar/with-standard-ispell-args 'ispell-word)
    (advice-add 'ispell-word :around #'cdadar/with-standard-ispell-args))
  (unless (advice-member-p #'cdadar/with-standard-ispell-args 'flyspell-auto-correct-word)
    (advice-add 'flyspell-auto-correct-word :around #'cdadar/with-standard-ispell-args)))
;; }}

;; {{ flyspell configuration
(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))

(defun cdadar/enable-flyspell-prog-mode-conditionally ()
  "Enable `flyspell-prog-mode' when spell checking is available."
  (when (can-enable-flyspell-mode)
    (flyspell-prog-mode)))

(use-package flyspell
  :ensure nil
  :bind (("C-c s" . flyspell-auto-correct-word)
         :map flyspell-mode-map
         ("C-." . nil)
         ("C-;" . nil))
  :hook ((text-mode . text-mode-hook-setup)
         (prog-mode . cdadar/enable-flyspell-prog-mode-conditionally))
  :custom
  (flyspell-issue-message-flag nil)
  :config
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
  (unless (advice-member-p #'flyspell-highlight-incorrect-region-hack
                           'flyspell-highlight-incorrect-region)
    (advice-add 'flyspell-highlight-incorrect-region
                :around #'flyspell-highlight-incorrect-region-hack)))

;; Add auto spell-checking in comments for all programming language modes
;; if and only if there is enough memory
;; You can use prog-mode-hook instead.
(defun can-enable-flyspell-mode ()
  (and (not *no-memory*)
       ispell-program-name
       (executable-find ispell-program-name)))

(defun enable-flyspell-mode-conditionally ()
  (when (can-enable-flyspell-mode)
    (flyspell-mode 1)))

;; turn on flyspell-mode for programming languages
;; See the `flyspell' use-package hook above.

;; I don't use flyspell in text-mode because I often write Chinese.
;; I'd rather manually spell check the English text

;; {{ avoid spell-checking doublon (double word) in certain major modes
(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")

(make-variable-buffer-local 'flyspell-check-doublon)

(defun flyspell-highlight-incorrect-region-hack (orig-fun &rest args)
  "Skip doublon highlighting when `flyspell-check-doublon' is nil."
  (when (or flyspell-check-doublon (not (eq 'doublon (nth 2 args))))
    (apply orig-fun args)))
;; }}

(defun my-clean-aspell-dict ()
  "Clean ~/.aspell.pws (dictionary used by aspell)."
  (interactive)
  (let* ((dict (file-truename "~/.aspell.en.pws"))
         (lines (with-temp-buffer
                  (insert-file-contents dict)
                  (split-string (buffer-string) "\n" t)))
         ;; sort words
         (aspell-words (sort (cdr lines) 'string<)))
    (with-temp-file dict
      (insert (format "%s %d\n%s"
                      "personal_ws-1.1 en"
                      (length aspell-words)
                      (mapconcat 'identity aspell-words "\n"))))))

(provide 'init-spelling)
;;; init-spelling.el ends here
