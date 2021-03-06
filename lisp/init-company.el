;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (when (maybe-require-package 'company-prescient)
      (require 'prescient)
      (prescient-persist-mode 1)
      (company-prescient-mode))
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-mode-map [remap completion-at-point] 'company-complete)
    (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (setq-default company-dabbrev-downcase nil
                  company-dabbrev-other-buffers 'all
                  ;; Some languages use camel case naming convention,
                  ;; so company should be case sensitive.
                  company-dabbrev-ignore-case nil
                  ;; make previous/next selection in the popup cycles
                  company-selection-wrap-around t
                  company-require-match nil
                  company-etags-ignore-case t
                  company-idle-delay 0.2
                  ;; press M-number to choose candidate
                  company-show-numbers t
                  company-tooltip-limit 10
                  company-minimum-prefix-length 2
                  company-clang-insert-arguments nil
                  ;; @see https://github.com/company-mode/company-mode/issues/146
                  company-tooltip-align-annotations t))
  (global-set-key (kbd "M-C-/") 'company-complete)
  (global-set-key (kbd "C-c y") 'company-yasnippet)

  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends backend)))

(with-eval-after-load 'company-etags
  '(progn
     ;; insert major-mode not inherited from prog-mode
     ;; to make company-etags work
     (defadvice company-etags--candidates (around company-etags--candidates-hack activate)
       (let* ((prefix (car (ad-get-args 0)))
              (tags-table-list (company-etags-buffer-table))
              (tags-file-name tags-file-name)
              (completion-ignore-case company-etags-ignore-case))
         (and (or tags-file-name tags-table-list)
              (fboundp 'tags-completion-table)
              (save-excursion
                (unless (and company-etags-timer
                             tags-completion-table
                             (> (length tags-completion-table) 0)
                             (< (- (float-time (current-time)) (float-time company-etags-timer))
                                company-etags-update-interval))
                  (setq company-etags-timer (current-time))
                  ;; `visit-tags-table-buffer' will check the modified time of tags file. If it's
                  ;; changed, the tags file is reloaded.
                  (visit-tags-table-buffer))
                ;; In function `tags-completion-table', cached variable `tags-completion-table' is
                ;; accessed at first. If the variable is empty, it is set by parsing tags file
                (all-completions prefix (tags-completion-table))))))))

(with-eval-after-load 'company
  (when (maybe-require-package 'company-ctags)
    (company-ctags-auto-setup)))


(with-eval-after-load 'company
  (require 'company-english-helper)
  (global-set-key (kbd "M-C-y") 'toggle-company-english-helper))

(with-eval-after-load 'company
  (when (maybe-require-package 'company-try-hard)
    (global-set-key (kbd "C-z") #'company-try-hard)
    (define-key company-active-map (kbd "C-z") #'company-try-hard)))

(provide 'init-company)
;;; init-company.el ends here
