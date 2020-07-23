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
    ;; @see https://github.com/company-mode/company-mode/issues/348
    (when (maybe-require-package 'company-statistics)
      (company-statistics-mode))

    (dolist (backend '(company-eclim company-semantic))
      (delq backend company-backends))
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t))
  (global-set-key (kbd "M-C-/") 'company-complete)
  (global-set-key (kbd "C-c y") 'company-yasnippet)

  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends backend)))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(with-eval-after-load 'company
  (with-eval-after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))


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

(provide 'init-company)
;;; init-company.el ends here
