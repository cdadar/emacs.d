;;; init-helm.el --- Use helm for completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




(when (maybe-require-package 'helm)
  (helm-mode 1)
  (after-load 'diminish
    (diminish 'helm-mode))
  (after-load 'helm
    (require-package 'helm-ls-git)
    (require-package 'helm-c-yasnippet)

    (require 'helm-config)
    (require 'helm-buffers)
    (require 'helm-ls-git)
    (require 'helm-c-yasnippet)
    (require 'helm-for-files)
    (require 'helm-x-files)
    (require 'helm-ring)
    
    (setq helm-buffer-max-length 60) ; make filename has enough width to display full name

    (setq helm-autoresize-max-height 0)
    (setq helm-autoresize-min-height 30)
    (helm-autoresize-mode 1)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t)



    (defvar helm-source-elisp-library
      (helm-build-in-buffer-source  "Elisp libraries (Scan)"
        :data #'helm-locate-library-scan-list
        :fuzzy-match helm-locate-library-fuzzy-match
        :keymap helm-generic-files-map
        :search (unless helm-locate-library-fuzzy-match
                  (lambda (regexp)
                    (re-search-forward
                     (if helm-ff-transformer-show-only-basename
                         (replace-regexp-in-string
                          "\\`\\^" "" regexp)
                       regexp)
                     nil t)))
        :match-part (lambda (candidate)
                      (if helm-ff-transformer-show-only-basename
                          (helm-basename candidate) candidate))
        :filter-one-by-one (lambda (c)
                             (if helm-ff-transformer-show-only-basename
                                 (cons (helm-basename c) c) c))
        :action (helm-actions-from-type-file)))



    ;; MacOS use spotlight instead locate.
    (defvar helm-source-system
      (if *is-a-mac*
          helm-source-mac-spotlight
        helm-source-locate))

    (defun helm-dwim ()
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil)
            helm-source-list)
        (unless helm-source-buffers-list
          (setq helm-source-buffers-list
                (helm-make-source "Buffers" 'helm-source-buffers)))
        (unless helm-source-ls-git
          (setq helm-source-ls-git
                (helm-ls-git-build-ls-git-source)))
        (setq helm-source-list
              '(
		helm-source-buffers-list
                helm-source-ls-git
		helm-source-recentf
		helm-source-kill-ring
		helm-source-system
		helm-source-elisp-library
		helm-source-yasnippet
		))
        (helm-other-buffer helm-source-list "*helm search*")))


    (define-key helm-map (kbd "M-s-j") 'helm-next-source)
    (define-key helm-map (kbd "M-s-k") 'helm-previous-source)
    (define-key helm-map (kbd "M->") 'helm-scroll-other-window-down)
    (define-key helm-map (kbd "M-<") 'helm-scroll-other-window)
    (define-key helm-map (kbd "M-o") 'backward-delete-char-untabify)
    (define-key helm-map (kbd "M-m") 'move-beginning-of-line)


    (global-set-key (kbd "C-c o") #'helm-dwim)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-c h") #'helm-mini)
    (global-set-key (kbd "M-s /") #'helm-occur)
    (global-set-key (kbd "M-s '") #'helm-imenu)
    
    (when (maybe-require-package 'projectile)
      (when (maybe-require-package 'helm-projectile)
        (helm-projectile-on)))

    (when (maybe-require-package 'helm-smex)
      (global-set-key [remap execute-extended-command] #'helm-smex)
      (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)
      )))

(provide 'init-helm)
;;; init-helm.el ends here
