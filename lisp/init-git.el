;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(use-package git-modes)
(use-package git-timemachine
  :bind
  ("C-x v t" . git-timemachine-toggle))

(use-package git-link)

(defun sanityinc/magit-or-vc-log-file (&optional follow)
  "Show Git history for the current file, or fall back to `vc-print-log'.
With prefix argument FOLLOW, ask Magit to follow renames for the current file."
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (magit-log-buffer-file follow)
    (vc-print-log)))

(defun magit-submodule-remove+ ()
  "Force-remove a Magit submodule chosen from the current repository."
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))

(defun sanityinc/vc-msg-magit-find-file ()
  "Visit the file for the commit currently described by `vc-msg'."
  (let* ((info vc-msg-previous-commit-info)
         (git-dir (locate-dominating-file default-directory ".git")))
    (magit-find-file (plist-get info :id)
                     (concat git-dir (plist-get info :filename)))))

(use-package magit
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-visit-prefer-worktree t)
  :bind
  (
   ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
   ;; quickly open magit on any one of your projects.
   ([(meta f12)] . magit-status)
   ("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-x M-f" . magit-file-dispatch)
   :map magit-status-mode-map
   ("C-M-<up>" . magit-section-up))
  :hook
  (git-commit-mode . goto-address-mode)
  :config
  (sanityinc/fullframe-mode 'magit-status-mode)
  )


(use-package magit-todos)

(use-package vc-msg
  :config
  ;; show code of commit
  (setq vc-msg-git-show-commit-function 'magit-show-commit)
  ;; open file of certain revision
  (push '("m"
          "[m]agit-find-file"
          sanityinc/vc-msg-magit-find-file)
        vc-msg-git-extra))


;; Convenient binding for vc-git-grep
(use-package vc
  :ensure nil
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep)
              ("l" . sanityinc/magit-or-vc-log-file)))


;;; git-svn support
;; (use-package magit-svn
;;   :config
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   :hook
;;   (magit-stash-mode . sanityinc/maybe-enable-magit-svn-mode))




(use-package compile
  :ensure nil
  :config
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\(.*\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil
  "Cached list of git svn subcommands")

(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read
                      "git-svn command: "
                      (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

(provide 'init-git)
;;; init-git.el ends here
