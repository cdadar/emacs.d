;;; init-git.el --- Version control and Git support -*- lexical-binding: t -*-
;;; Commentary:

;; Most GitHub-specific packages are configured separately: see
;; init-github.el.

;;; Code:

(defun cdadar/vc-dir-current-should-skip-p ()
  "Return non-nil when the current vc-dir line should be skipped."
  (when vc-ewoc
    (let* ((node (ewoc-locate vc-ewoc))
           (data (and node (ewoc-data node))))
      (when data
        (or (vc-dir-fileinfo->directory data)
            (eq (vc-dir-fileinfo->state data) 'up-to-date))))))

(defun cdadar/vc-dir-move-and-diff (move-fn wrap-pos)
  "Move with MOVE-FN, wrapping to WRAP-POS, then show diff for the next changed file."
  (let ((start-node (and vc-ewoc (ewoc-locate vc-ewoc)))
        (wrapped nil))
    (catch 'done
      (while t
        (let ((prev-node (and vc-ewoc (ewoc-locate vc-ewoc))))
          (funcall move-fn 1)
          (let ((cur-node (and vc-ewoc (ewoc-locate vc-ewoc))))
            ;; 到达边界无法继续时，回环到另一端
            (when (and (eq cur-node prev-node) (not wrapped))
              (setq wrapped t)
              (goto-char wrap-pos)
              (funcall move-fn 1)
              (setq cur-node (and vc-ewoc (ewoc-locate vc-ewoc))))
            ;; 已遍历一圈回到起始节点
            (when (eq cur-node start-node)
              (message "No edited files found")
              (throw 'done nil))
            (unless (cdadar/vc-dir-current-should-skip-p)
              (throw 'done nil))
            ;; 回环后仍无法移动
            (when (eq cur-node prev-node)
              (message "No edited files found")
              (throw 'done nil)))))))
  (save-selected-window
    (vc-diff)))

(defun cdadar/vc-dir-next-and-diff ()
  "Move to the next changed file in vc-dir and show its diff."
  (interactive)
  (cdadar/vc-dir-move-and-diff #'vc-dir-next-line (point-min)))

(defun cdadar/vc-dir-prev-and-diff ()
  "Move to the previous changed file in vc-dir and show its diff."
  (interactive)
  (cdadar/vc-dir-move-and-diff #'vc-dir-previous-line (point-max)))

(defun cdadar/vc-dir-quick-commit-all ()
  "Mark all tracked changed files in vc-dir and enter the commit interface."
  (interactive)
  (vc-dir-unmark-all-files 1)
  (dolist (state '(edited added removed))
    (vc-dir-mark-state-files state))
  (let ((files (vc-dir-marked-files)))
    (if files
        (vc-next-action nil)
      (message "No files to commit"))))

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
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

(defun cdadar/setup-vc-msg-git-integration ()
  "Configure vc-msg Git integration with Magit without duplicating menu entries."
  ;; show code of commit
  (setq vc-msg-git-show-commit-function #'magit-show-commit)
  ;; open file of certain revision
  (let ((entry (assoc "m" vc-msg-git-extra)))
    (if entry
        (setcdr entry '("[m]agit-find-file"
                        sanityinc/vc-msg-magit-find-file))
      (push '("m"
              "[m]agit-find-file"
              sanityinc/vc-msg-magit-find-file)
            vc-msg-git-extra))))

(use-package vc
  :ensure nil
  :defer t
  :custom
  (vc-handled-backends '(Git))
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep)
              ("l" . sanityinc/magit-or-vc-log-file)))

(use-package vc-dir
  :ensure nil
  :after vc
  :bind (:map vc-dir-mode-map
              ("e" . vc-ediff)
              ("d" . vc-diff)
              ("k" . vc-revert)
              ("c" . cdadar/vc-dir-quick-commit-all)
              ("<tab>" . cdadar/vc-dir-next-and-diff)
              ("<backtab>" . cdadar/vc-dir-prev-and-diff)
              ("F" . vc-pull)))

(use-package diff-hl
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (after-init . global-diff-hl-mode)
  :bind
  (:map diff-hl-mode-map
        (("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)
         ("M-C-]" . diff-hl-next-hunk)
         ("M-C-[" . diff-hl-previous--hunk))))

(use-package git-modes)

(use-package git-timemachine
  :bind
  ("C-x v t" . git-timemachine-toggle))

(use-package git-link)

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
  (sanityinc/fullframe-mode 'magit-status-mode))

(use-package magit-todos)

(use-package vc-msg
  :config
  (with-eval-after-load 'vc-msg-git
    (cdadar/setup-vc-msg-git-integration)))


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
  "Return the cached list of available git svn subcommands."
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(defun cdadar/git-svn-compilation-buffer-name (_major-mode-name)
  "Return the compilation buffer name used by `git-svn'."
  "*git-svn*")

(use-package vc-git
  :ensure nil
  :commands (vc-git-root))

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read
                      "git-svn command: "
                      (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function #'cdadar/git-svn-compilation-buffer-name))
    (compile (concat "git svn " command))))

(provide 'init-git)
;;; init-git.el ends here
