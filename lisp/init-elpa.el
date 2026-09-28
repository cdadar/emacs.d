;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl-lib)



(use-package emacs
  :ensure nil
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-unsigned-archives "melpa")
  :custom
  (package-install-upgrade-built-in t)
  (package-native-compile t))


;;
;; ELPA mirrors
;;
;; MELPA is frequently unreachable from mainland China.  `melpa' keeps the
;; upstream URLs; the rest are mirrors (https://elpa.emacs-china.org/).
;; Pick one with `M-x cdadar/test-package-archives' (measures and saves) or
;; `M-x cdadar/set-package-archives'.

(defcustom cdadar-package-archives-alist
  `((melpa   . (("gnu"    . "https://elpa.gnu.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                ("melpa"  . "https://melpa.org/packages/")))
    (tencent . (("gnu"    . "https://mirrors.cloud.tencent.com/elpa/gnu/")
                ("nongnu" . "https://mirrors.cloud.tencent.com/elpa/nongnu/")
                ("melpa"  . "https://mirrors.cloud.tencent.com/elpa/melpa/")))
    (tuna    . (("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (ustc    . (("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (bfsu    . (("gnu"    . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                ("nongnu" . "https://mirrors.bfsu.edu.cn/elpa/nongnu/")
                ("melpa"  . "https://mirrors.bfsu.edu.cn/elpa/melpa/"))))
  "Package archive sets to choose from."
  :group 'package
  :type '(alist :key-type symbol :value-type alist))

(defcustom cdadar-package-archives 'melpa
  "Package archive set in use, a key of `cdadar-package-archives-alist'.
Use `cdadar/set-package-archives' (or Customize) to change it."
  :group 'package
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value cdadar-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type '(radio
          ,@(mapcar (lambda (item) (list 'const (car item)))
                    cdadar-package-archives-alist)))

;; `defcustom' only runs `:set' through Customize, so apply the default here.
;; A value saved by `cdadar/set-package-archives' in custom.el wins later.
(setq package-archives (alist-get cdadar-package-archives cdadar-package-archives-alist))

(defun cdadar/set-package-archives (archives &optional refresh)
  "Use the package archive set ARCHIVES and save the choice to `custom-file'.
ARCHIVES is one of `cdadar-package-archives-alist'.  With REFRESH
\(interactively, a prefix argument) also refresh the package contents."
  (interactive
   (list (intern (completing-read "Select package archives: "
                                  (mapcar #'car cdadar-package-archives-alist)))
         current-prefix-arg))
  (customize-save-variable 'cdadar-package-archives archives)
  (when refresh (package-refresh-contents))
  (message "Set package archives to `%s'" archives))

(defun cdadar/test-package-archives (&optional no-save)
  "Fetch `archive-contents' from every mirror in
`cdadar-package-archives-alist' and use the fastest one.
Return the fastest archive name.  With NO-SAVE, only report it."
  (interactive)
  (let* ((results
          (mapcar
           (lambda (pair)
             (let ((url (concat (cdr (assoc "melpa" (cdr pair))) "archive-contents"))
                   (start (float-time)))
               (message "Fetching %s..." url)
               (ignore-errors (url-copy-file url null-device t))
               (cons (car pair) (- (float-time) start))))
           cdadar-package-archives-alist))
         (fastest (caar (sort results (lambda (a b) (< (cdr a) (cdr b)))))))
    (message "`%s' is the fastest package archive" fastest)
    (unless no-save
      (cdadar/set-package-archives fastest))
    fastest))

;;; Fire up package.el

(use-package package
  :ensure nil
  :custom
  (package-user-dir
   (locate-user-emacs-file (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
  (package-enable-at-startup nil)
  :hook (package-menu-mode . sanityinc/maybe-widen-package-menu-columns)
  :config
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;; Setup `use-package'
;; Should set before declaring packages.
(eval-and-compile
  (require 'use-package)
  (use-package use-package-core
    :ensure nil
    :custom
    (use-package-always-ensure t)
    (use-package-always-defer t)
    (use-package-expand-minimally t)
    (use-package-enable-imenu-support t)))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort (copy-sequence known)
                              (lambda (a b)
                                (version-list-<= (package-desc-version b)
                                                 (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))



;; Update packages
(use-package auto-package-update
  :if (not (fboundp 'package-upgrade-all))
  :hook (after-init . auto-package-update-maybe)
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

;; Required by `use-package'
(use-package bind-key)

(use-package elpa-mirror
  :vc (:url "https://github.com/redguardtoo/elpa-mirror" :rev :newest)
  :commands (elpamr-create-mirror-for-installed))




;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)




;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.

(defvar sanityinc/required-packages nil)

(defun sanityinc/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`sanityinc/required-packages'.  This function is used as an
advice for `require-package', to which ARGS are passed."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'sanityinc/required-packages package)))))

(unless (advice-member-p #'sanityinc/note-selected-package 'require-package)
  (advice-add 'require-package :around #'sanityinc/note-selected-package))



;; Work around an issue in Emacs 29 where seq gets implicitly
;; reinstalled via the rg -> transient dependency chain, but fails to
;; reload cleanly due to not finding seq-25.el, breaking first-time
;; start-up
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67025
(when (string= "29.1" emacs-version)
  (defun sanityinc/reload-previously-loaded-with-load-path-updated (orig pkg-desc)
    (let ((load-path (cons (package-desc-dir pkg-desc) load-path)))
      (funcall orig pkg-desc)))

  (unless (advice-member-p #'sanityinc/reload-previously-loaded-with-load-path-updated
                           'package--reload-previously-loaded)
    (advice-add 'package--reload-previously-loaded :around
                #'sanityinc/reload-previously-loaded-with-load-path-updated)))




(defun sanityinc/save-selected-packages ()
  "Persist packages installed via `require-package' to Custom."
  (package--save-selected-packages
   (seq-uniq (append sanityinc/required-packages package-selected-packages))))

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook #'sanityinc/save-selected-packages))



(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (setq package-version-column-width 20)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (setq package-archive-column-width longest-archive-name))))


(provide 'init-elpa)
;;; init-elpa.el ends here
