;;; init-site-lisp.el --- Support elisp manually installed in the site-lisp dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Set load path

(use-package emacs
  :ensure nil
  :preface
  (defun sanityinc/add-subdirs-to-load-path (parent-dir)
    "Add every non-hidden subdir of PARENT-DIR to `load-path'."
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir)
                             t directory-files-no-dot-files-regexp))
           load-path)))
  :config
  ;; Add both site-lisp and its immediate subdirs to `load-path'
  (let ((site-lisp-dir (locate-user-emacs-file "site-lisp/")))
    (add-to-list 'load-path site-lisp-dir)
    (sanityinc/add-subdirs-to-load-path site-lisp-dir)))

;;; Utilities for grabbing upstream libs

(defun site-lisp-dir-for (name)
  "Return the site-lisp directory for library NAME."
  (locate-user-emacs-file (format "site-lisp/%s" name)))

(defun site-lisp-library-el-path (name)
  "Return the source file path for site-lisp library NAME."
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  "Download site-lisp module NAME from URL and return its source path."
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  "Ensure site-lisp library NAME exists locally, downloading from URL if needed."
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "Return non-nil when library NAME is loadable from `site-lisp'."
  (let ((f (locate-library (symbol-name name))))
    (and f
         (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))

(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
