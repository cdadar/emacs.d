;;; init-lsp.el --- LSP support via eglot  -*- lexical-binding: t -*-

;; Copyright (C) 2025 chens
;;
;; Version: 0.0.1
;; Keywords: LSP eglot
;; Author: chens <chens>
;; URL: https://github.com/cdadar/emacs.d/lisp/init-lsp.el
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;;; Code:

(use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
     :init
     (setq read-process-output-max (* 1024 1024)) ; 1MB
     (setq eglot-autoshutdown t
           eglot-events-buffer-size 0
           eglot-send-changes-idle-time 0.5)
     :config
     (use-package consult-eglot
       :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

     ;; Emacs LSP booster
     (when (executable-find "emacs-lsp-booster")
       (unless (package-installed-p 'eglot-booster)
         (and (fboundp #'package-vc-install)
              (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
       (use-package eglot-booster
         :ensure nil
         :autoload eglot-booster-mode
         :init (eglot-booster-mode 1))))



(cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                      "org-src-babel.tmp"))
           (when (fboundp 'eglot-ensure)
                (eglot-ensure)))
         (put ',intern-pre 'function-documentation
              (format "Enable elgot in the buffer of org source block (%s)."
                      (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

(defconst org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
    "The supported programming languages for interactive Babel.")

(dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-lsp)
;;; init-lsp.el ends here
