;;; init-lsp.el --- LSP support via eglot  -*- lexical-binding: t -*-

;; Copyright (C) 2025 chens
;;
;; Version: 0.0.1
;; Keywords: LSP eglot
;; Author: chens <chens>
;; URL: https://github.com/cdadar/emacs.d/lisp/init-lsp.el
;; Package-Requires: ((emacs "29.1"))

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

(require 'cl-lib)

(defun cdadar/eglot-managed-mode-p ()
  "Return non-nil when the current buffer should auto-enable Eglot."
  (not (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)))

(defun cdadar/eglot-ensure-maybe ()
  "Enable Eglot in the current buffer when appropriate."
  (when (cdadar/eglot-managed-mode-p)
    (eglot-ensure)))

(use-package eglot
  :hook ((prog-mode . cdadar/eglot-ensure-maybe)
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  :init
  (setq eglot-events-buffer-size 0))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))

;; Emacs LSP booster
(when (and (executable-find "emacs-lsp-booster")
           (locate-library "eglot-booster"))
  (use-package eglot-booster
    :ensure nil
    :after eglot
    :commands (eglot-booster-mode)
    :init
    (eglot-booster-mode 1)))

(use-package org-src
  :ensure nil
  :preface
  (defun cdadar/lsp-org-babel-buffer-file-name (info)
    "Return a suitable `buffer-file-name' from Babel INFO."
    (or (alist-get :file (caddr info))
        "org-src-babel.tmp"))

  (cl-defmacro cdadar/lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "cdadar/lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (setq buffer-file-name (cdadar/lsp-org-babel-buffer-file-name info))
           (when (fboundp 'eglot-ensure)
             (eglot-ensure)))
         (put ',intern-pre 'function-documentation
              (format "Enable eglot in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defconst cdadar/lsp-org-babel-language-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
    "The supported programming languages for interactive Babel.")
  :config
  (dolist (lang cdadar/lsp-org-babel-language-list)
    (eval `(cdadar/lsp-org-babel-enable ,lang))))

(provide 'init-lsp)
;;; init-lsp.el ends here
