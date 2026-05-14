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
(require 'subr-x)

(use-package eglot
  :ensure nil
  :preface
  (defconst cdadar/eglot-managed-extra-modes
    '(html-mode markdown-mode
      tex-mode latex-mode LaTeX-mode
      yaml-mode yaml-ts-mode)
    "Non-`prog-mode' major modes that should auto-enable Eglot when a server is available.")

  (defconst cdadar/eglot-excluded-prog-modes
    '(emacs-lisp-mode lisp-mode makefile-mode snippet-mode)
    "`prog-mode' derivatives that should not auto-enable Eglot.")

  (defcustom cdadar/eglot-auto-install-server t
    "When non-nil, auto-install a recommended LSP server before enabling Eglot.
The install is only attempted in interactive sessions, only for server
programs listed in `cdadar/eglot-server-installers', and only once per
missing server program per Emacs session."
    :type 'boolean
    :group 'eglot)

  (defcustom cdadar/eglot-server-installers
    '(("basedpyright-langserver" . "uv tool install basedpyright")
      ("clangd" . "brew install llvm")
      ("clojure-lsp" . "brew install clojure-lsp/brew/clojure-lsp-native")
      ("docker-langserver" . "npm install -g dockerfile-language-server-nodejs")
      ("gopls" . "go install golang.org/x/tools/gopls@latest")
      ("html-languageserver" . "npm install -g vscode-langservers-extracted")
      ("lua-language-server" . "brew install lua-language-server")
      ("marksman" . "brew install marksman")
      ("nixd" . "nix profile install nixpkgs#nixd")
      ("phpactor" . "composer global require phpactor/phpactor")
      ("purescript-language-server" . "npm install -g purescript-language-server")
      ("pylsp" . "uv tool install 'python-lsp-server[all]'")
      ("pyright-langserver" . "npm install -g pyright")
      ("ruff" . "uv tool install ruff")
      ("rust-analyzer" . "rustup component add rust-analyzer")
      ("solargraph" . "gem install solargraph")
      ("terraform-ls" . "brew install hashicorp/tap/terraform-ls")
      ("typescript-language-server" . "npm install -g typescript-language-server typescript")
      ("vim-language-server" . "npm install -g vim-language-server")
      ("vscode-css-language-server" . "npm install -g vscode-langservers-extracted")
      ("vscode-html-language-server" . "npm install -g vscode-langservers-extracted")
      ("vscode-json-language-server" . "npm install -g vscode-langservers-extracted")
      ("yaml-language-server" . "npm install -g yaml-language-server")
      ("zls" . "brew install zls"))
    "Alist mapping LSP server executable names to install commands."
    :type '(alist :key-type string :value-type string)
    :group 'eglot)

  (defvar cdadar/eglot-server-install-processes (make-hash-table :test 'equal)
    "Server install processes started by `cdadar/eglot-install-server-maybe'.")

  (defun cdadar/eglot-managed-mode-p ()
    "Return non-nil when the current buffer should auto-enable Eglot."
    (or (and (derived-mode-p 'prog-mode)
             (not (apply #'derived-mode-p cdadar/eglot-excluded-prog-modes)))
        (apply #'derived-mode-p cdadar/eglot-managed-extra-modes)))

  (defun cdadar/eglot-contact-available-p (contact)
    "Return non-nil when CONTACT names an available Eglot server."
    (cond
     ((null contact) nil)
     ;; TCP server CONTACT: (HOST PORT).
     ((and (consp contact) (stringp (car contact)) (numberp (cadr contact))) t)
     ;; Local command CONTACT: (PROGRAM ARGS...).
     ((and (consp contact) (stringp (car contact)))
      (let ((program (car contact)))
        (if (file-name-absolute-p program)
            (file-executable-p program)
          (executable-find program))))
     ;; Preserve support for uncommon Eglot CONTACT shapes.
     (t t)))

  (defun cdadar/eglot-contact-program (contact)
    "Return the local executable program from Eglot CONTACT, or nil."
    (when (and (consp contact)
               (stringp (car contact))
               ;; TCP server CONTACT: (HOST PORT), not a local executable.
               (not (numberp (cadr contact))))
      (car contact)))

  (defun cdadar/eglot-missing-candidates-from-error (err)
    "Return missing executable candidates described by Eglot error ERR."
    (let ((message (error-message-string err)))
      (when (string-match "None of \\(.*\\) are valid executables" message)
        (mapcar (lambda (candidate)
                  (string-trim candidate "[[:space:]'‘’`]+" "[[:space:]'‘’`]+"))
                (split-string (match-string 1 message) "," t "[[:space:]]+")))))

  (defun cdadar/eglot-recommended-missing-server ()
    "Return a missing LSP server executable with a configured installer."
    (when (require 'eglot nil t)
      (condition-case err
          (let ((program (cdadar/eglot-contact-program (nth 3 (eglot--guess-contact nil)))))
            (when (and program
                       (not (if (file-name-absolute-p program)
                                (file-executable-p program)
                              (executable-find program)))
                       (assoc program cdadar/eglot-server-installers))
              program))
        (error
         (cl-find-if (lambda (candidate)
                       (assoc candidate cdadar/eglot-server-installers))
                     (cdadar/eglot-missing-candidates-from-error err))))))

  (defun cdadar/eglot-install-server-maybe ()
    "Auto-install the recommended LSP server for the current buffer if possible."
    (when (and cdadar/eglot-auto-install-server
               (not noninteractive))
      (let* ((server (cdadar/eglot-recommended-missing-server))
             (command (cdr (assoc server cdadar/eglot-server-installers)))
             (installer (car (and command (split-string-and-unquote command))))
             (target-buffer (current-buffer)))
        (cond
         ((null server) nil)
         ((gethash server cdadar/eglot-server-install-processes)
          (message "Eglot: already installing %s" server))
         ((and installer (not (executable-find installer)))
          (message "Eglot: cannot install %s because %s is not in PATH"
                   server installer))
         (t
          (message "Eglot: installing %s via: %s" server command)
          (let ((process (start-process-shell-command
                          (format "eglot-install-%s" server)
                          (format "*eglot install %s*" server)
                          command)))
            (puthash server process cdadar/eglot-server-install-processes)
            (set-process-sentinel
             process
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (let ((server (replace-regexp-in-string "^eglot-install-" ""
                                                          (process-name proc))))
                   (remhash server cdadar/eglot-server-install-processes)
                   (if (zerop (process-exit-status proc))
                       (progn
                         (message "Eglot: installed %s" server)
                         (when (buffer-live-p target-buffer)
                           (with-current-buffer target-buffer
                             (when (cdadar/eglot-managed-mode-p)
                               (eglot-ensure)))))
                     (message "Eglot: failed to install %s; see %s"
                              server (buffer-name (process-buffer proc))))))))))))))

  (defun cdadar/eglot-server-available-p ()
    "Return non-nil when Eglot can infer an available server for this buffer."
    (when (require 'eglot nil t)
      (condition-case nil
          (cdadar/eglot-contact-available-p (nth 3 (eglot--guess-contact nil)))
        (error nil))))

  (defun cdadar/eglot-ensure-maybe ()
    "Enable Eglot, installing a recommended server first when needed."
    (when (cdadar/eglot-managed-mode-p)
      (if (cdadar/eglot-server-available-p)
          (eglot-ensure)
        (cdadar/eglot-install-server-maybe))))
  :commands (eglot eglot-ensure)
  :hook (prog-mode . cdadar/eglot-ensure-maybe)
  :init
  (dolist (mode cdadar/eglot-managed-extra-modes)
    (add-hook (intern (format "%s-hook" mode)) #'cdadar/eglot-ensure-maybe))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  :init
  ;; Emacs 29 uses `eglot-events-buffer-size'.  Emacs 30 replaced it with
  ;; `eglot-events-buffer-config', but still reads this legacy variable while
  ;; computing the new default, so keep it in `:init' before Eglot loads.
  (setq eglot-events-buffer-size 0)
  :config
  ;; Prefer project-independent Python LSP servers that work well with uv-managed
  ;; projects instead of virtualenvwrapper/pyvenv-style activation.
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       ("ruff" "server")
                       "pylsp"))))
  (when (boundp 'eglot-events-buffer-config)
    (setq eglot-events-buffer-config '(:size 0 :format full))))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))

;; Emacs LSP booster
(use-package eglot-booster
  :ensure nil
  :after eglot
  :if (and (executable-find "emacs-lsp-booster")
           (locate-library "eglot-booster"))
  :commands (eglot-booster-mode)
  :init
  (eglot-booster-mode 1))

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
