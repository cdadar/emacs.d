;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *win64* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin) )
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defconst *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(defconst *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(defconst *no-memory* (cond
                       (*is-a-mac*
                        (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                       (*linux* nil)
                       (t nil)))
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter


;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))


;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; General performance tuning
(use-package gcmh
  :diminish
  :init (setq gcmh-high-cons-threshold (* 128 1024 1024))
  :hook (after-init . gcmh-mode))

(setq jit-lock-defer-time 0)

;; (when (require-package 'gcmh)
;;   (setq gcmh-high-cons-threshold (* 512 1024 1024))
;;   (add-hook 'after-init-hook 'gcmh-mode)
;;   (with-eval-after-load 'diminish
;;     (diminish 'gcmh-mode)))
;; (setq jit-lock-defer-time 0)


;; allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package ripgrep)
(use-package scratch)
(use-package command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-compile)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-http)
(require 'init-python)
(require 'init-haskell)
(require 'init-elm)
(require 'init-ruby)
(require 'init-sql)
(require 'init-ocaml)
(require 'init-j)
(require 'init-nim)
(require 'init-rust)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(require 'init-terraform)
(require 'init-nix)
(use-package nginx-mode)
(use-package just-mode)
(use-package just-ts-mode
  ;; Undo overly-optimistic autoloading, so that things still work in
  ;; Emacs 29 without treesitter
  :config
  (sanityinc/remove-auto-mode 'just-ts-mode))
(use-package justl)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-sly)
(require 'init-clojure)
(require 'init-clojure-cider)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)

(require 'init-ledger)
(require 'init-lua)
(require 'init-uiua)
(require 'init-zig)
(require 'init-terminals)


;; user config
(require 'init-search)
;; web-mode
(require 'init-web-mode)
;; cc and gtags
(require 'init-cc-mode)
;; (require 'init-gud)
;; (require 'init-gtags)
;; (require 'init-ctags)
;; (require 'init-citre)
(require 'init-yasnippet)
(require 'init-emmet)
;; (require 'init-dict)
(require 'init-erc)
(require 'init-emojify)
(require 'init-puml)
(require 'init-golang)
(require 'init-vimrc)
(require 'init-elfeed)
(require 'init-eshell)
(require 'init-rpm)
(require 'init-shell)
(require 'init-deft)
(require 'init-latex)
(require 'init-perl)
(require 'init-anki)
(require 'init-date)
(require 'init-auto-space)
(require 'init-citar)
(require 'init-pdf)
(require 'init-epub)
(require 'init-ellama)

(require 'init-fonts)
(require 'init-personal)

;; Extra packages which don't require any configuration


(use-package sudo-edit)
(use-package gnuplot)
(use-package htmlize)
(when *is-a-mac*
  (use-package osx-location))
(use-package dotenv-mode)
(use-package shfmt)

(use-package uptimes
  :defer t
  :config
  (setq-default uptimes-keep-count 200))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))



;; Allow access from emacsclient
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))

(use-package server
  :ensure nil
  :init
  (require 'server)
  :hook
  (after-init . (lambda ()
                  (unless (server-running-p)
                    (server-start)))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)


(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
