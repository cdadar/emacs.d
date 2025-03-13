;;; init-aider.el --- MatthewZMD/aidermacs config  -*- lexical-binding: t -*-

;; Copyright (C) 2025 chens
;;
;; Version: 0.0.1
;; Keywords: ellama ai
;; Author: chens <gunbanmi@gmail.com>
;; URL: https://github.com/cdadar/emacd.d/lisp/init-ellama
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

(use-package aidermacs
  :config
  (setq aidermacs-default-model "sonnet")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  ; See the Configuration section below
  (setq aidermacs-auto-commits t)
  (setq aidermacs-use-architect-mode t)
  ; Ensure emacs can access *_API_KEY through .bashrc or setenv
  (setenv "ANTHROPIC_API_KEY" "anthropic-api-key"))


(provide 'init-aider)
;;; init-ellama.el ends here
