;;; init-ai.el --- about ai config  -*- lexical-binding: t -*-

;; Copyright (C) 2025 chens
;;
;; Version: 0.0.1
;; Keywords: ai emigo aidermacs
;; Author: chens <chens AT linux-asdf>
;; URL: https://github.com/cdadar/emacs.d/lisp/init-ai.el
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
(message "hello world")

(use-package emigo
  :vc (:url "https://github.com/MatthewZMD/emigo"  :rev :newest))



(when (executable-find "ollama")
  (use-package ellama
    :bind ("C-c e" . ellama-transient-main-menu)
    :init
    ;; setup key bindings
    ;; (setopt ellama-keymap-prefix "C-c e")
    ;; language you want ellama to translate to
    (setopt ellama-language "Chinese")
    ;; could be llm-openai for example
    (require 'llm-ollama)
    ;; (setopt ellama-provider
    ;;         (make-llm-ollama
    ;;          ;; this model should be pulled to use it
    ;;          ;; value should be the same as you print in terminal during pull
    ;;          :chat-model "llama3:8b-instruct-q8_0"
    ;;          :embedding-model "nomic-embed-text"
    ;;          :default-chat-non-standard-params '(("num_ctx" . 8192))))
    ;; (setopt ellama-summarization-provider
    ;;         (make-llm-ollama
    ;;          :chat-model "qwen2.5:3b"
    ;;          :embedding-model "nomic-embed-text"
    ;;          :default-chat-non-standard-params '(("num_ctx" . 32768))))
    ;; (setopt ellama-coding-provider
    ;;         (make-llm-ollama
    ;;          :chat-model "qwen2.5-coder:3b"
    ;;          :embedding-model "nomic-embed-text"
    ;;          :default-chat-non-standard-params '(("num_ctx" . 32768))))
    ;; Predefined llm providers for interactive switching.
    ;; You shouldn't add ollama providers here - it can be selected interactively
    ;; without it. It is just example.
    ;; (setopt ellama-providers
    ;;         '(("zephyr" . (make-llm-ollama
    ;;                        :chat-model "zephyr:7b-beta-q6_K"
    ;;                        :embedding-model "zephyr:7b-beta-q6_K"))
    ;;           ("mistral" . (make-llm-ollama
    ;;                         :chat-model "mistral:7b-instruct-v0.2-q6_K"
    ;;                         :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
    ;;           ("mixtral" . (make-llm-ollama
    ;;                         :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
    ;;                         :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
    ;; Naming new sessions with llm
    ;; (setopt ellama-naming-provider
    ;;         (make-llm-ollama
    ;;          :chat-model "llama3:8b-instruct-q8_0"
    ;;          :embedding-model "nomic-embed-text"
    ;;          :default-chat-non-standard-params '(("stop" . ("\n")))))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
    ;; Translation llm provider
    ;; (setopt ellama-translation-provider
    ;;         (make-llm-ollama
    ;;          :chat-model "qwen2.5:3b"
    ;;          :embedding-model "nomic-embed-text"
    ;;          :default-chat-non-standard-params
    ;;          '(("num_ctx" . 32768))))
    ;; (setopt ellama-extraction-provider (make-llm-ollama
    ;;                                     :chat-model "qwen2.5-coder:7b-instruct-q8_0"
    ;;                                     :embedding-model "nomic-embed-text"
    ;;                                     :default-chat-non-standard-params
    ;;                                     '(("num_ctx" . 32768))))
    ;; customize display buffer behaviour
    ;; see ~(info "(elisp) Buffer Display Action Functions")~
    (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
    (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
    :config
    ;; send last message in chat buffer with C-c C-c
    (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)))


(when (executable-find "aider")
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
                                        ; (setenv "ANTHROPIC_API_KEY" "anthropic-api-key")
    ))
(provide 'init-ai)
;;; init-ai.el ends here
