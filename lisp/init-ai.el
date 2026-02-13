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
    bb    ;;          :default-chat-non-standard-params '(("stop" . ("\n")))))
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
  (use-package aider
    :config
    ;; For latest claude sonnet model
    (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect")) ;; add --no-auto-commits if you don't want it
    (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
    ;; Or chatgpt model
    ;; (setq aider-args '("--model" "o4-mini"))
    ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
    ;; Or use your personal config file
    ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
    ;; ;;
    ;; Optional: Set a key binding for the transient menu
    (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
    ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
    (aider-magit-setup-transients) ;; add aider magit function to magit menu
    ;; auto revert buffer
    (global-auto-revert-mode 1)
    (auto-revert-mode 1))


  (use-package ai-code
    :config
    ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code, 'cursor
    (ai-code-set-backend 'codex)
    ;; Enable global keybinding for the main menu
    (global-set-key (kbd "C-c a") #'ai-code-menu)
    ;; Optional: Use eat if you prefer, by default it is vterm
    ;; (setq ai-code-backends-infra-terminal-backend 'eat) ;; for openai codex, github copilot cli, opencode, grok, cursor-cli; for claude-code-ide.el, you can check their config
    ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
    (global-auto-revert-mode 1)
    (setq auto-revert-interval 1) ;; set to 1 second for faster update
    ;; Optional: Set up Magit integration for AI commands in Magit popups
    (with-eval-after-load 'magit
      (ai-code-magit-setup-transients))))


(use-package eca)


(provide 'init-ai)

;;; init-ai.el ends here
