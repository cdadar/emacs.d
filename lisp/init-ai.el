;;; init-ai.el --- AI assistants and coding agents -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Infrastructure

(use-package vterm
  :defer t
  :commands (vterm)
  :custom
  (vterm-shell "/bin/zsh"))

(use-package mcp
  :defer t)


;;;; Coding agents

(use-package ai-code
  :defer t
  :commands (ai-code-menu)
  :bind (("C-c A" . ai-code-menu))
  :custom
  (ai-code-auto-test-type 'ask-me)
  :hook (after-init . ai-code-prompt-filepath-completion-mode)
  :config
  ;; Primary AI coding entrypoint. Other supported backends include
  ;; 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok,
  ;; 'cursor, 'kiro, 'codebuddy, 'aider, 'eca, 'agent-shell,
  ;; 'claude-code-ide and 'claude-code-el.
  (ai-code-set-backend 'codex)
  (with-eval-after-load 'evil
    (ai-code-backends-infra-evil-setup))
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(use-package aider
  :commands (aider aider-transient-menu)
  :bind (("C-c C-a" . aider-transient-menu))
  :custom
  (aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  :config
  (with-eval-after-load 'magit
    (aider-magit-setup-transients)))


;;;; Chat assistants

(use-package ellama
  :if (executable-find "ollama")
  :ensure nil
  :defer t
  :bind (("C-c e" . ellama-transient-main-menu))
  :init
  (setopt ellama-language "Chinese")
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  (require 'llm-ollama)
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

(use-package gptel
  :defer t
  :commands (gptel gptel-send)
  :custom
  (gptel-model "minimax/minimax-m2.5")
  (gptel-stream t)
  (gptel-system-message "You are a helpful assistant.")
  :config
  ;; `gptel-backend' is a defcustom, but its value is a runtime object
  ;; constructed by `gptel-make-openai'.  Keep it in :config so the
  ;; constructor runs after gptel (and its autoloads) are loaded, and so
  ;; the helper `cdadar/get-openrouter-api-key' is already defined.
  (setq gptel-backend
        (gptel-make-openai
            "OpenRouter"
          :host "openrouter.ai"
          :protocol "https"
          :endpoint "/api/v1/chat/completions"
          :key #'cdadar/get-openrouter-api-key
          :models '("minimax/minimax-m2.5"
                    "anthropic/claude-4.5-sonnet"
                    "openai/gpt-4o"
                    "google/gemini-2.0-flash")))
  (defun cdadar/get-openrouter-api-key ()
    (or (getenv "OPENROUTER_API_KEY")
        (auth-source-pick-first-password :host "openrouter.ai" :user "api")
        (user-error
         (concat
          "OpenRouter API key not found. Set OPENROUTER_API_KEY "
          "or add machine openrouter.ai login api password <key> to ~/.authinfo")))))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))


(provide 'init-ai)
;;; init-ai.el ends here
