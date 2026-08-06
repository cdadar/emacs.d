;;; init-ai.el --- AI assistants and coding agents -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Infrastructure

(use-package vterm
  :defer t
  :commands (vterm)
  :custom
  (vterm-shell (executable-find "zsh")))

(use-package mcp
  :defer t)

(use-package agent-shell
  :defer t
  :commands (agent-shell)
  :init
  (defun cdadar/agent-shell-hermes-make-agent-config ()
    (agent-shell-make-agent-config
     :identifier 'hermes
     :mode-line-name "Hermes"
     :buffer-name "Hermes"
     :shell-prompt "Hermes> "
     :shell-prompt-regexp "Hermes> "
     :client-maker
     (lambda (buffer)
       (agent-shell--make-acp-client
        :command "hermes"
        :command-params '("acp")
        :context-buffer buffer))
     :install-instructions
     "Install Hermes Agent and ensure `hermes acp --check` succeeds."))

  (defun cdadar/agent-shell-register-hermes ()
    (setq agent-shell-agent-configs
          (cons (cdadar/agent-shell-hermes-make-agent-config)
                (cl-remove-if (lambda (c) (eq (alist-get :identifier c) 'hermes))
                              agent-shell-agent-configs))))

  :config
  (cdadar/agent-shell-register-hermes)
  (setq agent-shell-preferred-agent-config 'hermes))


;;;; Coding agents

(use-package ai-code
  :defer t
  :commands (ai-code-menu)
  :bind (("C-c A" . ai-code-menu))
  :custom
  (ai-code-auto-test-type 'ask-me)
  :init
  (add-hook 'after-init #'ai-code-prompt-filepath-completion-mode)
  :config
  ;; Primary AI coding entrypoint, now routed through 'agent-shell
  ;; which uses Hermes ACP. Other supported backends include
  ;; 'codex, 'claude-code, 'gemini, 'github-copilot-cli, 'opencode,
  ;; 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'eca, 'agent-shell,
  ;; 'claude-code-ide and 'claude-code-el.
  (ai-code-set-backend 'agent-shell)
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


;;;; Pi coding agent
(use-package pi-coding-agent
  :if (executable-find "pi")
  :defer t
  :bind (("C-c C-p" . pi-coding-agent))
  :custom
  (pi-coding-agent-essential-grammar-action 'auto)
  :init
  ;; Convenience alias (as recommended by README)
  (defalias 'pi #'pi-coding-agent)

  ;; oh-my-pi is a pi package (npm: oh-my-pi) that replaces Pi's default
  ;; system prompt with a Sisyphus-style multi-agent orchestrator.  It adds
  ;; specialist agents (oracle, librarian, explore), a skill system, and the
  ;; in-session `/oh-my-pi-doctor' and `/oh-my-pi-reload' commands.
  ;; See https://github.com/acidsugarx/oh-my-pi
  ;;
  ;; Config cascade (low to high priority):
  ;;   built-in defaults  <  ~/.pi/oh-my-pi.jsonc  <  ./.oh-my-pi.jsonc
  ;;
  ;; These helpers only need the `pi' executable, so they live in :init and
  ;; are available before `pi-coding-agent' is first loaded.
  (defvar cdadar/pi-oh-my-pi-source "npm:oh-my-pi"
    "pi package source spec for oh-my-pi, as understood by `pi install'.")

  (defvar cdadar/pi-oh-my-pi-global-config
    (expand-file-name "~/.pi/oh-my-pi.jsonc")
    "User-global oh-my-pi config (JSONC).")

  (defvar cdadar/pi-oh-my-pi-project-config ".oh-my-pi.jsonc"
    "Project-local oh-my-pi config filename, relative to project root.")

  (defun cdadar/pi-oh-my-pi-installed-p ()
    "Return non-nil if the oh-my-pi package is installed for the Pi CLI."
    (with-temp-buffer
      (call-process "pi" nil t nil "list")
      (goto-char (point-min))
      (re-search-forward (regexp-quote "oh-my-pi") nil t)))

  (defun cdadar/pi-oh-my-pi--run-to-buffer (command &rest args)
    "Run `pi COMMAND ARGS...' streaming output to a `*oh-my-pi*' buffer.
Display the buffer and return the `call-process' exit status."
    (with-current-buffer (get-buffer-create "*oh-my-pi*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (display-buffer (current-buffer))
      (apply #'call-process "pi" nil t nil command args)))

  (defun cdadar/pi-install-oh-my-pi (&optional arg)
    "Install the oh-my-pi orchestration package for the Pi CLI.
With prefix ARG, force a reinstall (uninstall first).  Output is shown
in a `*oh-my-pi*' buffer.  Idempotent: does nothing if already installed."
    (interactive "P")
    (when (and arg (cdadar/pi-oh-my-pi-installed-p))
      (cdadar/pi-uninstall-oh-my-pi))
    (if (cdadar/pi-oh-my-pi-installed-p)
        (message "oh-my-pi is already installed")
      (message "Installing oh-my-pi (`pi install %s')..." cdadar/pi-oh-my-pi-source)
      (let ((exit (cdadar/pi-oh-my-pi--run-to-buffer
                   "install" cdadar/pi-oh-my-pi-source)))
        (if (eq exit 0)
            (message "oh-my-pi installed - restart Pi to activate")
          (message "oh-my-pi install failed (exit %s)" exit)))))

  (defun cdadar/pi-uninstall-oh-my-pi ()
    "Remove the oh-my-pi package from the Pi CLI."
    (interactive)
    (if (not (cdadar/pi-oh-my-pi-installed-p))
        (message "oh-my-pi is not installed")
      (message "Removing oh-my-pi (`pi remove %s')..." cdadar/pi-oh-my-pi-source)
      (let ((exit (cdadar/pi-oh-my-pi--run-to-buffer
                   "remove" cdadar/pi-oh-my-pi-source)))
        (if (eq exit 0)
            (message "oh-my-pi removed")
          (message "oh-my-pi remove failed (exit %s)" exit)))))

  (defun cdadar/pi-oh-my-pi--maybe-stub (file)
    "Insert a minimal oh-my-pi config stub into the current buffer if FILE is new."
    (when (and (not (file-exists-p file)) (zerop (buffer-size)))
      (insert
       (concat
        "{\n"
        "  // oh-my-pi orchestration config (JSONC).\n"
        "  // Cascade: defaults < ~/.pi/oh-my-pi.jsonc < ./.oh-my-pi.jsonc\n"
        "  // Docs: https://github.com/acidsugarx/oh-my-pi\n"
        "  \"orchestrator\": {\n"
        "    \"agentName\": \"oh-my-pi\",\n"
        "    \"promptTemplate\": \"sisyphus\"\n"
        "  }\n"
        "}\n"))))

  (defun cdadar/pi-oh-my-pi-edit-global-config ()
    "Open the user-global oh-my-pi config for editing.
Creates a minimal stub if the file does not yet exist."
    (interactive)
    (find-file cdadar/pi-oh-my-pi-global-config)
    (cdadar/pi-oh-my-pi--maybe-stub cdadar/pi-oh-my-pi-global-config))

  (defun cdadar/pi-oh-my-pi-edit-project-config ()
    "Open the project-local .oh-my-pi.jsonc for editing.
Uses the current project root, or `default-directory' outside a project.
Creates a minimal stub if the file does not yet exist."
    (interactive)
    (let* ((root (or (when (fboundp 'project-current)
                       (let ((p (project-current nil)))
                         (and p (fboundp 'project-root)
                              (project-root p))))
                     default-directory))
           (file (expand-file-name cdadar/pi-oh-my-pi-project-config root)))
      (find-file file)
      (cdadar/pi-oh-my-pi--maybe-stub file))))


(provide 'init-ai)
;;; init-ai.el ends here
