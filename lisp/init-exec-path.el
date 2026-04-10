;;; init-exec-path.el --- Import shell environment into Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep Emacs PATH/environment consistent with the user's login shell,
;; especially when Emacs is launched from GUI on macOS.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x pgtk))
          (daemonp))
  :init
  ;; 明确使用 zsh
  (setq exec-path-from-shell-shell-name "/bin/zsh")

  ;; 使用 login shell，读取 ~/.zprofile / ~/.zshenv
  ;; 不建议默认用 -i，避免 .zshrc 输出内容污染结果
  (setq exec-path-from-shell-arguments '("-l"))

  :config
  ;; 需要同步到 Emacs 的环境变量
  (dolist (var '("PATH"
                 "MANPATH"
                 "SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))

  ;; 初始化环境
  (exec-path-from-shell-initialize)

  ;; 可选：单独再确保 PATH 同步一次
  ;; 某些环境下这样更稳
  (exec-path-from-shell-copy-env "PATH"))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
