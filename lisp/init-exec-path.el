;;; init-exec-path.el --- Import shell environment into Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep Emacs PATH/environment consistent with the user's login shell,
;; especially when Emacs is launched from GUI on macOS.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x pgtk))
          (daemonp))
  :custom
  ;; 明确使用 zsh
  (exec-path-from-shell-shell-name "/bin/zsh")

  ;; 使用交互式登录 shell (-l -i)，读取 ~/.zprofile 与 ~/.zshrc。
  ;; 必须加 -i：~/.hermes/node/bin（pi）、volta 全局包 shim、zinit
  ;; polaris 等 PATH 仅在 ~/.zshrc 中设置，非交互 -l 读不到，会
  ;; 导致 `executable-find' 找不到 pi。exec-path-from-shell 用
  ;; __RESULT\0...\0__RESULT 标记解析 printf 输出，不受 .zshrc 的
  ;; echo/输出污染，故 -i 安全。
  (exec-path-from-shell-arguments '("-l" "-i"))

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
