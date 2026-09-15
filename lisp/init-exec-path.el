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

  ;; 用登录 shell（即 `zsh -l -c`）。
  ;; 不加 -i：dotfiles 已经把 PATH 与环境变量放进 ~/.zshenv（每个 zsh 进程都读）
  ;; 和 ~/.zshenv.local（机器差异：go sdk / Android SDK / PI_FFF_MODE），-l 另外
  ;; 还拿到 ~/.zprofile 的 rbenv / sdkman。加 -i 会为每次 Emacs 启动白付一遍
  ;; zinit 插件加载（实测 `zsh -l` 0.13s vs `zsh -l -i` 0.52s），并触发
  ;; atuin / mole / ssh-add 这些只该在交互终端发生的副作用。
  ;; 自检：`zsh -lc 'echo $PATH'` 与 `zsh -lic 'echo $PATH'` 应逐项一致。
  (exec-path-from-shell-arguments '("-l"))

  :config
  ;; 需要同步到 Emacs 的环境变量。
  ;; 注意：子进程靠 exec-path 找到二进制之后，还要靠这些变量跑起来 ——
  ;; volta 的 shim 要 VOLTA_HOME，go / adb / gradle 要 GOPATH / ANDROID_HOME，
  ;; pi 的 fff override 要 PI_FFF_MODE。
  (dolist (var '("PATH"
                 "MANPATH"
                 "SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "VOLTA_HOME"
                 "GOPATH"
                 "ANDROID_HOME"
                 "PI_FFF_MODE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))

  ;; 初始化环境（上面列表里的变量与 exec-path 一次性同步完成，无需再单独 copy PATH）
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
