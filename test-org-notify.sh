#!/bin/sh -e
# Focused tests for lisp/init-org-extras.el: cdadar/notify, the cross-platform
# notification helper behind the org-pomodoro hooks.
#
# The old implementation called `notify-send' unconditionally, which does not
# exist on macOS, so every pomodoro hook raised (file-missing ...) there. These
# tests pin down which OS primitive each platform gets, that the AppleScript
# string is escaped and quoted correctly, and that a broken backend never
# propagates an error into the hook. Platform branches are stubbed, so the file
# runs on any OS; only the darwin case invokes a real system program.
#
# Run: ./test-org-notify.sh
cd "$(dirname "$0")"

cat > /tmp/org-notify-test.el <<'EOF'
;;; notify-test.el --- cdadar/notify 的分支与转义测试  -*- lexical-binding: t -*-
;; 每条断言都能独立失败：分支用 stub 断言「调了谁、传了什么」，
;; 最后一条在 macOS 上真调一次 osascript 做端到端。

(let ((user-emacs-directory
       (file-name-as-directory (or (getenv "REPO") default-directory))))
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defvar nt-failures 0)

(defun nt-assert (label expected actual)
  (if (equal expected actual)
      (princ (format "ok   %s\n" label))
    (setq nt-failures (1+ nt-failures))
    (princ (format "FAIL %s\n     期望 %S\n     实得 %S\n" label expected actual))))

(defvar nt-cap nil)
(defun nt-capture-call-process (fn)
  "Run FN with `call-process' stubbed; return the captured (PROGRAM . ARGS)."
  (let ((nt-cap nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (setq nt-cap (cons program args))
                 0)))
      (funcall fn))
    nt-cap))

(defvar nt-log nil)
(defun nt-collect-messages (fn)
  "Run FN, return the list of complete strings it passed to `message'."
  (let ((nt-log nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) nt-log))))
      (funcall fn))
    (nreverse nt-log)))

;; --- 1. AppleScript 字符串转义 -------------------------------------------
(nt-assert "applescript: 引号与反斜杠都转义"
           "He said \\\"hi\\\" \\\\ done"
           (cdadar/notify-applescript-string "He said \"hi\" \\ done"))
(nt-assert "applescript: nil 变空串"
           "" (cdadar/notify-applescript-string nil))

;; --- 2. macOS：调 osascript，语句合法 ------------------------------------
(let ((cap (nt-capture-call-process (lambda () (cdadar/notify "标题" "正文")))))
  (nt-assert "darwin: 程序是 osascript" "osascript" (car cap))
  (nt-assert "darwin: 实参（call-process 的 nil 0 nil 之后）"
             (list nil 0 nil "-e" "display notification 正文 with title 标题")
             (cdr cap)))

;; --- 3. Windows ----------------------------------------------------------
(fset 'w32-notification-notify (lambda (&rest args) (setq nt-win args)))
(cdadar/notify "标题" "正文")
(nt-assert "windows: 用 :title/:body" '(:title "标题" :body "正文") nt-win)
(fmakunbound 'w32-notification-notify)

;; --- 4. Haiku ------------------------------------------------------------
(fset 'haiku-notifications-notify (lambda (&rest args) (setq nt-haiku args)))
(cdadar/notify "标题" "正文")
(nt-assert "haiku: 用 :title/:body" '(:title "标题" :body "正文") nt-haiku)
(fmakunbound 'haiku-notifications-notify)

;; --- 5. Linux 桌面（D-Bus）；system-type 必须是 linux，否则先命中 darwin ---
(cl-letf (((symbol-function 'notifications-notify)
           (lambda (&rest args) (setq nt-dbus args))))
  (let ((system-type 'gnu/linux))
    (cdadar/notify "标题" "正文")))
(nt-assert "linux/dbus: 用 :title/:body" '(:title "标题" :body "正文") nt-dbus)

;; --- 6. 只剩 notify-send 时走外部命令 -------------------------------------
(fmakunbound 'notifications-notify)
(let ((cap (nt-capture-call-process
            (lambda ()
              (cl-letf (((symbol-function 'executable-find)
                         (lambda (cmd) (and (string= cmd "notify-send")
                                            "/usr/bin/notify-send"))))
                (let ((system-type 'gnu/linux))
                  (cdadar/notify "标题" "正文")))))))
  (nt-assert "notify-send: 程序与实参"
             (list "notify-send" nil 0 nil "标题" "正文") cap))

;; --- 7. 一个后端都没有：只 message，不报错 --------------------------------
(let ((cap nil) (logs nil))
  (setq cap (nt-capture-call-process
             (lambda ()
               (setq logs
                     (nt-collect-messages
                      (lambda ()
                        (cl-letf (((symbol-function 'executable-find)
                                   (lambda (&rest _) nil)))
                          (let ((system-type 'gnu/linux))
                            (cdadar/notify "标题" "正文")))))))))
  (nt-assert "无后端: 不调任何外部命令" nil cap)
  (nt-assert "无后端: 仍 message 一次" '("标题: 正文") logs))

;; --- 8. 后端自己失败：不向调用方抛错 --------------------------------------
(let ((logs nil) (thrown nil))
  (setq logs (nt-collect-messages
              (lambda ()
                (condition-case e
                    (cl-letf (((symbol-function 'call-process)
                               (lambda (&rest _) (error "boom"))))
                      (cdadar/notify "标题" "正文"))
                  (error (setq thrown e))))))
  (nt-assert "后端失败: 不抛给调用方" nil thrown)
  (nt-assert "后端失败: 两条日志（echo area + 诊断）" 2 (length logs))
  (nt-assert "后端失败: 第二条是诊断"
             t (and (string-match-p "通知后端不可用" (or (nth 1 logs) "")) t)))

;; --- 9. 端到端：pomodoro 的 finished 钩子真跑一次 --------------------------
;; 修复前这里抛 (file-missing "Searching for program" "notify-send")
(let ((err (condition-case e
               (progn (cdadar/org-pomodoro-finished-notify) nil)
             (error e))))
  (nt-assert "pomodoro finished 钩子不抛错" nil err))

(princ (format "\n%s\n" (if (zerop nt-failures)
                            "全部通过"
                          (format "%d 条失败" nt-failures))))
(when (> nt-failures 0) (kill-emacs 1))
EOF

REPO="$PWD" emacs -nw --batch -l /tmp/org-notify-test.el
