(defun c-mode-common-hook-setup ()
  (progn
    ;;company-c-headers
    (when (maybe-require-package 'company-c-headers)
      (push #'company-c-headers company-backends)

      ;;  gcc -xc++ -E -v - 获取头文件路径
      (setq  company-c-headers-path-system '("." "/usr/include/"
                                             "/usr/local/include/")))

    ;; avoid default "gnu" style, use more popular one
    (setq c-default-style "linux")

    (setq c-basic-offset 4)
    ;; give me NO newline automatically after electric expressions are entered
    (setq c-auto-newline nil)

    ;;company-clang
    (push #'company-clang company-backends)))

(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)

(provide 'init-cc-mode)
