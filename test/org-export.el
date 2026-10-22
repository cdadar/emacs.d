;;; org-export.el --- Org LaTeX 导出等价性门的执行体 -*- lexical-binding: t -*-
;;; Commentary:
;; 由 test-org-export.sh 调用，不被 init.el require。
;; 用真实配置导出 test/org-export-cases/*.org，把结果写进 $OUT。
;; 两个版本的配置（拆分前 / 拆分后）各跑一次，快照 diff 为空即证明导出行为没变。
;;; Code:

(defun cdadar/org-export-normalize (tex)
  "抹掉每次导出都不同、但语义相同的噪声，其余一律不动。
两类噪声：`% Created <时间戳>' 与 org 每次随机生成的 `orgXXXXXXX' id
（出现在 `\\label{...}' / `\\ref{...}' 里）。"
  (setq tex (replace-regexp-in-string "^% Created .*$" "% Created <normalized>" tex))
  (replace-regexp-in-string "org[0-9a-f]\\{7\\}" "org<id>" tex))

(let ((repo (or (getenv "REPO") default-directory))
      (out (getenv "OUT")))
  (unless out (error "OUT 未设置"))
  (setq user-emacs-directory (file-name-as-directory (expand-file-name repo)))
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (make-directory out t)
  (let ((cases (sort (directory-files
                      (expand-file-name "test/org-export-cases" repo) t "\\.org\\'")
                     #'string<)))
    (dolist (case cases)
      (let ((buf (find-file-noselect case))
            (dest (expand-file-name (file-name-nondirectory case) out)))
        (with-current-buffer buf
          ;; 第 4 个参数必须是 nil：body-only 时导言区不生成，final-output
          ;; 过滤器几乎都靠 "\begin{document}" 守卫，会被静默全部跳过。
          (let ((tex (org-export-as 'latex nil nil nil)))
            (with-temp-file dest
              (insert (cdadar/org-export-normalize tex)))
            (princ (format "%-24s %5d bytes\n"
                           (file-name-nondirectory case) (length tex)))))
        (kill-buffer buf)))))
