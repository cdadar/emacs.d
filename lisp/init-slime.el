;;; init-slime.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'slime)
(push (expand-file-name "contrib" (file-name-directory (locate-library "slime"))) load-path)



;;; Lisp buffers

(with-eval-after-load 'slime
  (require 'slime-autoloads)
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((features '(slime-fancy slime-repl slime-fuzzy)))
    (slime-setup features)) )


;;; REPL

(defun sanityinc/slime-repl-setup ()
  "Mode setup function for slime REPL."
  (sanityinc/lisp-setup))

(with-eval-after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))



(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH="
                                  (mapconcat 'identity exec-path ":"))
                          (concat "QUICKLISP_HOME="
                                  (file-name-as-directory directory) "quicklisp/"))))

(provide 'init-slime)
;;; init-slime.el ends here
