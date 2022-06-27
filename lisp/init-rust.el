;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :hook (rust-mode . cdadar/rust-compile)
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "RET") 'av/auto-indent-method-maybe)
  (defun cdadar/rust-compile ()
    (setq-local compile-command "cargo check --color never --tests")))

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (defun cdadar/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  :bind (:map rust-mode-map
         (("C-c C-t" . cdadar/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)
           (cargo-process--command-flags "--  --nocapture")))

(provide 'init-rust)
;;; init-rust.el ends here
