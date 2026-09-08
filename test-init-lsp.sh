#!/bin/sh -e
# Focused batch tests for lisp/init-lsp.el (Eglot integration).
# Pure logic tests; no external LSP server is required, so this runs on CI
# across Emacs 29.x-30.x. Run: ./test-init-lsp.sh
cd "$(dirname "$0")"

cat > /tmp/init-lsp-test.el <<'EOF'
;; -*- lexical-binding: t -*-
(require 'use-package)
(load (expand-file-name "lisp/init-lsp.el" default-directory) nil t)
;; The eglot block is deferred via :hook; in a real session the first
;; prog-mode buffer triggers (require 'eglot).  Mirror that here so
;; `eglot-server-programs' and `eglot--guess-contact' are available.
(require 'eglot)

(defun assert-eq (label expected actual)
  (unless (equal expected actual)
    (error "FAIL %s: expected %S, got %S" label expected actual))
  (princ (format "ok %s\n" label)))

;; 1. CONTACT parsing: local command -> program, TCP -> nil.
(assert-eq "contact-program local" "clangd"
           (cdadar/eglot-contact-program '("clangd" "--background-index")))
(assert-eq "contact-program tcp" nil
           (cdadar/eglot-contact-program '("127.0.0.1" 2087)))

;; 2. Missing-candidates extraction from an Eglot-style error (real format:
;;    a single quoted pair joined with ", ").
(let ((err (condition-case e (error "None of 'clangd, clangd-18' are valid executables")
             (error e))))
  (assert-eq "missing-candidates" '("clangd" "clangd-18")
             (cdadar/eglot-missing-candidates-from-error err)))

;; 3. Server present on PATH -> no missing server (recommended-missing-server nil).
(let ((dir (make-temp-file "eglot-fake-" t)))
  (with-temp-file (expand-file-name "fake-server" dir)
    (insert "#!/bin/sh\nexit 0\n"))
  (set-file-modes (expand-file-name "fake-server" dir) #o755)
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (push dir exec-path)
  (define-derived-mode cdadar/fake-present-mode prog-mode "FakeP")
  (add-to-list 'eglot-server-programs
               '(cdadar/fake-present-mode . ("fake-server")))
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "p.zzz" dir))
    (cdadar/fake-present-mode)
    (assert-eq "server-present" nil (cdadar/eglot-recommended-missing-server))
    ;; ensure-maybe must not throw and should not start an install.
    (assert-eq "ensure-present-no-error" t (progn (cdadar/eglot-ensure-maybe) t))))

;; 4. Server missing and installer configured -> its name is reported.
(define-derived-mode cdadar/fake-missing-mode prog-mode "FakeM")
(add-to-list 'eglot-server-programs
             '(cdadar/fake-missing-mode . ("no-such-server-xyz")))
(add-to-list 'cdadar/eglot-server-installers '("no-such-server-xyz" . "echo skip"))
(with-temp-buffer
  (setq buffer-file-name "/tmp/eglot-test/m.zzz")
  (cdadar/fake-missing-mode)
  (assert-eq "server-missing-installable" "no-such-server-xyz"
             (cdadar/eglot-recommended-missing-server))
  ;; noninteractive: install-server-maybe must not spawn anything.
  (assert-eq "no-spawn-noninteractive" t
             (progn (cdadar/eglot-install-server-maybe "no-such-server-xyz") t)))

;; 5. Server missing but not in installers -> nil (no recommendation).
(define-derived-mode cdadar/fake-unknown-mode prog-mode "FakeU")
(add-to-list 'eglot-server-programs
             '(cdadar/fake-unknown-mode . ("unknown-server-abc")))
(with-temp-buffer
  (setq buffer-file-name "/tmp/eglot-test/u.zzz")
  (cdadar/fake-unknown-mode)
  (assert-eq "server-missing-no-installer" nil
             (cdadar/eglot-recommended-missing-server)))

;; 6. Excluded prog modes are not auto-managed.
(with-temp-buffer
  (setq buffer-file-name "/tmp/eglot-test/elisp.el")
  (emacs-lisp-mode)
  (assert-eq "excluded-mode" nil (cdadar/eglot-managed-mode-p)))

(princ "ALL TESTS PASSED\n")
EOF

${EMACS:=emacs} --batch -Q -l /tmp/init-lsp-test.el
rm -f /tmp/init-lsp-test.el
