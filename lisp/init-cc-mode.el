;;; init-cc-mode.el --- cc-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-



(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook
   hook
   '(lambda ()
      (require 'cc-mode)
      (require-package 'c-eldoc)
      (require-package 'modern-cpp-font-lock)

      (defun c-mode-style-setup ()
        (interactive)
        "Set up c-mode and related modes.
Includes support for Qt code (signal, slots and alikes)."
        ;; eldoc.
        (c-turn-on-eldoc-mode)

        ;; cpp font lock.
        (modern-c++-font-lock-global-mode t)

        ;; base-style
        (c-set-style "stroustrup")


        (when (maybe-require-package 'company-c-headers)
          (push #'company-c-headers company-backends)

          ;;  gcc -xc++ -E -v - 获取头文件路径
          (setq  company-c-headers-path-system '("." "/usr/include/"
                                                 "/usr/local/include/")))
        ;;company-clang
        (push #'company-clang company-backends)

        ;; qt keywords and stuff ...
        ;; set up indenting correctly for new qt kewords
        (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                       "\\|protected slot\\|private\\|private slot"
                                       "\\)\\>")
              c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                       "\\|public slots\\|protected slots\\|private slots"
                                       "\\)\\>[ \t]*:"))
        (progn
          ;; modify the colour of slots to match public, private, etc ...
          (font-lock-add-keywords 'c++-mode
                                  '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
          ;; make new font for rest of qt keywords
          (make-face 'qt-keywords-face)
          (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
          ;; qt keywords
          (font-lock-add-keywords 'c++-mode
                                  '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
          (font-lock-add-keywords 'c++-mode
                                  '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
          (font-lock-add-keywords 'c++-mode
                                  '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
          ))

      (c-mode-style-setup))))

(provide 'init-cc-mode)
