;;; init-common-lisp.el --- Common Lisp support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun cdadar/ensure-slime-for-lisp-mode ()
  "Load SLIME and re-run mode setup for Common Lisp buffers when available."
  (unless (featurep 'slime)
    (when (require 'slime nil t)
      (normal-mode))))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Search lispdoc.com for the symbol at point, or prompt for one."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(defun cdadar/add-slime-implementation-if-executable (name command coding-system)
  "Add SLIME implementation NAME using COMMAND when its executable exists.
CODING-SYSTEM is passed through to `slime-lisp-implementations'."
  (when (executable-find (car command))
    (add-to-list 'slime-lisp-implementations
                 `(,name ,command :coding-system ,coding-system))))

;; See http://bc.tech.coop/blog/070927.html
(use-package lisp-mode
  :ensure nil
  :mode ("\\.cl\\'" . lisp-mode)
  :hook (lisp-mode . cdadar/ensure-slime-for-lisp-mode)
  :bind (:map lisp-mode-map
              ("C-c l" . lispdoc)))

(use-package slime
  :ensure nil
  :defer t
  :if (locate-library "slime")
  :config
  (cdadar/add-slime-implementation-if-executable
   'cmucl '("lisp") 'iso-latin-1-unix)
  (cdadar/add-slime-implementation-if-executable
   'ccl '("ccl") 'utf-8-unix)
  (cdadar/add-slime-implementation-if-executable
   'sbcl '("sbcl") 'utf-8-unix)
  (cdadar/add-slime-implementation-if-executable
   'roswell '("ros" "-Q" "run") 'utf-8-unix))

(use-package common-lisp-snippets
  :defer t
  :after lisp-mode)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here
