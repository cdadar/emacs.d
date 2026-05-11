;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:
(use-package ibuffer-vc
  :preface
  (defun ibuffer-set-up-preferred-filters ()
    "Group ibuffer entries by VC root and keep filename/process sorting."
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  :hook
  (ibuffer . ibuffer-set-up-preferred-filters))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-show-empty-filter-groups nil)
  :custom
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :config
  (sanityinc/fullframe-mode 'ibuffer-mode)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

;; Modify the default ibuffer-formats (toggle with `)
;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;               (name 22 22 :left :elide)
;;               " "
;;               (size-h 9 -1 :right)
;;               " "
;;               (mode 12 12 :left :elide)
;;               " "
;;               vc-relative-file)
;;         (mark modified read-only vc-status-mini " "
;;               (name 22 22 :left :elide)
;;               " "
;;               (size-h 9 -1 :right)
;;               " "
;;               (mode 14 14 :left :elide)
;;               " "
;;               (vc-status 12 12 :left)
;;               " "
;;               vc-relative-file)))

(use-package tempbuf
  :ensure nil
  :load-path "site-lisp/tempbuf"
  :hook
  ((custom-mode w3-mode Man-mode view-mode) . turn-on-tempbuf-mode))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
