;;; init-personal.el --- Emacs Prelude: Personal settings
;;
;;; Commentary:
;; Personal settings to augment those of Prelude
;; Install Emacs through homebrew with --cocoa --srgb

;;; Code:
;;; make cursor style bar
(setq-default cursor-type 'box)



;; 如果配置好了， 下面 20 个汉字与 40 个英文字母应该等长
;; here are 20 hanzi and 40 english chars, see if they are the same width
;;
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
;; 你你你你你你你你你你你你你你你你你你你你|
;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,|
;; 。。。。。。。。。。。。。。。。。。。。|
;; 1111111111111111111111111111111111111111|
;; 東東東東東東東東東東東東東東東東東東東東|
;; ここここここここここここここここここここ|
;; ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ|
;; 까까까까까까까까까까까까까까까까까까까까|

;; (setq fonts
;;       (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
;;             ((eq system-type 'gnu/linux)  '("DejaVu Sans Mono"     "WenQuanYi Zen Hei Mono"))
;;             ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))

;; (set-face-attribute 'default nil :font
;;                     (format "%s:pixelsize=%d" (car fonts) 14))

;; (when (display-graphic-p)
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset
;;                       (font-spec :family (car (cdr fonts))))))


;; (when (eq system-type 'darwin)
;;   (setq fonts '("Monaco" "STKaiti"))
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" (car fonts) 15))
;;   (setq face-font-rescale-alist '(("STKaiti". 1.2))))

;; (when (eq system-type 'windows-nt)
;;   (setq fonts '("Inconsolata" "华文楷体"))
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" (car fonts) 20))
;;   (setq face-font-rescale-alist '(("华文楷体". 1.0))))

;; (when (eq system-type 'gnu/linux)
;;   (setq fonts '("Inconsolata" "STKaiti"))
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" (car fonts) 18))
;;   (setq face-font-rescale-alist '(("STKaiti". 1.0))))

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family (car (cdr fonts)))))


;; if gui do something in whatver type of emacs instance we are using
(defun apply-if-gui (&rest action)
  "Do specified ACTION if we're in a gui regardless of daemon or not."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p frame)
                      (apply action))))
    (if (display-graphic-p)
        (apply action))))

;; Default font (cant be font with hyphen in the name like Inconsolata-g)
(setq initial-frame-alist '((font . "JetBrainsMono")))
(setq default-frame-alist '((font . "JetBrainsMono")))

(defun cdadar/set-backup-fonts()
  "Set the emoji and glyph fonts."
  (when (display-graphic-p)
    (progn
      (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "JetBrainsMono" 13)) ;; 11 13 17 19 23
      ;; chinese font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "LXGW WenKai")))) ;; 14 16 20 22 28
    ))

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(apply-if-gui 'cdadar/set-backup-fonts)


;; (use-package cnfonts
;;   :config
;;   (require 'cnfonts)
;;   (cnfonts-mode 1)
;;   :bind  (:map cnfonts-mode-map
;;                ("C--" . cnfonts-decrease-fontsize)
;;                ("C-+" . cnfonts-increase-fontsize)))

(defun cdadar/reset-frame-size (&optional frame)
  "重设窗体大小"
  (interactive)
  (progn (add-to-list 'default-frame-alist '(height . 40))
         (add-to-list 'default-frame-alist '(width . 120))))

(apply-if-gui 'cdadar/reset-frame-size)


(provide 'init-personal)
;;; init-personal.el ends here
