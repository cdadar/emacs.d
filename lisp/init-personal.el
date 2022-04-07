;;; init-personal.el --- Emacs Prelude: Personal settings
;;
;;; Commentary:
;; Personal settings to augment those of Prelude
;; Install Emacs through homebrew with --cocoa --srgb

;;; Code:
;;; make cursor style bar
(setq-default cursor-type 'box)

(defun cdadar/reset-frame-size (&optional frame)
  "重设窗体大小"
  (interactive)
  (when frame
    (select-frame frame))
  (progn (set-frame-width (selected-frame) 120)
         (set-frame-height (selected-frame) 50)))

(add-hook 'after-make-frame-functions 'cdadar/reset-frame-size)


;; 如果配置好了， 下面20个汉字与40个英文字母应该等长
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

(when (display-graphic-p)
  (progn
    (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 16)) ;; 11 13 17 19 23
    ;; chinese font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
  )

(provide 'init-personal)
;;; init-personal.el ends here
