;;; init-translate.el --- Translation utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gt
  :ensure t
  :commands (gt-translate gt-setup gt-switch-translator)
  :bind (("C-c s t" . gt-translate)
         ("C-c s T" . gt-setup))
  :preface
  (defun cdadar/gt-word-translator ()
    "Create a translator for word lookup between Chinese and English."
    (gt-translator
     :taker (gt-taker :langs '(en zh) :text 'word :pick nil :prompt t)
     :engines (list (gt-youdao-dict-engine)
                    (gt-youdao-suggest-engine :if '(and word src:en)))
     :render (gt-buffer-render)))

  (defun cdadar/gt-paragraph-translator ()
    "Create a translator for sentence or paragraph translation."
    (gt-translator
     :taker (gt-taker :langs '(en zh) :text 'paragraph :pick nil :prompt t)
     :engines (list (gt-bing-engine))
     :render (gt-buffer-render)))

  (defun cdadar/gt-preset-translators ()
    "Return the preset translators for `gt'."
    `((word . ,(cdadar/gt-word-translator))
      (paragraph . ,(cdadar/gt-paragraph-translator))))

  (defun cdadar/gt-setup-default-translator ()
    "Initialize `gt' presets and choose the word translator by default."
    (setq gt-preset-translators (cdadar/gt-preset-translators))
    (setq gt-default-translator (alist-get 'word gt-preset-translators)))
  :custom
  (gt-langs '(en zh))
  (gt-taker-text 'word)
  (gt-taker-pick nil)
  (gt-taker-prompt t)
  :config
  (cdadar/gt-setup-default-translator))

(use-package sdcv
  :if (executable-find "sdcv")
  :vc (:url "https://github.com/manateelazycat/sdcv" :rev :newest)
  :commands (sdcv-search-input
             sdcv-search-input+
             sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-check)
  :custom
  (sdcv-say-word-p t)                ;say word after search
  (sdcv-dictionary-data-dir
   (concat user-emacs-directory "sdcv-dict")) ;设置星际译王本地词典的目录
  (sdcv-dictionary-simple-list    ;星际译王屏幕取词词典, 简单, 快速
   '("懒虫简明英汉词典"
     "懒虫简明汉英词典"
     "KDic11万英汉词典"))
  (sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
   '(
     "懒虫简明英汉词典"
     "英汉汉英专业词典"
     "XDICT英汉辞典"
     "stardict1.3英汉辞典"
     "WordNet"
     "XDICT汉英辞典"
     "Jargon"
     "懒虫简明汉英词典"
     "FOLDOC"
     "新世纪英汉科技大词典"
     "KDic11万英汉词典"
     "朗道汉英字典5.0"
     "CDICT5英汉辞典"
     "新世纪汉英科技大词典"
     "牛津英汉双解美化版"
     "21世纪双语科技词典"
     "quick_eng-zh_CN")))

(provide 'init-translate)
;;; init-translate.el ends here
