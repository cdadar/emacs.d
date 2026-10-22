;;; init-org-latex-fonts.el --- Org LaTeX 导出的字体与缺字回退 -*- lexical-binding: t -*-
;;; Commentary:
;; 导出 .tex 时的字体处理：主字体替换、本机没装的字体换替代字体、笔记未声明
;; CJK 字体时注入默认字体、正文缺字的回退映射、xeCJK 自带回退。
;; 全部在 `org-export-filter-final-output-functions' 的 final-output 阶段改字符串，
;; 注册顺序在 init-org-latex-export.el（那里决定了它们谁先跑）。
;;; Code:

(defconst cdadar/org-latex-english-main-font "TeX Gyre Termes"
  "English main font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-main-font "LXGW WenKai"
  "Default Chinese body font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-mono-font "LXGW WenKai Mono"
  "Default Chinese monospaced font used for Org LaTeX/PDF exports.")

(defconst cdadar/org-latex-cjk-quote-font "Kaiti SC"
  "Default Chinese quote font used for Org LaTeX/PDF exports.")

(defun cdadar/org-latex-tex-gyre-termes-mainfont-snippet ()
  "Return a portable fontspec snippet for TeX Gyre Termes.
Use file-name based lookup so TeX can find the OTF files through kpathsea even
when the TeX Live installation path changes."
  "\\setmainfont{texgyretermes}[
  Extension=.otf,
  UprightFont=*-regular,
  BoldFont=*-bold,
  ItalicFont=*-italic,
  BoldItalicFont=*-bolditalic,
  Scale=1.0]")

(defun cdadar/org-latex-use-english-main-font (text backend info)
  "Use `cdadar/org-latex-english-main-font' for Org LaTeX TEXT.
This keeps older notes that specify Times New Roman in their local headers
visually consistent with the current Org PDF export preference.  BACKEND and
INFO follow the Org export filter protocol."
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\setmainfont\\(?:\\[[^]]*\\]\\)?{Times New Roman}\\(?:[[:blank:]]*%.*\\)?"
       (cdadar/org-latex-tex-gyre-termes-mainfont-snippet)
       text t t)
    text))

(defconst cdadar/org-latex-font-fallbacks
  '(("思源宋体 CN" . "LXGW WenKai")
    ("思源宋体" . "LXGW WenKai")
    ("Noto Serif CJK SC" . "LXGW WenKai")
    ("Noto Serif SC" . "LXGW WenKai")
    ("思源黑体 CN" . "PingFang SC")
    ("楷体" . "Kaiti SC")
    ("Courier Std" . "Courier New"))
  "本机没装的字体 → 已装的替代字体。
每篇笔记仍然照旧写自己想要的字体；导出时那一行会被包进字体存在性判断，
本机装了真字体就自动用回真字体，不必改笔记。")

(defconst cdadar/org-latex-font-load-regexp
  (rx "\\"
      (or (seq "set" (optional "CJK") (or "main" "sans" "mono") "font")
          (seq "new" (optional "CJK") "fontfamily" "\\" (+ (any "A-Za-z@"))))
      (optional "[" (* (not (any "]"))) "]"))
  "匹配一条 LaTeX 字体加载命令（含可选参数）。")

(defun cdadar/org-latex-use-installed-fonts (text backend info)
  "本机没装的字体改用已装的字体，导出 LaTeX TEXT。
笔记照旧写自己想要的字体：这一行会被包进字体存在性判断，本机装了真字体
（比如以后补装了思源宋体）就自动用回真字体，不必改任何笔记。
BACKEND 与 INFO 遵循 Org 导出过滤器协议。"
  (ignore info)
  (if (org-export-derived-backend-p backend 'latex)
      (let ((case-fold-search nil))
        (dolist (pair cdadar/org-latex-font-fallbacks text)
          (setq text
                (replace-regexp-in-string
                 (concat cdadar/org-latex-font-load-regexp
                         "{" (regexp-quote (car pair)) "}")
                 (lambda (match)
                   ;; MATCH 是「命令 + {字体名}」；直接去掉尾部字体名得出命令，
                   ;; 不依赖 match data（替换回调里 match data 不可靠）。
                   (let ((command (substring match 0
                                             (- (length match)
                                                (length (car pair)) 2))))
                     (concat "\\IfFontExistsTF{" (car pair) "}"
                             "{" command "{" (car pair) "}}"
                             "{" command "{" (cdr pair) "}}")))
                 text t t)))
        text)
    text))

(defconst cdadar/org-latex-cjk-char-regexp
  (format "[%c-%c%c-%c%c-%c%c-%c]"
          #x3000 #x303F #x3400 #x4DBF #x4E00 #x9FFF #xFF00 #xFFEF)
  "中日韩标点、汉字与全角字符，用来判断文档是否真需要 CJK 字体。")

(defconst cdadar/org-latex-default-cjk-font-marker "cdadar-default-cjk-font"
  "已注入默认 CJK 字体时留在导言区的标记，用于避免重复注入。")

(defun cdadar/org-latex-default-cjk-font-snippet ()
  "Return the default xeCJK setup for notes that declare no CJK font."
  (format "\\usepackage{xeCJK}
%% %s
\\setCJKmainfont[Scale=1.0]{%s}
\\setCJKsansfont[Scale=1.0]{%s}
\\setCJKmonofont[Scale=1.0]{%s}"
          cdadar/org-latex-default-cjk-font-marker
          cdadar/org-latex-cjk-main-font
          cdadar/org-latex-cjk-main-font
          cdadar/org-latex-cjk-mono-font))

(defun cdadar/org-latex-default-cjk-font (text backend info)
  "Give a note that declares no CJK font the default one.
A note without `\\setCJKmainfont' renders every Chinese character with the main
font, i.e. as nothing.  Notes that declare their own CJK font, or that contain no
CJK characters at all, are left alone.  BACKEND and INFO follow the Org export
filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "fontspec" text)
           (string-match-p cdadar/org-latex-cjk-char-regexp text)
           (not (string-match-p "\\\\setCJK\\(main\\|sans\\|mono\\)font" text))
           (not (string-match-p cdadar/org-latex-default-cjk-font-marker text))
           (string-match-p (regexp-quote "\\begin{document}") text))
      (replace-regexp-in-string
       (regexp-quote "\\begin{document}")
       (concat (cdadar/org-latex-default-cjk-font-snippet) "\n\\begin{document}")
       text t t)
    text))

(defconst cdadar/org-latex-glyph-fallback-ranges
  '((#x0250 #x02FF "\\cdadaripa")        ; 国际音标与修饰符
    (#x0370 #x03FF "\\cdadargreek")      ; 希腊文
    (#x1F00 #x1FFF "\\cdadargreek")      ; 希腊文扩展（带音）
    (#x0590 #x05FF "\\cdadarheb")        ; 希伯来文（字母、元音符号、读经记号）
    (#x2460 #x24FF "\\cdadarcjk")        ; 圈号 ①-⑳ ⑴-⒇
    (#x2500 #x257F "\\cdadarcjk")        ; 制表符
    (#x25A0 #x25FF "\\cdadarsym")        ; 几何图形 ▸ ●
    (#x2600 #x26FF "\\cdadarsym")        ; 杂项符号 ☆ ★ ☀
    (#x2700 #x27BF "\\cdadaremoji")      ; Dingbats ✅ ❤ ✂（Apple Symbols 无）
    (#x2B00 #x2BFF "\\cdadaremoji")      ; ⭐ ⭕ ⬛ ⬅
    (#x200D #x200D "\\cdadaremoji")      ; ZWJ（组合 emoji 序列）
    (#x1F000 #x1FAFF "\\cdadaremoji"))   ; 表情符号区 🔥 👍 🎉
  "字符区间 → 回退字体命令。
Times 系主字体没有国际音标、希伯来文、带音希腊文、圈号、制表符、几何符号与杂项
符号，XeTeX 又不像 LuaTeX 那样自动回退，那些字符会变成方块。这里的分配是：音标用
Lato，希腊文用笔记自己的 SBL Greek，希伯来文（含元音符号 niqqud，笔记里常它直接
写在中文句子里）用 SBL Hebrew，CJK 语境的圈号/制表符用 CJK 字体，文字类符号用系统
符号字体，emoji 与 Dingbats 用单色 emoji 字体（安装见
`~/project/private/script/local/install_emoji_fonts.sh'）。

区间互不重叠，`cdadar/org-latex-glyph-fallback-chars' 按列表顺序取第一个命中的
区间，所以重叠会让结果依赖顺序。

全角中点 ・(U+30FB) 与变体选择符 VS15/VS16(U+FE0E/FE0F) 这类缺字不在这里：
它们被 xeCJK 归入 MiddlePunct / CM 类，由 `cdadar/org-latex-cjk-autofallback'
打开 xeCJK 自带的回退（并把 emoji 字体接到回退链上）顶替，写成 `\\newunicodechar'
对它们无效。")

(defconst cdadar/org-latex-glyph-fallback-marker "cdadar-glyph-fallback"
  "已注入回退字体设置时留在导言区的标记，用于避免重复注入。")

(defun cdadar/org-latex-unicodechar-mapped-p (text char)
  "Return non-nil when TEXT already maps CHAR with `\\newunicodechar'.
Redefining such a character puts an active copy of it inside its own replacement
text, and newunicodechar then expands it forever."
  (string-match-p (concat (regexp-quote "\\newunicodechar{")
                          (regexp-quote (string char))
                          "}")
                  text))

(defun cdadar/org-latex-glyph-fallback-chars (text)
  "Return the characters in TEXT that need a fallback font.
Each element is (CHARACTER . FONT-COMMAND-NAME).  Characters the note maps
itself with `\\newunicodechar' are left to the note."
  (let (found)
    (dolist (char (delete-dups (string-to-list text)))
      (dolist (range cdadar/org-latex-glyph-fallback-ranges)
        (when (and (<= (nth 0 range) char (nth 1 range))
                   (not (assq char found))
                   (not (cdadar/org-latex-unicodechar-mapped-p text char)))
          (push (cons char (nth 2 range)) found))))
    (sort found #'car-less-than-car)))

(defun cdadar/org-latex-glyph-fallback-snippet (text)
  "Return LaTeX fallback-font setup for the characters in TEXT, or nil."
  (let ((chars (cdadar/org-latex-glyph-fallback-chars text)))
    (when chars
      (concat
       "\\usepackage{newunicodechar}\n"
       (format "%% %s\n" cdadar/org-latex-glyph-fallback-marker)
       "\\IfFontExistsTF{SBL Greek}"
       "{\\providefontfamily\\cdadargreek{SBL Greek}}"
       "{\\providefontfamily\\cdadargreek{DejaVu Sans}}\n"
       "\\IfFontExistsTF{SBL Hebrew}"
       "{\\providefontfamily\\cdadarheb[Script=Hebrew]{SBL Hebrew}}"
       "{\\providefontfamily\\cdadarheb[Script=Hebrew]{DejaVu Sans}}\n"
       (format "\\providefontfamily\\cdadarcjk{%s}\n" cdadar/org-latex-cjk-main-font)
       "\\IfFontExistsTF{Apple Symbols}"
       "{\\providefontfamily\\cdadarsym{Apple Symbols}}"
       "{\\providefontfamily\\cdadarsym{Arial Unicode MS}}\n"
       "\\IfFontExistsTF{Lato}"
       "{\\providefontfamily\\cdadaripa{Lato}}"
       "{\\providefontfamily\\cdadaripa{DejaVu Sans}}\n"
       "\\IfFontExistsTF{Noto Emoji}"
       "{\\providefontfamily\\cdadaremoji{Noto Emoji}}"
       "{\\providefontfamily\\cdadaremoji{Symbola}}\n"
       (mapconcat (lambda (pair)
                    (format "\\newunicodechar{%c}{{%s %c}}"
                            (car pair) (cdr pair) (car pair)))
                  chars "\n")))))

(defun cdadar/org-latex-add-glyph-fallback (text backend info)
  "Redirect characters the main font lacks to a font that has them.
The Times-like main font has no accented Greek, circled numbers, box drawing
characters or geometric shapes, and XeTeX has no automatic font fallback, so
they would come out blank; only the characters actually present in TEXT are
remapped.  Requires fontspec, hence the guard.  BACKEND and INFO follow the Org
export filter protocol."
  (ignore info)
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "fontspec" text)
           (not (string-match-p cdadar/org-latex-glyph-fallback-marker text)))
      (let ((snippet (cdadar/org-latex-glyph-fallback-snippet text)))
        (if snippet
            (replace-regexp-in-string
             (regexp-quote "\\begin{document}") (concat snippet "\n\\begin{document}")
             text t t)
          text))
    text))

(defconst cdadar/org-latex-cjk-autofallback-marker "cdadar-cjk-autofallback"
  "标记 xeCJK 缺字回退那行由导出过滤器注入，便于在导出的 .tex 里找到它。")

(defun cdadar/org-latex-cjk-autofallback-snippet (text)
  "Return the xeCJK fallback setup for TEXT.
AutoFallBack 让当前 CJK 字体族缺字时换用回退字体；两个
`\\setCJKfallbackfamilyfont' 把 emoji 字体接到回退链上——变体选择符 U+FE00-FE0F
被 xeCJK 归入 CM 类，`\\newunicodechar' 对它无效，只能靠字体回退。
TEXT 里出现 quotecjkfont（quote 用楷体）时额外给那个字体族也接上。"
  (concat
   (format "%% %s\n" cdadar/org-latex-cjk-autofallback-marker)
   "\\xeCJKsetup{AutoFallBack=true}\n"
   "\\IfFontExistsTF{Noto Emoji}"
   "{\\setCJKfallbackfamilyfont{\\CJKrmdefault}{Noto Emoji}}"
   "{\\setCJKfallbackfamilyfont{\\CJKrmdefault}{Symbola}}\n"
   (when (string-match-p "quotecjkfont" text)
     (concat "\\IfFontExistsTF{Noto Emoji}"
             "{\\setCJKfallbackfamilyfont{quotecjkfont}{Noto Emoji}}"
             "{\\setCJKfallbackfamilyfont{quotecjkfont}{Symbola}}\n"))))

(defconst cdadar/org-latex-variation-selectors
  "[\uFE00-\uFE0F\U000E0100-\U000E01EF]"
  "变体选择符（VS1-VS16 与补充区）：只影响 emoji 的呈现，排版时直接删掉。
它们被 xeCJK 归入 CM（组合标记）类，`\\newunicodechar' 对它无效；删掉既不用
依赖某个字体恰好有这几个零宽字形，也不影响字形（emoji 本身由 cdadaremoji 映射
负责）。")

(defun cdadar/org-latex-add-cjk-autofallback (text backend info)
  "Let xeCJK substitute glyphs missing from the CJK family in use.
Quote environments switch to Kaiti SC (see `cdadar/org-latex-cjk-quote-font'),
and Kaiti SC has no glyph for the full-width middle dot ・(U+30FB) -- the frame
in such a note then shows a tofu box.  Turning on xeCJK's own fallback replaces
such glyphs with the note's body CJK font.  A `\\newunicodechar' mapping cannot
fix this: the middle dot is an xeCJK MiddlePunct character, so xeCJK picks the font again
inside the replacement text.  BACKEND and INFO follow the Org export filter
protocol."
  (ignore info)
  (if (not (org-export-derived-backend-p backend 'latex))
      text
    ;; 变体选择符先删（CM 类字符只能靠字体回退，而回退字体未必有这两个零宽字形）
    (let ((text (replace-regexp-in-string cdadar/org-latex-variation-selectors
                                          "" text t t)))
      (if (and (string-match-p "xeCJK" text)
               ;; 笔记自己的 preamble（模板 snippet）可能已经写了这一行
               (not (string-match-p "AutoFallBack" text))
               (string-match-p (regexp-quote "\\begin{document}") text))
          (replace-regexp-in-string
           (regexp-quote "\\begin{document}")
           (concat (cdadar/org-latex-cjk-autofallback-snippet text)
                   "\n\\begin{document}")
           text t t)
        text))))

(provide 'init-org-latex-fonts)
;;; init-org-latex-fonts.el ends here
