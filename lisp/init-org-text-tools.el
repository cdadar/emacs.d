;;; init-org-text-tools.el --- 中文/多文种笔记的文本工具 -*- lexical-binding: t -*-
;;; Commentary:
;; 与导出后端无关、只改 Org 源码文本的交互命令：
;; - 圈号 ①..⑳ 统一写成 \textcircled{N}
;; - 裸的希伯来文/希腊文包成笔记前言的 \heb{} / \gr{}
;; - 给紧贴汉字而失效的强调标记补边界空格
;; - 拆掉被行内代码里的同名标记抢掉闭合的强调（嵌套冲突）
;; - 嵌套强调 → LaTeX 导出片段（或补空格走原生嵌套）
;; - 剪贴板 Markdown → Org（走 pandoc）
;;; Code:



(defconst cdadar/org-circled-number-regexp
  (format "[%c-%c]" #x2460 #x2473)
  "匹配 ①..⑳（U+2460..U+2473）。")

(defun cdadar/org-replace-circled-numbers ()
  "Replace ①..⑳ in the current Org buffer with `\\textcircled{N}'.
笔记里本来就到处用 `\\textcircled{1}'，这个命令把从别处粘来的圈号字符统一成同一写法。
跳过 `#' 开头的行（关键字、注释、块分隔符；笔记 header 可能用 `\\newunicodechar'
映射过这个字符，改了会破坏映射）与 src / example 块内容；整体算一次 undo。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((count 0))
    (atomic-change-group
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cdadar/org-circled-number-regexp nil t)
          (let ((char (char-after (match-beginning 0))))
            (unless (or (save-excursion (forward-line 0)
                                        (looking-at-p "[[:blank:]]*#"))
                        (org-in-block-p '("src" "example")))
              (replace-match (format "\\textcircled{%d}" (- char #x245F)) t t)
              (setq count (1+ count)))))))
    (message "圈号 → \\textcircled{}：替换 %d 处" count)
    count))

(defconst cdadar/org-hebrew-char-regexp
  (format "[%c-%c]" #x0590 #x05FF)
  "匹配希伯来文块：字母、元音符号 niqqud（U+05B0-05BD 等）与读经记号。")

(defconst cdadar/org-greek-char-regexp
  (format "[%c-%c%c-%c]" #x0370 #x03FF #x1F00 #x1FFF)
  "匹配希腊文与扩展区（多音符号，如 ἀγαπάω、ἐκκλησία）。")

(defun cdadar/org-script-run-regexp (char-regexp)
  "Return a regexp matching runs of CHAR-REGEXP separated by blanks.
一段 = 连续的该文种字符，可跨空白（不跨换行）继续，让 `בָּרוּךְ אַתָּה'
与 `μακάριοι οἱ καθαροί' 包成同一个宏参数；首尾空白留在匹配之外，不会吃掉
后面中文。"
  (format "\\(?:%s\\)\\(?:[[:blank:]]*\\(?:%s\\)\\)*" char-regexp char-regexp))

(defun cdadar/org-script-run-wrapped-p (macros)
  "Return non-nil when the current match already sits inside MACROS{...}.
MACROS 是该文种可接受的宏名（笔记自家简写与 polyglossia 写法），已包过的
不再叠一层；命令要能反复跑（笔记是一直加内容的）。
先取下 match 两端：`looking-back' 自己会写 match data，之后再取 match-end 就是
它自己那个匹配的了。"
  (save-match-data
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (open (concat "\\(?:" (regexp-opt macros) "\\){")))
      (and (save-excursion
             (goto-char beg)
             (looking-back open (max (point-min) (- beg 16))))
           (save-excursion
             (goto-char end)
             (looking-at-p "}"))))))

(defun cdadar/org-script-macro-for (shorthand)
  "Return the macro to wrap a script with in the current buffer, or nil.
前言里定义了 SHORTHAND（`\\heb' 这个简写，模板都这么写）才用它；没定义就返回
nil——这类老笔记里既没有简写也没有 polyglossia，硬包上去导出必报
`Undefined control sequence'（实测 3 篇如此；它们的希伯来文/希腊文靠缺字回退
照常渲染），由调用者跳过并在 message 里报出来。
判断不写反斜杠正则：`{\\heb}' 这个字面串只在定义处出现，用 `regexp-quote' 拼。"
  (when (string-match-p (concat "{" (regexp-quote shorthand) "}") (buffer-string))
    shorthand))

(defun cdadar/org-wrap-script-runs (char-regexp macros)
  "Wrap runs of CHAR-REGEXP with the first of MACROS in the current buffer.
MACROS 是同一文种的可接受宏名（自己的简写在前，polyglossia 写法在后）：已经在
其中任何一个里的段落不再包。跳过 `#' 开头的行与 src / example / export 块。
起点/终点先取出来再替换：中间那些检查会跑搜索，`replace-match' 靠的 match
data 靠不住。返回包的个数。"
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (let ((re (cdadar/org-script-run-regexp char-regexp)))
        (while (re-search-forward re nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (run (match-string 0)))
            (unless (or (save-excursion (forward-line 0)
                                        (looking-at-p "[[:blank:]]*#"))
                        (org-in-block-p '("src" "example" "export"))
                        (cdadar/org-script-run-wrapped-p macros))
              (delete-region beg end)
              (insert (car macros) "{" run "}")
              (setq count (1+ count)))))))
    count))

(defun cdadar/org-wrap-hebrew-greek ()
  "Wrap every Hebrew and Greek run in the current Org buffer with `\\heb{}' / `\\gr{}'.
从别处粘来的经文常常是裸字符（没包宏），导出时只能落在主字体上，靠
`cdadar/org-latex-add-glyph-fallback' 的缺字回退兜底。这个命令把正文里的
希伯来文/希腊文按文种包成笔记自家前言的 `\\heb{}' / `\\gr{}'（= `\\texthebrew' /
`\\textgreek'）：字体、RTL、字体缩放都交给 polyglossia，源码也不再依赖导出过滤器。
前言里没有 `\\newcommand{\\heb}' / `\\newcommand{\\gr}' 时跳过该文种并在 message 里
报出来：那类笔记没有可用的宏，硬包只会让导出报 Undefined control sequence。
跳过 `#' 开头的行（关键字、注释、块分隔符；header 里就是 `\\heb{}' 的定义与例子）、
src / example / export 块，以及已经包过的（含 `\\hebl{}' / `\\grl{}' /
`\\texthebrew{}' / `\\textgreek{}'）；整体算一次 undo，返回包的个数。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((count 0) (skipped nil))
    (atomic-change-group
      (dolist (spec (list (list cdadar/org-hebrew-char-regexp "希伯来文" "\\heb"
                                '("\\heb" "\\hebl" "\\texthebrew"))
                          (list cdadar/org-greek-char-regexp "希腊文" "\\gr"
                                '("\\gr" "\\grl" "\\textgreek"))))
        (let ((macro (cdadar/org-script-macro-for (nth 2 spec))))
          (if macro
              (setq count (+ count (cdadar/org-wrap-script-runs
                                    (nth 0 spec) (cons macro (nth 3 spec)))))
            (push (nth 1 spec) skipped)))))
    (message "希伯来文/希腊文 → \\heb{} / \\gr{}：包了 %d 处%s" count
             (if skipped
                 (format "；%s 跳过（前言没有对应的 \\heb / \\gr 定义）"
                         (mapconcat #'identity (nreverse skipped) "、"))
               ""))
    count))


;; org 的强调边界写死在 `org-element--parse-generic-emphasis'（org 9.8）里：开标记前
;; 只能是空白、`-'、ASCII 括号与引号、`{' 或行首，闭标记后只能是空白、`-'、ASCII
;; 句读与括号或行尾。中文笔记里的 `说*重点*好啊'、`2. *结论*：…' 紧贴汉字，org 既不
;; 加粗也不导出；`org-emphasize'（C-c e）只补它自己新插入的标记，补不了写好的文字。
(defconst cdadar/org-emphasis-markers "*_=~"
  "`cdadar/org-fix-emphasis-boundaries' 处理的强调标记。
`/' 不在内：中文笔记里 `/` 几乎都是路径与并列分隔符（`图像/视频'、`视频_素材/杂项'），
按斜体处理会大量误判。
`+' 也不在内，这是实测的：近 120 天笔记里成对且紧贴汉字的 `+' 只有 4 处，全部是
「加号 / 连接符」而不是删除线（`.rtf 14.9万+.doc 11.8万'、`（杂项+待定+按格式_PDF）'），
补空格会把这些文字真的变成删除线；同一批语料里 `*' 命中 261 行、`=' 312 行、`~' 5 行，
命中的全是紧贴全角标点的强调。要用 `+' 就把它加回这个字符串。")

(defconst cdadar/org-emphasis-opening-chars "-('\"{"
  "开标记前 org 认的 ASCII 边界字符（其余合法边界是空白与行首）。")

(defconst cdadar/org-emphasis-closing-chars "-.,;:!?'\")}\\["
  "闭标记后 org 认的 ASCII 边界字符（其余合法边界是空白与行尾）。")

(defconst cdadar/org-emphasis-repairable-types
  '(paragraph headline table-cell item quote-block center-block verse-block)
  "org 会解析强调对象的节点类型。
其余节点里的标记不是强调或不能动：src / example / export 块、comment、fixed-width
行、链接（含中文文件路径）、verbatim / code、LaTeX 片段、`#+NAME:' 这类关键字。")

(defun cdadar/org-emphasis-boundary-p (char chars)
  "Return non-nil when CHAR is a valid org emphasis border.
CHARS are the ASCII characters org accepts on that side; every other valid
border is whitespace -- including U+3000, which Emacs gives whitespace syntax
-- or the beginning / end of the buffer."
  (or (null char)
      (memq char (string-to-list chars))
      (eq (char-syntax char) ?\s)))       ; ?\s 就是空格字符，U+3000 也是这个语法类

(defun cdadar/org-emphasis-repairable-p (marker content before after bad-left bad-right)
  "Return non-nil when a broken marker pair is emphasis worth a space.
MARKER is the emphasis character, CONTENT what sits between the two markers,
BEFORE / AFTER the characters around the pair, BAD-LEFT / BAD-RIGHT whether
that side fails `cdadar/org-emphasis-boundary-p'.  Point must be at the first
character of CONTENT.
Only a border blocked by a non-ASCII character counts as Chinese emphasis:
all-ASCII pairs (`a*b*c', `sdk=[34,35] ... sdk=34') stay untouched, as do
marker runs (`**bold**', which org parses on its own)."
  (and (or bad-left bad-right)
       (or (and bad-left before (> before 127))
           (and bad-right after (> after 127)))
       (not (memq marker (list before after)))
       (not (memq marker (string-to-list content)))
       ;; `bold' 这类：标记已经被 org 解析成了一个（可能跨过头的）强调对象，也要能修。
       (memq (org-element-type (org-element-context))
             (append cdadar/org-emphasis-repairable-types
                     '(bold italic underline strike-through)))))

(defun cdadar/org-fix-emphasis-boundaries (beg end)
  "给因缺少边界空格而失效的 org 强调标记补空格。
org 的强调边界写死在 `org-element--parse-generic-emphasis' 里，中文笔记里紧贴汉字的
标记（`说*重点*好啊'、`—*结论*：…'）既不显示也不导出为粗体，而 `org-emphasize'
（C-c e）只补它自己新插入的标记。在 BEG..END（有选区时是选区，否则整个 buffer）
内找这种标记，在紧贴非 ASCII 字符的那一侧补一个空格，与 `org-emphasize' 补的一致。
只动 org 会解析强调的地方，且只动中文相关的边界：`a*b*c'、`sdk=[34,35] … sdk=34' 这类全
ASCII 的算式与标识符不动，src / example / export 块、链接路径、verbatim / code、LaTeX
片段、`#+NAME:' 这类关键字里的标记也不动。整条命令算一次 undo，可以反复跑（修好的不再
是候选）；处理哪几个标记见 `cdadar/org-emphasis-markers'。返回补过的标记对数。"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((regexp (format "\\([%s]\\)\\([^[:space:]][^\n]*?[^[:space:]]\\)\\1"
                        cdadar/org-emphasis-markers))
        (end (copy-marker end t))
        (pairs 0)
        (spaces 0))
    (atomic-change-group
      (save-excursion
        (goto-char beg)
        (while (re-search-forward regexp end t)
          (let* ((marker (string-to-char (match-string-no-properties 1)))
                 (content (match-string-no-properties 2))
                 (open-beg (match-beginning 1))
                 (before (char-before open-beg))
                 (after (char-after (match-end 0)))
                 (bad-left (not (cdadar/org-emphasis-boundary-p
                                 before cdadar/org-emphasis-opening-chars)))
                 (bad-right (not (cdadar/org-emphasis-boundary-p
                                  after cdadar/org-emphasis-closing-chars)))
                 ;; 搜索点跟着插入的文本往后走，保证循环推进；`org-element-context'
                 ;; 会冲掉 match data，所以位置先存下来。
                 (resume (copy-marker (match-end 0) t)))
            (when (save-excursion
                    (goto-char (1+ open-beg))
                    (cdadar/org-emphasis-repairable-p
                     marker content before after bad-left bad-right))
              ;; 先补右边界再补左边界，前面的位置不受影响。
              (when bad-right
                (goto-char resume)
                (insert " ")
                (setq spaces (1+ spaces)))
              (when bad-left
                (goto-char open-beg)
                (insert " ")
                (setq spaces (1+ spaces)))
              (setq pairs (1+ pairs)))
            (goto-char resume)
            (set-marker resume nil)))
        (set-marker end nil)))
    (message "强调边界补空格：%d 处标记、%d 个空格" pairs spaces)
    pairs))


;; === 嵌套：内容里的同名标记抢闭合 ===
(defconst cdadar/org-emphasis-pair-markers "*_+/"
  "会被内容里的同名标记抢掉闭合的强调标记。
`=' 与 `~' 不在此列：它们是代码 / 等宽分隔符，按构造内容里不可能再出现同名标记。")

(defconst cdadar/org-inline-code-regexp "\\([=~]\\)\\(\\S-[^=~\n]*?\\S-\\)\\1"
  "行内代码 / 等宽段的整段匹配（`=code='、`~verbatim~'），要求内容两端都不是空白。")

(defun cdadar/org--inline-code-ranges (line)
  "Return the inline-code ranges of LINE as a list of (BEG . END) offsets."
  (let (ranges (start 0))
    (while (string-match cdadar/org-inline-code-regexp line start)
      (push (cons (match-beginning 0) (match-end 0)) ranges)
      (setq start (match-end 0)))
    (nreverse ranges)))

(defun cdadar/org--emphasis-closing-position-p (line pos)
  "Return non-nil when the marker at POS in LINE can close an emphasis.
That is org's rule: a non-whitespace character before it, and whitespace,
a closing ASCII border, or the end of line after it."
  (let ((prev (and (> pos 0) (aref line (1- pos))))
        (next (and (< (1+ pos) (length line)) (aref line (1+ pos)))))
    (and prev
         (not (eq (char-syntax prev) ?\s))
         (or (null next)
             (eq (char-syntax next) ?\s)
             (memq next (string-to-list cdadar/org-emphasis-closing-chars))))))

(defun cdadar/org--unsafe-emphasis-pairs (line marker)
  "把 LINE 里被行内代码中同名标记抢掉闭合的 MARKER 配对找出来。
返回 ((OPEN . CLOSE) ...)，偏移相对 LINE 起点。只算那些「代码段整个落在配对中间、
且段内的 MARKER 落在 org 的闭标记位置」的配对；其余配对在 org 里仍能正确闭合。"
  (let* ((ranges (cdadar/org--inline-code-ranges line))
         (in-code-p (lambda (pos)
                      (seq-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r))))
                                ranges)))
         (positions nil)
         (i 0)
         pairs)
    (while (< i (length line))
      (when (and (eq (aref line i) marker) (not (funcall in-code-p i)))
        (push i positions))
      (setq i (1+ i)))
    (setq positions (nreverse positions))
    (while (cdr positions)
      (let ((open (pop positions))
            (close (pop positions)))
        (when (seq-some
               (lambda (r)
                 (and (> (car r) open)
                      (< (cdr r) close)
                      (let ((p (1+ (car r)))
                            (hit nil))
                        (while (< p (1- (cdr r)))
                          (when (and (eq (aref line p) marker)
                                     (cdadar/org--emphasis-closing-position-p line p))
                            (setq hit t))
                          (setq p (1+ p)))
                        hit)))
               ranges)
          (push (cons open close) pairs))))
    (nreverse pairs)))

(defun cdadar/org--emphasis-raw-context-p (pos)
  "Return non-nil when POS holds markup org does not parse as emphasis.
src / example / export 块、注释、`:NAME:' 类关键字、链接路径、代码与 LaTeX 片段里的
标记改了只会弄坏内容，`cdadar/org-fix-emphasis-nesting' 一律不碰。"
  (save-excursion
    (goto-char pos)
    (memq (org-element-type (org-element-context))
          '(link code verbatim latex-fragment macro src-block example-block
                 export-block comment fixed-width keyword))))

(defun cdadar/org-fix-emphasis-nesting (beg end &optional report-only)
  "拆掉被行内代码里的同名标记抢掉闭合的强调标记。
org 的强调是扁平正则：`*外层 =a*b= 内层*' 里，代码段中的 `*'（前非空白，后紧跟空白、
`- . , ; : ! ? ' \" ) } \\ [' 或行尾）会先被当成粗体的闭标记，结果 code 后面的文字被吞进
粗体、真正的闭标记反过来成了新的开标记。这和 `cdadar/org-fix-emphasis-boundaries'
处理的全角边界不是一回事，补空格也救不了，只能拆掉外层标记。

在 BEG..END（有选区时是选区，否则整个 buffer）内逐行检查，只删掉「跨过一个含同名
标记的代码段」的那一对标记：同一行里别处的强调、代码段本身、以及代码段里那个标记都
原样保留。边界没修好的行会算错位，所以同一行既有全角边界问题又有嵌套冲突时，先跑
`cdadar/org-fix-emphasis-boundaries'（C-c C-x F）再跑本命令。有前缀参数（REPORT-ONLY
非 nil）时只报告不修改。整条命令算一次 undo，可以反复跑。表格里的改动不会自动重排，
事后 `M-x org-table-align'。返回拆掉的配对数。"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (point-min) (point-max) current-prefix-arg)))
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((found nil)
        (lines nil)
        (pairs 0))
    (atomic-change-group
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let* ((line-beg (line-beginning-position))
                 (line (buffer-substring-no-properties line-beg (line-end-position)))
                 (hits nil))
            (dolist (marker (string-to-list cdadar/org-emphasis-pair-markers))
              (dolist (pair (cdadar/org--unsafe-emphasis-pairs line marker))
                ;; 只在 org 真会解析强调的地方动：src / example 块、链接里的标记不碰。
                (unless (or (cdadar/org--emphasis-raw-context-p (+ line-beg (car pair)))
                            (cdadar/org--emphasis-raw-context-p (+ line-beg (cdr pair))))
                  (push (+ line-beg (car pair)) hits)
                  (push (+ line-beg (cdr pair)) hits))))
            (when hits
              (push (line-number-at-pos line-beg) lines)
              (setq pairs (+ pairs (/ (length hits) 2)))
              (setq found (append hits found)))
            (forward-line 1)))
        (unless report-only
          (dolist (pos (sort found #'>))
            (goto-char pos)
            (delete-char 1))))
    (message "%s强调嵌套：%d 对标记%s%s"
             (if report-only "待拆" "已拆")
             pairs
             (if found "" "（无冲突）")
             (if lines
                 (format "，第 %s 行%s"
                         (mapconcat #'number-to-string
                                    (seq-take (nreverse lines) 5) "、")
                         (if (> (length lines) 5) " 等" ""))
               ""))
    pairs)))


;; === 嵌套强调 → LaTeX 宏 ===
(defconst cdadar/org-latex-emphasis-markers "*_=~"
  "转换成 LaTeX 宏的 org 标记：`*粗*' → `\\textbf{}'、`_下划线_' → `\\underline{}'、
`=code=' / `~verbatim~' → `\\texttt{}'。
`/' 与 `+' 不在内：中文笔记里的 `/` 几乎都是路径与并列分隔符，按斜体处理会误判；
`+' 要 ulem 的 `\\sout'，而这两个标记本来就能在 org 里正常嵌套。")

(defconst cdadar/org-latex-emphasis-macros
  '((?* . "\\textbf{%s}")
    (?_ . "\\underline{%s}")
    (?= . "\\texttt{%s}")
    (?~ . "\\texttt{%s}"))
  "标记字符 → LaTeX 宏模板。")

(defconst cdadar/org-latex-escape-alist
  '((?\\ . "\\textbackslash{}")
    (?_ . "\\_")
    (?% . "\\%")
    (?& . "\\&")
    (?# . "\\#")
    (?$ . "\\$")
    (?^ . "\\textasciicircum{}")
 (?{ . "\\{")
    (?} . "\\}")
    (?~ . "\\textasciitilde{}"))
  "LaTeX 文本模式下的特殊字符转义表。")

(defun cdadar/org--latex-escape (s)
  "Return S with LaTeX special characters escaped."
  (mapconcat (lambda (ch)
               (or (cdr (assq ch cdadar/org-latex-escape-alist))
                   (char-to-string ch)))
             (string-to-list s) ""))

(defun cdadar/org--latex-wrap (marker body)
  "Wrap BODY in the LaTeX macro for MARKER."
  (format (or (cdr (assq marker cdadar/org-latex-emphasis-macros))
              "%s")
          body))

(defun cdadar/org--latex-convert (s)
  "Rewrite the org markers in S as LaTeX macros, escaping plain text.
只处理成对且内容非空的标记；找不到闭合的那一个当普通字符转义。"
  (let ((out "")
        (i 0)
        (n (length s)))
    (while (< i n)
      (let* ((ch (aref s i))
             (marker (and (memq ch (string-to-list cdadar/org-latex-emphasis-markers)) ch))
             (close (and marker
                         (string-match (regexp-quote (char-to-string ch)) s (1+ i)))))
        (if (and close (> close (1+ i)))
            (setq out (concat out
                              (cdadar/org--latex-wrap
                               ch
                               (cdadar/org--latex-convert (substring s (1+ i) close))))
                  i (1+ close))
          (setq out (concat out (cdadar/org--latex-escape (char-to-string ch)))
                i (1+ i)))))
    out))

(defun cdadar/org--latex-border-open-p (s pos)
  "Return non-nil when the marker at POS in S may open an emphasis.
同 org 的规则，但额外允许前面紧靠另一个标记（`*=code=*' 这类 org 能正常解析）。"
  (let ((prev (and (> pos 0) (aref s (1- pos))))
        (next (and (< (1+ pos) (length s)) (aref s (1+ pos)))))
    (and (or (null prev)
             (eq (char-syntax prev) ?\s)
             (memq prev (string-to-list
                         (concat "-('\"{" cdadar/org-latex-emphasis-markers))))
         next
         (not (eq (char-syntax next) ?\s)))))

(defun cdadar/org--latex-border-close-p (s pos)
  "Return non-nil when the marker at POS in S may close an emphasis."
  (let ((prev (and (> pos 0) (aref s (1- pos))))
        (next (and (< (1+ pos) (length s)) (aref s (1+ pos)))))
    (and prev
         (not (eq (char-syntax prev) ?\s))
         (or (null next)
             (eq (char-syntax next) ?\s)
             (memq next (string-to-list
                         (concat cdadar/org-emphasis-closing-chars
                                 cdadar/org-latex-emphasis-markers)))))))

(defun cdadar/org--in-ranges-p (ranges pos)
  "Return non-nil when POS falls inside one of RANGES."
  (seq-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r)))) ranges))

(defun cdadar/org--line-pairs (line)
  "Return the emphasis pairs ((OPEN . CLOSE) ...) org would see in LINE.
按 org 的边界规则左右扫描（标记可以嵌套），行内代码段里的标记不算。"
  (let* ((ranges (cdadar/org--inline-code-ranges line))
         (markers (string-to-list cdadar/org-latex-emphasis-markers))
         (i 0)
         (n (length line))
         (stack nil)
         pairs)
    (while (< i n)
      (unless (cdadar/org--in-ranges-p ranges i)
        (let ((ch (aref line i)))
          (when (memq ch markers)
            (cond ((and stack (eq (caar stack) ch)
                        (cdadar/org--latex-border-close-p line i))
                   (push (cons (cdar stack) i) pairs)
                   (pop stack))
                  ((cdadar/org--latex-border-open-p line i)
                   (push (cons ch i) stack))))))
      (setq i (1+ i)))
    pairs))

(defun cdadar/org--glued-markers (line)
  "Return the positions of markers in LINE that can neither open nor close.
这种标记就是「紧贴汉字、org 解析不了」的内层标记，也是要重写的触发点。"
  (let ((ranges (cdadar/org--inline-code-ranges line))
        (markers (string-to-list cdadar/org-latex-emphasis-markers))
        hits
        (i 0)
        (n (length line)))
    (while (< i n)
      (when (and (memq (aref line i) markers)
                 (not (cdadar/org--in-ranges-p ranges i))
                 (not (cdadar/org--latex-border-open-p line i))
                 (not (cdadar/org--latex-border-close-p line i)))
        (push i hits))
      (setq i (1+ i)))
    (nreverse hits)))

(defun cdadar/org--rewrite-line (line add-spaces)
  "Rewrite the unparsable nested emphasis in LINE.
Return (NEW-LINE . PLACES), PLACES being the number of rewritten spans; glue
markers that no recognisable pair encloses are left alone and reported through
`cdadar/org--rewrite-line-leftovers' instead."
  (let* ((pairs (cdadar/org--line-pairs line))
         (glued (cdadar/org--glued-markers line))
         (targets nil)
         (leftovers 0))
    (dolist (pos glued)
      (let ((enclosing (seq-filter (lambda (p) (and (< (car p) pos) (> (cdr p) pos))) pairs)))
        (if enclosing
            ;; 取最内层的合法外层（同一行可能有多处）
            (let ((best (car (sort enclosing
                                   (lambda (a b) (< (- (cdr a) (car a)) (- (cdr b) (car b))))))))
              (push best targets))
          (setq leftovers (1+ leftovers)))))
    (setq targets (delete-dups (nreverse targets)))
    ;; 合并/去重：内层已经有外层要整段重写时，丢掉内层
    (setq targets (seq-filter
                   (lambda (p)
                     (not (seq-some (lambda (q)
                                      (and (not (equal p q))
                                           (<= (car q) (car p)) (>= (cdr q) (cdr p))
                                           (> (- (cdr q) (car q)) (- (cdr p) (car p)))))
                                    targets)))
                   targets))
    (let ((new line))
      (dolist (p (sort targets (lambda (a b) (> (car a) (car b)))))
        (let* ((open (car p))
               (close (cdr p))
               (marker (aref line open))
               (body (substring line (1+ open) close))
               (repl (if add-spaces
                         (concat (char-to-string marker)
                                 (cdadar/org--space-nest body)
                                 (char-to-string marker))
                       (format "@@latex:%s@@"
                               (cdadar/org--latex-wrap marker
                                                      (cdadar/org--latex-convert body))))))
          (setq new (concat (substring new 0 open) repl (substring new (1+ close))))))
      (cons new (cons (length targets) leftovers)))))

(defun cdadar/org--space-nest (s)
  "Return S with a space inserted around every unsupported marker pair.
`*粗体_下划线_结束*' → `粗体 _下划线_ 结束'：补上空格后 org 就能原生解析嵌套。"
  (let ((out "")
        (i 0)
        (n (length s)))
    (while (< i n)
      (let* ((ch (aref s i))
             (marker (and (memq ch (string-to-list cdadar/org-latex-emphasis-markers)) ch))
             (close (and marker
                         (string-match (regexp-quote (char-to-string ch)) s (1+ i)))))
        (if (and close (> close (1+ i)))
            (let ((inner (cdadar/org--space-nest (substring s (1+ i) close))))
              (setq out (concat out
                                (if (cdadar/org--latex-border-open-p s i) "" " ")
                                (char-to-string ch) inner (char-to-string ch)
                                (if (cdadar/org--latex-border-close-p s close) "" " "))
                    i (1+ close)))
          (setq out (concat out (char-to-string ch))
                i (1+ i)))))
    out))

(defun cdadar/org-nested-emphasis-to-latex (beg end &optional add-spaces)
  "把 org 解析不了的嵌套强调批量改写：默认转 LaTeX，ADD-SPACES 时改补空格。
org 的强调不能随意嵌套：内层标记紧贴汉字时（`*粗体_下划线_结束*'）org 只认外层，
内层标记会被当成普通字符（`_' 还会变成下标），buffer 里看不见、导出也没有。

默认（前缀参数为空）把这种段落改写成导出片段：
  `*粗体_下划线_结束*' → `@@latex:\\textbf{粗体\\underline{下划线}结束}@@'
必须用 `@@latex:...@@' 而不是裸的 `\\textbf{}'：editor 会把纯文本里的 `{' `}'
转义成 `\\{ / \\}'（实测 LaTeX 导出生成了 `\\textbf\\{...\\}'），只有导出片段才原样
直通。代价：PDF/LaTeX 后端才有嵌套效果；buffer 里看到的是片段原码；**HTML 等其它后端
会整段丢弃**（实测 `A @@latex:x@@ B' 导出成 `A  B'）。

带前缀参数（ADD-SPACES 非 nil）时走另一条路：只在内层标记两侧补空格，
`*粗体 _下划线_ 结束*' —— org 原生嵌套，buffer / HTML / PDF 都有样式，代价是多两个空格。

两种模式都只处理「外层解析成功、内层没解析成对象」的单行强调；能正常嵌套的不动。
只动 BEG..END（选区或整个 buffer）内的强调，处理的标记见
`cdadar/org-latex-emphasis-markers'。整条命令算一次 undo，可反复跑。返回改写处数。"
  (interactive
   (list (if (use-region-p) (region-beginning) (point-min))
         (if (use-region-p) (region-end) (point-max))
         current-prefix-arg))
  (unless (derived-mode-p 'org-mode)
    (user-error "只在 Org buffer 里用"))
  (let ((end (copy-marker end t))
        (edits nil)
        (count 0)
        (leftovers 0))
    (atomic-change-group
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let* ((line-beg (line-beginning-position))
                 (line-end (line-end-position))
                 (line (buffer-substring-no-properties line-beg line-end)))
            (unless (or (string-match-p "\\`[[:space:]]*\\'" line)
                        (cdadar/org--emphasis-raw-context-p line-beg))
              (let* ((result (cdadar/org--rewrite-line line add-spaces))
                     (new (car result))
                     (places (cadr result)))
                (when (> places 0)
                  (push (cons line-beg line-end) edits)
                  (setq count (+ count places)))
                (setq leftovers (+ leftovers (cddr result)))
                (unless (string= new line)
                  (delete-region line-beg line-end)
                  (goto-char line-beg)
                  (insert new)))))
          (forward-line 1))))
    (set-marker end nil)
    (message "嵌套强调：%d 处%s%s"
             count
             (cond ((zerop count) "（没有需要转换的）")
                   (add-spaces "，已补空格（org 原生嵌套，全后端可用）")
                   (t "，已改成 @@latex:@@ 片段（仅 LaTeX/PDF 后端）"))
             (if (> leftovers 0)
                 (format "；另有 %d 个标记没有可用的外层，先跑 C-c C-x F 补边界" leftovers)
               ""))
    count))
;; === Markdown → Org from clipboard ===
(defun cdadar/md-clipboard-to-org (&optional insert-p)
  "Convert markdown text to Org, replacing the clipboard with the result.

Source of the markdown:
- If the region is active, the region's text.
- Otherwise, the clipboard (pasteboard on macOS).

The converted Org text replaces the clipboard (and primary selection);
with a prefix argument (or INSERT-P non-nil) it is also inserted at
point in the current buffer.

Uses pandoc:  pandoc -f markdown -t org --wrap=preserve
When org-pandoc-import is installed, its `_simple-headers.lua' filter is
reused to drop pandoc's CUSTOM_ID properties and rewrite internal links."
  (interactive "P")
  (unless (executable-find "pandoc")
    (user-error "pandoc is not installed (brew install pandoc)"))
  (let* ((opi-el (locate-library "org-pandoc-import"))
         (simple-headers (and opi-el
                              (expand-file-name "filters/_simple-headers.lua"
                                                (file-name-directory opi-el))))
         (pandoc-args (append (list "-f" "markdown" "-t" "org" "--wrap=preserve")
                              (when (and simple-headers (file-exists-p simple-headers))
                                (list "--lua-filter" simple-headers))))
         (input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (gui-get-selection 'CLIPBOARD)))
         (org-text nil))
    (unless (and input (string-match-p "[^[:space:]]" input))
      (user-error "No markdown text found in region or clipboard"))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (unless (zerop (apply #'call-process-region (point-min) (point-max)
                            "pandoc" t t nil pandoc-args))
        (error "pandoc conversion failed"))
      (setq org-text (string-trim-right (buffer-string) "\n")))
    (if (display-graphic-p)
        (progn
          (gui-set-selection 'CLIPBOARD org-text)
          (gui-set-selection 'PRIMARY org-text))
      (kill-new org-text))
    (when insert-p
      (insert org-text))
    (message "markdown → org: %d chars, clipboard updated" (length org-text))))

(provide 'init-org-text-tools)
;;; init-org-text-tools.el ends here
