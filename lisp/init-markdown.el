;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom cdadar/markdown-pdf-cjk-font "PingFang SC"
  "CJK font used when exporting markdown to PDF (pandoc xelatex engine)."
  :type 'string
  :group 'markdown)

(defconst cdadar/markdown-pdf-header-tex
  "% Pandoc LaTeX header for markdown-export-pdf.
% Keeps long tables inside the page: xltabular X columns wrap long paths and
% code spans instead of letting longtable cells overlap their neighbours.
\\usepackage{xurl}
\\usepackage{fvextra}
\\usepackage{xltabular}
\\usepackage{booktabs}
\\DefineVerbatimEnvironment{verbatim}{Verbatim}{breaklines,breakanywhere}
\\DefineVerbatimEnvironment{Verbatim}{Verbatim}{breaklines,breakanywhere}
\\usepackage{etoolbox}
\\AtBeginEnvironment{xltabular}{\\footnotesize}
\\setlength{\\tabcolsep}{3pt}
\\setlength{\\emergencystretch}{10em}
\\tolerance=2000
\\sloppy
\\renewcommand{\\arraystretch}{1.2}
"
  "LaTeX header passed to pandoc -H.")

(defconst cdadar/markdown-pdf-table-lua
  "-- Replace pandoc's equal-width longtable with xltabular X columns.
-- X columns wrap long paths/tokens instead of overflowing into neighbours.
local function cell_len(cell)
  return #pandoc.utils.stringify(cell)
end

local function scan(rows, maxlens)
  for _, row in ipairs(rows) do
    for i, cell in ipairs(row.cells) do
      local l = cell_len(cell)
      if i <= #maxlens and l > maxlens[i] then maxlens[i] = l end
    end
  end
end

local function cell_tex(cell)
  local s = pandoc.write(pandoc.Pandoc(cell.contents), 'latex')
  s = s:gsub('^%s+', ''):gsub('%s+$', ''):gsub('\\n', ' ')
  return s
end

local function row_tex(row)
  local cells = {}
  for _, cell in ipairs(row.cells) do
    table.insert(cells, cell_tex(cell))
  end
  return table.concat(cells, ' & ') .. ' \\\\\\\\'
end

function Table(t)
  if not FORMAT:match('latex') then return t end
  local n = #t.colspecs
  local maxlens = {}
  for i = 1, n do maxlens[i] = 0 end
  scan(t.head.rows, maxlens)
  for _, body in ipairs(t.bodies) do scan(body.body, maxlens) end

  local weights, total = {}, 0
  for i = 1, n do
    weights[i] = math.sqrt(math.max(maxlens[i], 4))
    total = total + weights[i]
  end
  local w, sumw = {}, 0
  for i = 1, n do
    w[i] = math.min(math.max(weights[i] / total, 0.08), 0.55)
    sumw = sumw + w[i]
  end

  local cols = {}
  for i = 1, n do
    local hs = w[i] / sumw * n
    table.insert(cols, string.format(
      '>{\\\\hsize=%.4f\\\\hsize\\\\raggedright\\\\arraybackslash}X', hs))
  end

  local lines = { '\\\\begin{xltabular}{\\\\linewidth}{@{}' .. table.concat(cols) .. '@{}}' }
  table.insert(lines, '\\\\toprule')
  for _, r in ipairs(t.head.rows) do table.insert(lines, row_tex(r)) end
  table.insert(lines, '\\\\midrule\\\\endhead')
  for _, body in ipairs(t.bodies) do
    for _, r in ipairs(body.body) do table.insert(lines, row_tex(r)) end
  end
  table.insert(lines, '\\\\bottomrule\\\\end{xltabular}')
  return pandoc.RawBlock('latex', table.concat(lines, '\\n'))
end

-- URL-like code spans may break at / . - ; a path containing the | delimiter
-- or LaTeX specials falls through to pandoc's normal \\texttt.
function Code(el)
  if FORMAT:match('latex') then
    if el.text:find('[|{}%%\\\\]') then
      return nil
    end
    return pandoc.RawInline('latex', '\\\\path|' .. el.text .. '|')
  end
end
"
  "Pandoc Lua filter that rebalances tables to xltabular X columns.")

(defun cdadar/markdown-export-pdf ()
  "Export the current markdown buffer to a PDF next to the file, then open it.

The PDF is opened by its default handler (`open' on macOS, i.e. Preview),
not by `browse-url-of-file', which would hand it to the web browser.

Pandoc renders through xelatex.  An inline header and Lua filter replace
pandoc's equal-width longtable with xltabular X columns, so long paths and
version strings wrap inside their cell instead of overlapping the next one."
  (interactive)
  (let* ((in (buffer-file-name))
         (out (and in (concat (file-name-sans-extension in) ".pdf")))
         (header (make-temp-file "markdown-pdf-" nil ".tex"))
         (filter (make-temp-file "markdown-pdf-" nil ".lua")))
    (unless in
      (user-error "Buffer is not visiting a file"))
    (unless (executable-find "pandoc")
      (user-error "pandoc not found in `exec-path'"))
    (unless (executable-find "xelatex")
      (user-error "xelatex not found in `exec-path'"))
    (unwind-protect
        (progn
          (with-temp-file header
            (insert cdadar/markdown-pdf-header-tex))
          (with-temp-file filter
            (insert cdadar/markdown-pdf-table-lua))
          (with-temp-buffer
            (unless (zerop (call-process "pandoc" nil t nil
                                         in "-o" out
                                         "--pdf-engine=xelatex"
                                         "-V" (concat "CJKmainfont="
                                                      cdadar/markdown-pdf-cjk-font)
                                         "-V" "geometry:margin=12mm"
                                         "--columns=200"
                                         "-H" header
                                         "--lua-filter" filter))
              (user-error "PDF export failed:\n%s" (buffer-string))))
          (unless (file-exists-p out)
            (user-error "PDF export failed, no output at %s" out))
          (message "Exported %s" out)
          (call-process "open" nil 0 nil out))
      (delete-file header)
      (delete-file filter))))

(use-package markdown-mode
  :bind (:map markdown-mode-command-map
         ("P" . cdadar/markdown-export-pdf))
  :config
  (setq markdown-command "pandoc")
  (add-auto-mode 'markdown-mode "\\.md\\..markdown\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
