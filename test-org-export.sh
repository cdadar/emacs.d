#!/bin/sh -e
# Org LaTeX 导出等价性门：改 lisp/init-org*.el 与 ox/ox-latex 相关配置时用。
#
# 用法：
#   ./test-org-export.sh OUT                只生成快照
#   ./test-org-export.sh OUT BASELINE       生成快照并与 BASELINE 比对，不一致则失败
#
# 典型用法（拆分/重构前后各跑一次）：
#   ./test-org-export.sh /tmp/org-before
#   ...改动...
#   ./test-org-export.sh /tmp/org-after /tmp/org-before
#
# 只导出 test/org-export-cases/*.org（仓库内合成用例），不读 ~/org-mode，
# 用例专门盖住 final-output 阶段的每个导出过滤器。
#
# 不在 CI 跑：快照里带有本机 org 版本与字体探测的产物，跨机 diff 只会是噪声。
# 这个门证明的是「同一台机器上，改动前后的导出结果一样」。
cd "$(dirname "$0")"
OUT=${1:?usage: $0 OUT [BASELINE]}
BASE=${2:-}

REPO="$PWD" OUT="$OUT" emacs -nw --batch -l test/org-export.el

if [ -n "$BASE" ]; then
    if diff -r "$BASE" "$OUT"; then
        echo "OK: 导出结果与基线 '$BASE' 一致"
    else
        echo "FAIL: 导出结果与基线 '$BASE' 不同（上面是差异）" >&2
        exit 1
    fi
fi
