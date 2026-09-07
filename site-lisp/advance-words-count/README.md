# advance-words-count.el
Extended `count-words` function

```emacs-lisp 
(require 'advance-words-count) 
``` 

Use `advance-words-count` to display a words count message in minibuffer. If an
universal-argument is used, display the message verbosely.

This package is intended to be a replacement of Emacs's original `count-words`
function. It uses regexp to match words and characters of any language, which
can be set in `words-count-regexp-list`.

You can define your own function to display messages, and set to
`words-count-messages-func` to your perfered function. For an easier way, use
`words-count-define-func` to define rhe function.

Example:
```emacs-lisp
(words-count-define-func " %d %d" ((cadr list) (car list)) t)
```

Set `(setq words-count-messages-display 'pos-tip)` to uses `pos-tip.`

## Output fields

Short form: `Ns:%d, Al:%d, Ln:%d, An:%d, Ha:%d, Wc:%d, Fc:%d` (Fc only shown in org-mode):

| Field | Meaning | Source |
|---|---|---|
| `Ns` | Characters without space | `words-count-rule-nonespace` (`[^[:space:]]`) |
| `Al` | All characters (including space) | `end - start` |
| `Ln` | Number of lines | `count-lines` |
| `An` | ASCII words | `words-count-rule-ansci` |
| `Ha` | CJK characters | `words-count-rule-CJK` (`\cc`) |
| `Wc` | Word count = `Ha` + `An` | |
| `Fc` | Filtered characters (non-space, excluding org headings and `#+` meta lines) | `words-count-filtered-char-count` |

Example: `Ns:9875, Al:11134, Ln:608, An:894, Ha:4960, Wc:5854, Fc:7117` —
`Wc = Ha + An = 4960 + 894 = 5854`; `Al - Ns = 11134 - 9875 = 1259` spaces;
`Fc:7117` counts only non-space characters outside headings and `#+` lines.

In org-mode the filtered dimension can be tuned with the defcustom
`words-count-org-excluded-lines`; outside org-mode `Fc` is omitted.
