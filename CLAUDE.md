# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

For Codex, OpenCode, and other project-level agents, the parallel project guidance lives in `AGENTS.md`. Keep `CLAUDE.md` and `AGENTS.md` aligned when project conventions change.

## Overview

This is a personal Emacs configuration based on [Purcell's emacs.d](https://github.com/purcell/emacs.d). The configuration is modular, with each feature separated into its own file in the `lisp/` directory.

## Development Principles

These rules are authoritative for all code changes in this repository:

1. 不保留向后兼容。过时的直接删，别加兼容层、别写 migration、别留 fallback。
2. 选能满足当前需求的最简单实现。不要预防性抽象，不要多此一举的配置层。
3. 系统分层长。先跑通一个最小的端到端版本，再往上加东西。绝不为了未完成的复杂度拆掉能跑的东西。
4. 组件保持模块化，关注点分离。
5. 优先用成熟的、有人维护的库。没有明确理由别自己重写。
6. 先翻项目里已有的依赖能做什么，再考虑加新包或自己写。别上来就假设库里没有。
7. 架构决策往长了做。不接受"先这样以后再换"的临时方案。
8. 先看成熟产品怎么解决同一个问题，用已验证的模式，别从零发明。

## Repository Structure

- `init.el` - Main entry point that loads all configuration modules
- `early-init.el` - Pre-initialization (Emacs 29.1+), handles GC, native compilation, and UI optimization
- `lisp/` - Contains 100+ configuration modules (e.g., `init-org.el`, `init-python.el`, `init-ai.el`)
- `site-lisp/` - Local packages not available in package archives
- `elpa-{VERSION}/` - Package installation directory (versioned per Emacs version)
- `snippets/` - YASnippet templates
- `custom.el` - Auto-generated customization settings
- `init-mini.el` - Minimal configuration for fast startup

## Testing Configuration

Test the full configuration startup:
```bash
./test-startup.sh
```

Test with minimal configuration:
```bash
emacs -Q -l ~/.emacs.d/init-mini.el
```

The CI workflow in `.github/workflows/test.yml` automatically tests startup on multiple Emacs versions (29.1-30.2).

## Key Architecture Patterns

### Package Management
- Uses `use-package` for declarative package configuration
- Package directory is version-specific: `elpa-{major}.{minor}/` to prevent bytecode incompatibility
- Packages are installed from MELPA repository
- Helper function `require-package` in `init-elpa.el` for on-demand package installation

### Module Loading
The `init.el` loads configuration modules in a specific order:
1. Core utilities (`init-utils.el`, `init-site-lisp.el`, `init-elpa.el`)
2. Performance tuning (gcmh for garbage collection)
3. Optional local customizations (`init-preload-local.el`)
4. UI and editing (`init-themes.el`, `init-editing-utils.el`, etc.)
5. Version control (`init-git.el`, `init-github.el`)
6. Language-specific modes (Python, JavaScript, Haskell, Go, etc.)
7. User customizations at the end (`init-ai.el`, `init-personal.el`, `init-fonts.el`)

### Customization Strategy
To add personal customizations without modifying core files:
- Create `lisp/init-local.el` for custom configuration
- Create `lisp/init-preload-local.el` for early initialization code
- Both files are loaded conditionally and won't break if missing

### Version-Specific Code
The configuration uses constants to enable/disable features based on Emacs version:
- `emacs/>=29p`, `emacs/>=30p`
- Platform-specific: `*is-a-mac*`, `*linux*`, `*win64*`

## Language Support

LSP integration via `eglot` (configured in `init-lsp.el`):
- Auto-enabled for most programming modes except emacs-lisp, lisp, makefile
- Configured with optimized settings: 4MB read-process-output-max, autoshutdown enabled

Major language configurations available:
- Go: `init-go.el`
- Python: `init-python.el`
- JavaScript/TypeScript: `init-javascript.el`
- Ruby: `init-ruby.el`
- Rust: `init-rust.el`
- Haskell: `init-haskell.el`
- And many others (see full list in `init.el`)

## AI Integration (init-ai.el)

The configuration includes multiple AI coding assistants:

1. **emigo** - AI code assistant
2. **ellama** - Local LLM integration via Ollama (if available)
3. **aider** - AI pair programming (bound to `C-c a`, uses Anthropic Claude Sonnet)
4. **ai-code** - Universal AI backend switcher (supports opencode, claude-code, cursor, etc.)
5. **eca** - Emacs Code Assistant
6. **agent-shell** - AI-powered shell with Evil mode support

When modifying AI-related code:
- API keys are set via environment variables (e.g., `ANTHROPIC_API_KEY`)
- Backend selection uses `ai-code-set-backend` with options: 'codex, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code, 'cursor
- Auto-revert is enabled globally for AI changes to appear automatically

## Org-mode Configuration

The `init-org.el` contains extensive org-mode setup:
- GTD (Getting Things Done) workflow support
- Org capture bound to `C-c c`
- Org clock commands under `C-c o` prefix
- Org buffers soft-wrap long headings and body text with visual wrap markers, without auto-enabling `prose-mode`
- LaTeX export keeps five Org headline levels as real headings (`section` through `subparagraph`) instead of degrading deep headings into enumerate/list items
- LaTeX quote export rewrites `\\` + blank lines inside quote blocks to preserve left alignment in generated PDFs
- Diagram support configures existing PlantUML / ditaa tools from local jars or `PLANTUML_JAR` / `DITAA_JAR`; it does not auto-download them at startup
- Configured for various export formats

## Common Modifications

When editing this configuration:

1. **Adding a new package**: Use `use-package` form in the relevant `init-*.el` file
2. **New language support**: Create `lisp/init-{language}.el` and require it in `init.el`
3. **Keybindings**: Prefer `:bind` for package-owned keymaps; keep `define-key` in `:config` when preserving cross-package/load-order semantics is safer
4. **Package settings**: Move only package-owned `defcustom` variables to `:custom`; keep `defvar` or runtime setup in `:config`
5. **Performance tuning**: Adjust gcmh settings in `init.el` or `early-init.el`

## Refactoring and Verification Conventions

Use conservative, behavior-preserving `use-package` refactors:
- Transform existing settings in place; do not remove user configuration without explicit approval.
- Keep comments unless they are factually wrong or misleading.
- Use the `cdadar/` prefix for newly introduced helper functions; do not rename existing `sanityinc/` helpers unless explicitly requested.
- For small modules, keep helper functions at top level when they are shared, and move package-owned hooks, keymaps, and `defcustom` settings into the relevant `use-package` block.
- Before moving a variable into `:custom`, verify it is a `defcustom` of that package in the installed source.
- After moving `:mode`, `:hook`, or `:bind` declarations, run a targeted batch check for the resulting auto-mode entry, hook, or keymap binding instead of relying on startup alone.
- For **local configuration modules** (not external packages, e.g. `init-treesitter.el`, `init-spelling.el`), keep `when` + `require` style. Do not wrap them in `use-package`; `:ensure nil` still triggers `package-installed-p` checks and can fail at startup.

For each focused cleanup batch, run at least:
```bash
emacs --batch --eval '(progn (with-temp-buffer (insert-file-contents "lisp/init-FOO.el") (emacs-lisp-mode) (check-parens)) (princ "check-parens: OK\n"))'
emacs --batch --eval '(progn (load-file "lisp/init-FOO.el") (princ "load-file: OK\n"))'
emacs --batch -f batch-byte-compile lisp/init-FOO.el
./test-startup.sh
```

## Important Notes

- Spell checking is disabled by default (`*spell-check-support-enabled*` is nil)
- Server mode starts automatically after initialization
- Custom file is separate (`custom.el`) to avoid polluting init files
- The config supports but doesn't require native compilation (configured in `early-init.el`)
