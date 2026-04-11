# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration based on [Purcell's emacs.d](https://github.com/purcell/emacs.d). The configuration is modular, with each feature separated into its own file in the `lisp/` directory.

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
emacs -Q --l ~/.emacs.d/init-mini.el
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
- Configured with optimized settings: 1MB read-process-output-max, autoshutdown enabled

Major language configurations available:
- Go: `init-golang.el`
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
- Auto-downloads PlantUML and ditaa jars for diagram support
- Configured for various export formats

## Common Modifications

When editing this configuration:

1. **Adding a new package**: Use `use-package` form in the relevant `init-*.el` file
2. **New language support**: Create `lisp/init-{language}.el` and require it in `init.el`
3. **Keybindings**: Use `:bind` in use-package or `define-key` in config sections
4. **Performance tuning**: Adjust gcmh settings in `init.el` or `early-init.el`

## Important Notes

- Spell checking is disabled by default (`*spell-check-support-enabled*` is nil)
- Server mode starts automatically after initialization
- Custom file is separate (`custom.el`) to avoid polluting init files
- The config supports but doesn't require native compilation (configured in `early-init.el`)
