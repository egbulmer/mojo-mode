# mojo-mode

An Emacs major mode for the [Mojo](https://mojolang.org/) programming language.

It loads, associates Mojo files, highlights the current language,
indents Mojo blocks, and can start the Mojo language server.  A REPL,
formatter, and compile commands are not implemented yet.

Keyword lists follow the [Mojo 1.1 language reference](https://mojolang.org/docs/reference/keywords/).
Removed spellings (`fn`, `let`, `alias`, `owned`, and the rest) are not
treated as keywords.

## Status

| Piece | State |
| --- | --- |
| `.mojo` and `.🔥` file association | done |
| `#!/usr/bin/env mojo` shebang | done |
| Syntax table (comments, strings, backticks) | done |
| Font-lock for keywords, declarations, builtins | done |
| Imenu for `struct`, `trait`, `def`, `comptime` | done |
| Eglot registration for `mojo-lsp-server` | done |
| Block-aware indentation | done (`mojo-indent.el`) |
| `mojo run` / `mojo build` / `mojo format` | not yet |
| REPL | not yet |

## Requirements

- Emacs 30.1 or newer
- A [Mojo SDK](https://mojolang.org/install/) on `PATH`, only if you want
  the language server.  Editing and highlighting work without it.

## Installation

The package is not on a package archive yet.  Load it from a checkout:

```elisp
(add-to-list 'load-path "/path/to/mojo-mode")
(require 'mojo-mode)
```

With `use-package`:

```elisp
(use-package mojo-mode
  :load-path "/path/to/mojo-mode"
  :hook (mojo-mode . eglot-ensure))
```

`mojo-lsp-server` ships with the Mojo SDK.  The mode registers it with
Eglot but does not start it; add the hook above if you want it started
automatically.

## Configuration

```elisp
(setq mojo-indent-offset 4)
(setq mojo-lsp-server-command '("mojo-lsp-server"))
```

## Development

Tests use [Cask](https://github.com/cask/cask) and Buttercup.

```bash
make install   # cask install
make test      # buttercup
make lint      # byte-compile with warnings as errors; no Cask needed
```

## License

GPL-3.0-or-later.  See [LICENSE](LICENSE).

`mojo-indent.el` is a port of the indentation engine from GNU Emacs's
`python.el`, which is Copyright (C) 2013-2026 Free Software Foundation,
Inc., written by Fabián Ezequiel Gallina.  It was adapted from the
`python.el` shipped with GNU Emacs 30.2.
