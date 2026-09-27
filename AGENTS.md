# Paredit

Use the `paredit` CLI or MCP/tools for balancing parentheses.

- Start with `paredit_capabilities` (or `paredit_run` with `inspect capabilities`) when a command is not one of the dedicated tools. Writes need an explicit `--write`; without it they only print.
- Seed or append a whole form with `refactor insert-top-level --with <one complete form>`. Change an existing form with `edit replace --path <child-path> --with <form>`. Both refuse an unbalanced `--with`, so they never save a broken buffer.
- `edit wrap`, `slurp`, and `barf` only reshape a form that already parses. Select it with a child path from `inspect outline` first. A guessed path such as `0.3.0.1` is rejected. They will not repair a file that does not parse.
- When the buffer is already unbalanced, run `inspect check`, then `edit repair-unclosed-lists`. Do not reach for wrap or slurp.
- After edits made outside paredit, run `inspect check`. Use `inspect lint` for logic findings a paren matcher misses, such as a missing `lexical-binding` header. `edit format --write` reindents without touching delimiters.
