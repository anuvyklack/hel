# Keybindings Reference

> **Note:** You don't strictly need this file. Emacs has an absolutely wonderful help system that neither Vim nor Helix has anything close to. It is self-documented.
>
> In Emacs, keys are bound to commands. You can always place cursor on any command you're interested in and press `:` (or `M-x`), then type `helpful-at-point` to get detailed information.
>
> Use `C-h k` or `F1 k` to describe what a key does.

## Normal State

### Basic Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `h`, `j`, `k`, `l` | Arrow motions | Move left, down, up, right |
| `←`, `↓`, `↑`, `→` | Arrow keys | Alternative arrow navigation |
| `w` | Forward word start | Move to next word start |
| `W` | Forward WORD start | Move to next WORD start (whitespace-separated) |
| `b` | Backward word start | Move to previous word start |
| `B` | Backward WORD start | Move to previous WORD start |
| `e` | Forward word end | Move to next word end |
| `E` | Forward WORD end | Move to next WORD end |

### Line Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `gs` | Beginning of line | Move to start of current line |
| `gh` | First non-blank | Move to first non-whitespace character |
| `gl` | End of line | Move to end of current line |
| `gg` | Beginning of buffer | Jump to first line |
| `G` | End of buffer | Jump to last line |

### Find/Till Character

| Key | Command | Description |
|-----|---------|-------------|
| `f` | Find forward | Find next occurrence of char (with hints) |
| `F` | Find backward | Find previous occurrence of char |
| `t` | Till forward | Move before next occurrence of char |
| `T` | Till backward | Move before previous occurrence of char |
| `n` | Repeat find | Repeat last find/till forward |
| `N` | Repeat find reverse | Repeat last find/till backward |

> **Tip:** After `f`/`F`/`t`/`T`, hints appear at each target. You can press `n`/`N` to repeat the motion while hints are active.

### Paragraph & Structure Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `}` or `]p` | Next paragraph | Move to start of next paragraph |
| `{` or `[p` | Previous paragraph | Move to start of previous paragraph |
| `]f` | Next function | Mark from point to next function end |
| `[f` | Previous function | Mark from point to previous function start |
| `]s` or `].` | Next sentence | Mark from point to next sentence end |
| `[s` or `[.` | Previous sentence | Mark from point to previous sentence start |
| `]e` | Next error | Jump to next compilation/diagnostic error |
| `[e` | Previous error | Jump to previous compilation/diagnostic error |

### Easymotion / Avy

| Key | Command | Description |
|-----|---------|-------------|
| `gw` | Avy word forward | Show hints for words after point |
| `gb` | Avy word backward | Show hints for words before point |
| `gW` | Avy WORD forward | Show hints for WORDs after point |
| `gB` | Avy WORD backward | Show hints for WORDs before point |
| `gj` | Avy line down | Show hints for lines below |
| `gk` | Avy line up | Show hints for lines above |

## Editing Commands

### Insert Mode Entry

| Key | Command | Description |
|-----|---------|-------------|
| `i` | Insert | Enter Insert state before selection |
| `a` | Append | Enter Insert state after selection |
| `I` | Insert line | Enter Insert state at line start |
| `A` | Append line | Enter Insert state at line end |
| `o` | Open below | Open new line below and enter Insert state |
| `O` | Open above | Open new line above and enter Insert state |
| `c` | Change | Delete selection and enter Insert state |

### Deletion & Modification

| Key | Command | Description |
|-----|---------|-------------|
| `d` | Cut | Delete selection to kill-ring |
| `D` | Delete | Delete selection without kill-ring |
| `J` | Join lines | Join selected lines |
| `<` | Indent left | Decrease indentation |
| `>` | Indent right | Increase indentation |
| `=` | Indent region | Auto-indent selection |

### Case Manipulation

| Key | Command | Description |
|-----|---------|-------------|
| `` ` `` or `gu` | Lowercase | Convert selection to lowercase |
| `` M-` `` or `gU` | Uppercase | Convert selection to uppercase |
| `~` | Invert case | Toggle case of characters |

### Undo & Redo

| Key | Command | Description |
|-----|---------|-------------|
| `u` | Undo | Undo last change |
| `U` | Redo | Redo last undone change |

### Copy & Paste

| Key | Command | Description |
|-----|---------|-------------|
| `y` | Copy | Copy selection to kill-ring |
| `p` | Paste after | Paste after selection |
| `P` | Paste before | Paste before selection |
| `R` | Replace | Replace selection with kill-ring content |
| `C-p` | Paste pop | Cycle to previous kill-ring entry |
| `C-n` | Paste pop reverse | Cycle to next kill-ring entry |

### Line Manipulation

| Key | Command | Description |
|-----|---------|-------------|
| `]SPC` | Insert line below | Add blank line below |
| `[SPC` | Insert line above | Add blank line above |

## Selection Commands

### Selection Control

| Key | Command | Description |
|-----|---------|-------------|
| `ESC` | Exit/deactivate | Exit extend mode or deactivate selection |
| `v` | Extend selection | Toggle selection extension mode |
| `;` | Collapse selection | Collapse to single cursor |
| `M-;` or `g;` | Exchange point/mark | Swap cursor and mark positions |
| `x` | Expand line | Expand to full lines downward |
| `X` | Expand line up | Expand to full lines upward |
| `%` | Select all | Select entire buffer |
| `_` | Trim whitespace | Trim whitespace from selection ends |

### Multiple Cursors

| Key | Command | Description |
|-----|---------|-------------|
| `M-<mouse-1>` | Add cursor | Create cursor on click |
| `C` | Copy down | Copy cursor/selection down |
| `M-c` | Copy up | Copy cursor/selection up |
| `s` | Select regex | Create cursors for regex matches |
| `S` | Split on regex | Split selections on regex |
| `M-s` | Split on newline | Split selections on line breaks |
| `K` | Keep selections | Keep only selections matching regex |
| `M-K` | Remove selections | Remove selections matching regex |
| `,` | Remove fake cursors | Delete all secondary cursors |
| `M-,` | Remove main cursor | Make first fake cursor the main one |
| `M--` | Merge selections | Merge all cursors into one selection |
| `(` | Rotate backward | Rotate main selection backward |
| `)` | Rotate forward | Rotate main selection forward |
| `M-(` | Rotate content back | Rotate selections' content backward |
| `M-)` | Rotate content forward | Rotate selections' content forward |
| `&` | Align selections | Align selections vertically |

## Text Objects (Mark Commands)

All text object commands start with `m` (for "mark"). They accept numeric prefixes: `2mip` selects 2 paragraphs.

### Word Objects

| Key | Command | Description |
|-----|---------|-------------|
| `mw` or `miw` | Inner word | Select current word |
| `maw` | A word | Select word with surrounding space |
| `mW` or `miW` | Inner WORD | Select current WORD |
| `maW` | A WORD | Select WORD with surrounding space |

### Sentence & Paragraph

| Key | Command | Description |
|-----|---------|-------------|
| `m.` or `mis` | Inner sentence | Select current sentence |
| `mas` | A sentence | Select sentence with space |
| `mp` or `mip` | Inner paragraph | Select current paragraph |
| `map` | A paragraph | Select paragraph with space |

### Function Object

| Key | Command | Description |
|-----|---------|-------------|
| `mf` or `mif` | Inner function | Select function body |
| `maf` | A function | Select entire function |

### Quoted Strings

| Key | Command | Description |
|-----|---------|-------------|
| `m"` or `mi"` | Inner double quotes | Select inside `"..."` |
| `ma"` | A double quotes | Select including `"..."` |
| `m'` or `mi'` | Inner single quotes | Select inside `'...'` |
| `ma'` | A single quotes | Select including `'...'` |
| ``m` `` or ``mi` `` | Inner backticks | Select inside `` `...` `` |
| ``ma` `` | A backticks | Select including `` `...` `` |

### Brackets & Parentheses

| Key | Command | Description |
|-----|---------|-------------|
| `m(` or `mi(` | Inner parens | Select inside `(...)` |
| `ma(` | A parens | Select including `(...)` |
| `m[` or `mi[` | Inner brackets | Select inside `[...]` |
| `ma[` | A brackets | Select including `[...]` |
| `m{` or `mi{` | Inner braces | Select inside `{...}` |
| `ma{` | A braces | Select including `{...}` |
| `m<` or `mi<` | Inner angles | Select inside `<...>` |
| `ma<` | A angles | Select including `<...>` |

### Special Characters

For `!`, `@`, `#`, `$`, `%`, `^`, `&`, `*`, `~`, `=`, `_`:

| Key | Command | Description |
|-----|---------|-------------|
| `m<char>` or `mi<char>` | Inner | Select between `<char>...<char>` |
| `ma<char>` | Around | Select including `<char>...<char>` |

### Surround Commands

| Key | Command | Description |
|-----|---------|-------------|
| `mm` | Jump to match | Jump to matching bracket |
| `ms` | Surround | Add surrounding delimiters |
| `md` | Delete surround | Remove surrounding delimiters |
| `mr` or `mc` | Change surround | Replace surrounding delimiters |

## Search Commands

| Key | Command | Description |
|-----|---------|-------------|
| `/` | Search forward | Search forward with regex |
| `?` | Search backward | Search backward with regex |
| `*` | Pattern from selection | Create search from selections (with word boundaries) |
| `M-*` | Pattern no bounds | Create search from selections (no boundaries) |
| `n` | Next match | Jump to next search match |
| `N` | Previous match | Jump to previous search match |

> **Note:** When searching backward with `?`, `n` and `N` are swapped to match Vim behavior.

## Scrolling Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-u` | Scroll up | Scroll up half page (or N lines) |
| `C-d` | Scroll down | Scroll down half page (or N lines) |
| `C-b` | Page up | Scroll up full page |
| `C-f` | Page down | Scroll down full page |
| `C-y` | Line up | Scroll up by lines |
| `C-e` | Line down | Scroll down by lines |
| `zz` | Center/eye level | Scroll line to eye level (or center if configured) |
| `zt` | Top | Scroll line to top |
| `zb` | Bottom | Scroll line to bottom |

> **Tip:** Use numeric prefix with `C-u`/`C-d` to set scroll amount: `5 C-d` scrolls 5 lines. Use `0 C-d` to reset to half-page.

## Window Management (`C-w` prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `C-w s` | Split horizontal | Split window horizontally |
| `C-w v` | Split vertical | Split window vertically |
| `C-w d` or `C-w c` | Close window | Delete current window |
| `C-w o` | Only window | Close other windows |
| `C-w h/j/k/l` | Navigate | Move to window in direction |
| `C-w H/J/K/L` | Move window | Move window in direction |
| `C-w w` | Other window | Cycle to next window |
| `C-w p` | Pin window | Toggle window dedication |

### Buffer Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-w r` | Revert buffer | Reload buffer from disk |
| `C-w d` | Kill buffer | Close current buffer |
| `C-w q` | Kill buffer & window | Close buffer and window |
| `C-w z` | Bury buffer | Send buffer to end of list |
| `C-w b` | Clone buffer | Open indirect clone in other window |
| `C-w B` | Clone same window | Open indirect clone in same window |
| `C-w x` | Scratch buffer | Open scratch buffer |
| `]b` / `[b` | Next/previous buffer | Switch buffers |

## Navigation & Marks

| Key | Command | Description |
|-----|---------|-------------|
| `C-s` | Save point | Save position to mark ring |
| `C-o` | Jump back | Jump to previous position |
| `C-i` | Jump forward | Jump to next position |
| `C-S-o` | Global jump back | Jump to previous position (global) |
| `C-S-i` | Global jump forward | Jump to next position (global) |

## Xref (Go to Definition)

| Key | Command | Description |
|-----|---------|-------------|
| `gd` | Go to definition | Jump to symbol definition |
| `gD` | Find references | Find all references to symbol |
| `C-w gd` | Definition other window | Open definition in other window |
| `]x` | Next xref | Jump to next xref position |
| `[x` | Previous xref | Jump to previous xref position |

## Miscellaneous Commands

| Key | Command | Description |
|-----|---------|-------------|
| `:` | Command palette | Execute command (`M-x`) |
| `.` | Repeat | Repeat last command |
| `gc` | Comment | Toggle comment on selection |
| `gi` | Imenu | Navigate buffer structure |
| `ga` | Describe char | Show character information |
| `gf` | Find file at point | Open file path under cursor |
| `gx` | Browse URL | Open URL under cursor |
| `gq` | Fill region | Reflow text in selection |
| `gQ` | Fill as paragraph | Reflow as single paragraph |
| `zx` or `Z Z` | Save buffer | Save current buffer |
| `zn` | Narrow | Narrow buffer to selection |
| `zw` | Widen | Remove narrowing |

## Motion State

Motion state is used in special buffers (help, grep, etc.). It includes:

- Basic scrolling commands
- `:` for command palette
- `SPC` for leader key (if configured)
- `C-w` for window commands
- `]b` / `[b` for buffer switching
- `i` to enter Normal state (for text selection)
- Use `zx` or `C-x C-s` to return to Motion state

## Insert State

| Key | Command | Description |
|-----|---------|-------------|
| `ESC` | Normal state | Return to Normal state |
| `C-w` | Delete word backward | Delete previous word |

In Insert state, all normal Emacs keybindings are available, and your configured input methods work.

## Universal Argument

Since `C-u` is used for scrolling, the universal argument is rebound:

- `M-u` — Start universal argument (instead of `C-u`)
- `M-u M-u` — Continue universal argument

## Tips

1. **Help system:** Press `C-h k` followed by any key to see what it does.
2. **Command help:** Use `:` then type `helpful-at-point` with cursor on a command name.
3. **Customize:** All keybindings can be customized using `hel-keymap-set` and related functions.
4. **Practice:** The selection-based model takes time to learn but becomes very powerful once internalized.
