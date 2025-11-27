# Hel — Helix Emulation Layer for Emacs

Hel is a [Helix](https://helix-editor.com/) emulation layer for Emacs that brings selection-based modal editing with multiple cursors to the Emacs ecosystem.

## Key Features

- **Multiple cursors based modal editing** — Full-featured multiple cursors that work seamlessly with Helix-style commands
- **Smart undo/redo** — Undo system designed to play well with multiple cursors
- **PCRE regexps** — Perl-Compatible Regular Expressions by default (powered by [pcre2el](https://github.com/joddie/pcre2el))
- **Smooth scrolling** — Built-in smooth scrolling commands out of the box
- **Keypad mode** — Built-in Meow Keypad / God-mode like functionality for executing Emacs commands without modifiers

## Documentation

- [Customization Guide](docs/customization.md)
- [Keybindings Reference](docs/keybindings.md)
- [Multiple Cursors Guide](docs/multiple-cursors.md)
- [Extensions](docs/extensions.md)

## Installation

Hel is not yet on MELPA. You can install it directly from GitHub using Emacs'
built-in package manager:

```elisp
;; Install dependencies
(use-package dash :ensure t)
(use-package avy :ensure t)
(use-package pcre2el :ensure t)

;; Install Hel
(use-package hel
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :config
  (hel-mode 1))
```

## Can I use Hel without knowing Emacs keys?

> **Terminology note:** What Vim, Helix, and other modal editors call Normal and Insert *modes*, Hel refers to as *states*. This is because the word "mode" in Emacs is already used for its [major](https://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html) and [minor](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html) modes.

**Yes!** When I came to Emacs from Neovim several years ago, I loved Vim's editing model but found Emacs native keybindings cumbersome and had zero interest in learning them. I wanted Emacs not as a text editor (which it obviously lacks) but as an operating system with Lisp and all its power.

I asked myself: "Can I use modal editing in Emacs without learning Emacs keys?"

The answer is yes. I have never used — and still don't know — most Emacs keys. I only know a few that you need when something breaks early during Emacs startup and you don't have your Hel keys available:

- `M-x` — Command palette. The main key you need; all other commands can be invoked from it.
- `C-x C-s` — Save current buffer.
- `C-x C-c` — Exit Emacs.

That's it.

Hel and Emacs don't interfere much because Emacs is not a modal editor: letters and numbers are self-inserting, and most command key chords begin with `C-x` or `C-c`. Due to this, Helix emulation works as a layer on top of Emacs.

In Normal state you have selection-based editing, multiple cursors, and all the other Hel features. In Insert state, Hel steps aside and standard Emacs keys work as usual. Also, Hel doesn't touch `C-x` and `C-c`, so they are always available. This allows you to mix Hel and Emacs in any proportion.

## States Overview

### Normal State

In Normal state you get Helix text editing functionality with all the powerful selection-based commands and multiple cursor support.

See: [Normal state keybindings](docs/keybindings.md#normal-state)

### Insert State

In Insert state, Hel doesn't bind any keys by default, so you get the standard Emacs editing experience. Your configured input methods work here.

### Motion State

Motion state is the default state for special buffers: grep results, error lists, file managers — anything not intended for text editing. A simple heuristic is used: if any letter key is not self-inserting, Hel enables Motion state; otherwise, it uses Normal state.

By default, Motion state binds only essential navigation keys:

- `:` — Command palette
- `SPC` — Leader key (if hel-leader extension is loaded)
- `C-w` — Window and buffer manipulation prefix
- `[b` / `]b` — Switch buffers
- Scrolling commands (`C-f`, `C-b`, `C-d`, `C-u`, `zt`, `zb`, `zz`)
- `M-u` — `universal-argument` (since `C-u` is used for scrolling)
- `i` — Switch to Normal state (to select and copy text). Use `zx` or `C-x C-s` to switch back.

## Extensions

### Leader (hel-leader)

```elisp
(use-package hel-leader
  :after hel)
```

This extension provides a keypad state for executing commands without modifier keys, similar to Meow's keypad or God-mode.

Enter keypad state by pressing `SPC` in Normal or Motion state.

**Translation rules:**
- Start with `x`/`h`/`c`/`m`/`g` → begins with `C-x`/`C-h`/`C-c`/`M-`/`C-M-`
- Other keys → start with themselves in the leader keymap
- Following `m` → translates to `M-`
- Following `g` → translates to `C-M-`
- Keys after `m` or `g` → interpreted as `C-<key>`
- `SPC` → literal prefix (no `C-` modifier)
- Fallback: undefined `C-c C-a` → try `C-c a`

**Examples:**

| Input       | Translation      | Explanation                     |
|-------------|------------------|---------------------------------|
| `SPC a`     | `a` in leader map| Default map is `C-c`           |
| `SPC c t t` | `C-c C-t C-t`    | Start with `c` as `C-c`        |
| `SPC x m t` | `C-x M-t`        | `m` as Meta prefix             |
| `SPC g x`   | `C-M-x`          | `g` as Control+Meta prefix     |
| `SPC x SPC p`| `C-x p`         | `SPC` as literal prefix        |

### Paredit (hel-paredit)

```elisp
(use-package paredit :ensure t)
(use-package hel-paredit
  :after hel
  :hook (emacs-lisp-mode . hel-paredit-mode))
```

Integration with Paredit for structured editing of S-expressions. Provides:
- Enhanced word motions (`W`, `B`, `E`) that skip parentheses
- S-expression navigation (`M-n`/`M-p` or `C-l`/`C-h`)
- Tree climbing (`M-o`/`M-i` or `C-k`/`C-j`)
- Slurp/barf commands (`<`, `>`)
- Safe deletion that respects parenthesis balance

### Org Mode (hel-org)

```elisp
(use-package hel-org
  :after (hel org))
```

Integration with Org-mode for navigating and editing structured documents:
- AST navigation with `M-i`, `M-n`, `M-p`, `M-o` (or `C-hjkl`)
- Org-specific text objects (headings, emphasis, sentences)
- Promote/demote with `<` and `>`
- Smart insert commands for headings and list items

## Differences from Helix

This package is not a one-to-one emulation. Some commands are implemented
differently (improved from the author's point of view), and some features
like keyboard macros, registers, and jumplists already have their alternatives in Emacs.

**Key differences:**

- **Cursor position:** In Emacs, the cursor ("point") is located *between* characters rather than *on* a character. I kept this behavior because the primary object of interaction in Helix is the selection, not the cursor itself.

- **Line selection (`x`/`X`):** Reworked to expand/contract line-wise selection down when cursor is at the end, or up when at the beginning.

- **Inner objects shortcut:** Available directly under `m` prefix to reduce keystrokes: `mw` is the same as `miw` (select word).

- **Numeric arguments for marks:** Commands accept counts: `m2ip` or `2mip` selects 2 paragraphs.

- **Selection from position:** `gh`/`gl` make a selection from current position to beginning/end of line (Helix only moves cursor).

- **Enhanced easymotion:** Six commands provided:
  - `gw`/`gb` — Choose and mark word forward/backward
  - `gW`/`gB` — Choose and mark WORD forward/backward
  - `gj`/`gk` — Go to line down/up with hints

- **Vim-style navigation:** `gg`/`G` to go to first/last line (Helix uses `gg`/`ge`).

- **Enhanced find commands:** `f`, `F`, `t`, `T` show hints for targets and can be repeated with `n`/`N`.

- **Smart search direction:** When searching backward with `?`, `n` and `N` are swapped (like Vim).

- **Vim-style scrolling:** Scrolling keybindings are from Vim instead of Helix.

## Commands Not Yet Implemented

- `.` (repeat) — Need to decide what it should repeat
- `r` — Replace character
- `M-u`, `M-U` — Traverse undo tree
- `q`, `Q` — Record keyboard macros (use Emacs macros instead)

## Why "Hel"?

Short, memorable, and contains the word "Helix". Also, in Norse mythology, Hel
is the goddess of the underworld — fitting for something that operates beneath
the surface layer of Emacs.

## Kakoune vs Helix

The main difference in terms of emulation is how they handle expanding
selections: Kakoune uses `Shift` + motions, while Helix has a separate state on
the `v` key. Since I originally came from Vim, I prefer Helix's `v` key, so I
chose Helix. However, Kakoune was the original inventor of selection-based
editing, and this should be remembered.

## Acknowledgments

Hel depends on [dash.el](https://github.com/magnars/dash.el),
[pcre2el](https://github.com/joddie/pcre2el), and
[avy](https://github.com/abo-abo/avy) — wonderful packages.

Hel is heavily inspired by:
- [evil](https://github.com/emacs-evil/evil)
- [multiple-cursors.el](https://github.com/magnars/multiple-cursors.el)
- [meow](https://github.com/meow-edit/meow)
- [surround](https://github.com/mkleehammer/surround)
- [kak.el](https://github.com/aome510/kak.el)
- [doomemacs](https://github.com/doomemacs/doomemacs)
- [crux](https://github.com/bbatsov/crux)

You are welcome to go and give them all a star!

## Contributing

- **Share it** — A quick post about this package on your blog or social network could bring new users to Emacs!
- **Support development** — You can support the development with a donation.

## License

Copyright © 2025 Yuriy Artemyev

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.
