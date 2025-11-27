# Multiple Cursors Guide

This guide explains how to use Hel's powerful multiple cursors functionality.

## Table of Contents

- [Introduction](#introduction)
- [Creating Cursors](#creating-cursors)
- [Managing Cursors](#managing-cursors)
- [Editing with Multiple Cursors](#editing-with-multiple-cursors)
- [Advanced Techniques](#advanced-techniques)
- [Troubleshooting](#troubleshooting)

## Introduction

Multiple cursors is one of Hel's most powerful features. Unlike other implementations, Hel's multiple cursors work seamlessly with all text editing commands, including:

- All motion commands
- Text objects
- Surround commands
- Undo/redo (works correctly across all cursors)
- Copy/paste (each cursor has its own kill-ring)

### How It Works

Hel executes commands first for the main (real) cursor, then replays them for each fake cursor. Each cursor maintains its own:

- Position (point and mark)
- Selection state
- Kill ring (clipboard)
- Extended selection state (`v` key)

## Creating Cursors

### Manual Creation

#### Mouse Click (`M-<mouse-1>`)

Hold `Alt` (Meta) and click to add a cursor at the click position.

```
Before:          After Alt-click on second line:
Hello            Hello
World|           World●
Foo              Foo|
```

(`|` = main cursor, `●` = fake cursor)

#### Copy Selection Down/Up (`C` / `M-c`)

Duplicate the current cursor or selection.

```
Before:          After pressing C:
[Hello]          [Hello]
World            [World]
Foo              Foo

Before:          After pressing M-c:
Foo              [Foo]
[World]          [World]
Hello            Hello
```

### Pattern-Based Creation

#### Select Regex (`s`)

Create cursors for all regex matches within current selections.

**Workflow:**
1. Select text (or use `%` to select entire buffer)
2. Press `s`
3. Enter regex pattern
4. Press Enter

```
Before (with %):       After s → "world":
The world is big       The [world] is big
Hello world            Hello [world]
Another world here     Another [world] here
```

**Interactive highlighting:** As you type, matching text is highlighted in real-time.

#### Split on Regex (`S`)

Create cursors for regions that do NOT match the regex (complements).

```
Before (with %%):      After S → "[0-9]+":
Item 123               [Item ]123
Item 456               [Item ]456
Item 789               [Item ]789
```

#### Split on Newlines (`M-s`)

Split each selection at line boundaries.

```
Before:                After M-s:
[Line 1                [Line 1]
Line 2                 [Line 2]
Line 3]                [Line 3]
```

### Selection-Based Creation

Start with one selection, extend it, then split:

1. Select first occurrence: `*` (or `miw` for word at point)
2. Extend to next occurrences: `n` (keep pressing)
3. Split into separate cursors: `M-s`

Or use `s` directly to match a pattern and create cursors.

## Managing Cursors

### Filtering Cursors

#### Keep Matching (`K`)

Keep only cursors whose selection matches a regex.

```
Before:                After K → "important":
[task: routine]        [task: important]
[task: important]      [task: important]
[task: normal]
[task: important]
```

#### Remove Matching (`M-K`)

Remove cursors whose selection matches a regex.

```
Before:                After M-K → "test":
[debug code]           [debug code]
[test code]            [production code]
[production code]
[test validation]
```

### Cursor Navigation

#### Rotate Selection (`(` / `)`)

Cycle which cursor is "main" (has real focus).

- `(` — Rotate main selection backward
- `)` — Rotate main selection forward

Useful for:
- Reviewing each match
- Adjusting specific cursors
- Navigating to specific positions

```
[First]●     Press )     First●
Second●      →           [Second]●
Third●                   Third●
```

#### Remove Cursors (`,` / `M-,`)

- `,` — Remove all fake cursors (keep main)
- `M-,` — Remove main cursor, promote first fake cursor

### Merging & Aligning

#### Merge Selections (`M--`)

Combine all cursors into a single selection from first to last.

```
Before:                After M--:
Hello [world]●         Hello [world
This [is]●             This is
[A] test●              A] test
```

#### Align Selections (`&`)

Vertically align selections by adding spacing.

```
Before:                After &:
[x] = 1                [x]   = 1
[foo] = 2              [foo] = 2
[y] = 3                [y]   = 3
```

### Rotating Content

#### Rotate Content Forward/Backward (`M-)` / `M-(`)

Rotate the content of selections (not the cursors themselves).

```
Before:                After M-):
[One]●                 [Three]●
[Two]●                 [One]●
[Three]●               [Two]●
```

Useful for reordering similar items.

## Editing with Multiple Cursors

### Basic Editing

All normal editing commands work with multiple cursors:

```elisp
;; Type characters
i Hello ESC              ; Insert "Hello" at each cursor

;; Delete
d                        ; Delete all selections

;; Change
c World ESC              ; Replace selections with "World"

;; Paste
y ... p                  ; Copy and paste at multiple locations
```

### Text Objects

Text objects work independently at each cursor:

```
Before:                After miw (select inner word):
hello world            [hello] world
foo bar                [foo] bar
test value             [test] value
```

### Advanced Editing

#### Surround

Apply surround commands to all selections:

```
Before:                After ms ( :
[hello]●               ([hello])●
[world]●               ([world])●
[test]●                ([test])●
```

#### Indentation

Indent all selections at once:

```
Before:                After >:
[line1]●                   [line1]●
[line2]●                   [line2]●
[line3]●                   [line3]●
```

#### Case Changes

Convert case across all cursors:

```
Before:                After gU (uppercase):
[hello]●               [HELLO]●
[world]●               [WORLD]●
[test]●                [TEST]●
```

## Advanced Techniques

### Pattern Matching Workflow

1. **Find pattern**: `/pattern` or `?pattern`
2. **Select all matches**: Create initial selection with `*`, then `n` repeatedly
3. **Create cursors**: `M-s` to split into individual cursors
4. **Edit**: Make changes across all instances
5. **Verify**: Use `)` to cycle through and inspect

### Selective Editing

1. **Create many cursors**: Use `s` with broad pattern
2. **Filter down**: Use `K` or `M-K` to keep/remove specific ones
3. **Edit**: Make changes to filtered set
4. **Clean up**: `,` to remove cursors when done

### Incremental Numbers

Since each cursor executes commands independently, you can't directly insert incremental numbers. However, you can:

1. Use keyboard macros (Emacs built-in)
2. Use `rectangle-number-lines` for rectangles
3. Write a custom command with `hel-define-command`

Example custom command:

```elisp
(hel-define-command my-insert-numbers ()
  "Insert incremental numbers at each cursor."
  :multiple-cursors nil
  (interactive)
  (let ((n 0))
    (hel-with-each-cursor
      (insert (number-to-string (cl-incf n))))))
```

### Rectangular Editing

1. Use `C` or `M-c` to create cursors vertically:
   ```
   Before:          After C C C:
   line1|           line1|
   line2            line2●
   line3            line3●
   line4            line4●
   ```

2. Edit as normal:
   ```
   After i // ESC:
   //line1
   //line2
   //line3
   //line4
   ```

### Cross-Buffer Editing

Cursors are buffer-local, but you can:

1. Edit multiple locations in one buffer
2. Copy text
3. Switch buffers
4. Paste at multiple locations in new buffer

### Working with Narrowing

When using `zn` (narrow to region) with multiple cursors:

```elisp
;; Narrow to first selection
zn
;; Edit within narrow region
;; Widen back
zw
```

Cursors outside the narrow region are preserved.

## Troubleshooting

### Commands Not Working

Some commands don't support multiple cursors. When you use an unsupported command, Hel will ask:

```
Do <command> for all cursors? (y or n)
```

- `y` — Execute for all cursors (saved to whitelist)
- `n` — Execute only for main cursor (saved to blacklist)

Your preference is saved to `hel-whitelist-file`.

### Emacs Slowing Down

If Emacs becomes slow with many cursors:

1. **Set a limit:**
   ```elisp
   (setq hel-max-cursors-number 100)
   ```

2. **Use filtering:** Instead of creating 1000 cursors, create more and filter down.

3. **Disable minor modes:** Some modes don't work well with multiple cursors. Add to:
   ```elisp
   (add-to-list 'hel-minor-modes-incompatible-with-multiple-cursors
                'my-slow-mode)
   ```

### Undo Issues

Hel's undo is designed for multiple cursors:

- `u` undoes the last change across all cursors as a single unit
- Individual cursor edits are grouped together
- Undo state is preserved correctly

If you encounter issues, try:

```elisp
;; Reset undo
(setq buffer-undo-list nil)
```

### Misaligned Cursors

If cursors become misaligned after editing:

1. Check that the command supports multiple cursors
2. Use `)` to rotate through and inspect each cursor
3. Use `,` to remove all and start over
4. Report the command that caused issues

### Copy/Paste Confusion

Each cursor has its own kill-ring. When pasting:

- `y` copies to each cursor's kill-ring
- `p` pastes from each cursor's kill-ring
- To paste the same text everywhere, copy without multiple cursors active

### Performance Tips

1. **Use specific patterns:** Instead of selecting everything, select only what you need
2. **Filter early:** Use `K` and `M-K` to reduce cursor count
3. **Avoid complex commands:** Some commands are slow with many cursors
4. **Close unused cursors:** Use `,` when done editing

## Examples

### Example 1: Commenting Multiple Lines

```
1. Select lines with `x` (multiple times or with count)
2. Press `gc` to toggle comments
```

### Example 2: Surrounding Words with Quotes

```
1. Create cursors on words: `%` then `s` → `\bword\b`
2. Press `ms"` to surround with quotes
```

### Example 3: Renaming Variables

```
1. Select variable: `miw` or `*`
2. Find all occurrences: press `n` repeatedly
3. Split to cursors: `M-s`
4. Change all: `c` then type new name
```

### Example 4: Aligning Assignments

```
# Before:
x = 1
foo = 2
variable = 3

# Steps:
1. Select the = signs: `%` then `s` → `=`
2. Align them: `&`

# After:
x        = 1
foo      = 2
variable = 3
```

### Example 5: Creating a List

```
# Start with:
apple
banana
cherry

# Steps:
1. `%` to select all
2. `x` to select lines
3. `ms[` to surround with brackets
4. `,` to remove cursors
5. `J` to join lines

# Result:
[apple] [banana] [cherry]
```

## Keyboard Reference

| Key | Command | Description |
|-----|---------|-------------|
| `M-<mouse-1>` | Add cursor | Create cursor on click |
| `C` | Copy down | Duplicate cursor/selection down |
| `M-c` | Copy up | Duplicate cursor/selection up |
| `s` | Select regex | Create cursors for matches |
| `S` | Split regex | Create cursors for non-matches |
| `M-s` | Split newlines | Split on line boundaries |
| `K` | Keep matching | Keep only matching selections |
| `M-K` | Remove matching | Remove matching selections |
| `,` | Remove all fake | Keep only main cursor |
| `M-,` | Remove main | Promote first fake cursor |
| `(` | Rotate back | Rotate main selection backward |
| `)` | Rotate forward | Rotate main selection forward |
| `M-(` | Rotate content back | Rotate selections' content backward |
| `M-)` | Rotate content forward | Rotate selections' content forward |
| `M--` | Merge | Merge all into single selection |
| `&` | Align | Align selections vertically |

## See Also

- [Keybindings Reference](keybindings.md)
- [Customization Guide](customization.md)
- [Helix Documentation](https://docs.helix-editor.com/)
