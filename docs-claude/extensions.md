# Hel Extensions

This document describes the available extensions for Hel and how to use them.

## Table of Contents

- [hel-leader](#hel-leader)
- [hel-paredit](#hel-paredit)
- [hel-org](#hel-org)
- [Creating Custom Extensions](#creating-custom-extensions)

## hel-leader

The `hel-leader` extension provides a keypad-style interface for executing Emacs commands without modifier keys, similar to Meow's keypad or God-mode.

### Installation

```elisp
(use-package hel-leader
  :after hel)
```

### Usage

Press `SPC` in Normal or Motion state to enter keypad mode. In keypad mode, keys are automatically modified according to translation rules.

### Translation Rules

The first key determines the prefix:

| First Key | Prefix | Example |
|-----------|--------|---------|
| `x` | `C-x` | `x f` → `C-x f` |
| `h` | `C-h` | `h v` → `C-h v` |
| `c` | `C-c` | `c c` → `C-c c` |
| `m` | `M-` | `m x` → `M-x` |
| `g` | `C-M-` | `g x` → `C-M-x` |
| Other | Leader map | `a` → `C-c a` (by default) |

After the first key:

| Key | Meaning | Example |
|-----|---------|---------|
| `m` | `M-` prefix | `x m f` → `C-x M-f` |
| `g` | `C-M-` prefix | `x g f` → `C-x C-M-f` |
| `SPC` | Literal (no `C-`) | `x SPC p` → `C-x p` |
| Other | Add `C-` | `x f f` → `C-x C-f C-f` |

### Examples

```
Input         Translation      Emacs Command
-----------------------------------------------
SPC x f       C-x C-f          find-file
SPC x SPC p   C-x p            project-prefix-map
SPC m x       M-x              execute-extended-command
SPC g x       C-M-x            eval-defun
SPC c t t     C-c C-t C-t      (mode-specific)
SPC a         C-c a            (mode-specific)
```

### Fallback Behavior

If a binding like `C-c C-a` is undefined, it automatically tries `C-c a` as a fallback.

### Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `SPC` | Enter keypad | From Normal/Motion state |
| `C-w SPC` | Keypad other window | Execute in other window |
| `DEL` / `<backspace>` | Undo input | Remove last key |
| `ESC` / `<escape>` / `C-g` | Quit | Exit keypad |

### Customization

```elisp
;; Change the meta prefix key (default: "m")
(setq hel-leader-meta-prefix "m")

;; Change the control-meta prefix key (default: "g")
(setq hel-leader-ctrl-meta-prefix "g")

;; Change the literal prefix key (default: "SPC")
(setq hel-leader-literal-prefix "SPC")

;; Send C-x without additional Control modifier
(setq hel-leader-send-C-x-with-control-modifier nil)

;; Set custom leader keymap (default: mode-specific-map, i.e., C-c)
(setq hel-leader-leader-keymap my-custom-keymap)

;; Disable echo messages
(setq hel-leader-echo nil)

;; Customize message prefix
(setq hel-leader-message-prefix "Leader: ")
```

### Help Integration

To describe a keypad key sequence:

1. Press `C-h k` (or `F1 k`)
2. Press `SPC` to enter keypad
3. Type the key sequence
4. See the command description

### Tips

- Use `SPC x` frequently for `C-x` commands
- `SPC m` gives you access to all `M-` commands
- Mode-specific commands are under `SPC <key>` (defaults to `C-c`)
- The literal prefix (`SPC SPC`) is useful for commands like `C-x p f` (project-find-file)

## hel-paredit

Integration with [Paredit](https://paredit.org/) for structural editing of S-expressions in Lisp-like languages.

### Installation

```elisp
(use-package paredit :ensure t)
(use-package hel-paredit
  :after hel
  :hook (emacs-lisp-mode . hel-paredit-mode))
```

Or for all Lisp modes:

```elisp
(use-package hel-paredit
  :after hel
  :hook ((emacs-lisp-mode
          lisp-mode
          scheme-mode
          clojure-mode) . hel-paredit-mode))
```

### Features

#### Enhanced Word Motions

Word motions that intelligently skip parentheses:

| Key | Command | Description |
|-----|---------|-------------|
| `W` | WORD forward | Skip over balanced sexps |
| `B` | WORD backward | Skip over balanced sexps |
| `E` | WORD end | Move to end, skipping sexps |

```elisp
;; Before (at |):
|(foo bar (baz quux))

;; After W:
(foo bar (baz quux))|
```

#### S-expression Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `M-n` or `C-l` | Next sexp | Move forward one sexp |
| `M-p` or `C-h` | Previous sexp | Move backward one sexp |
| `M-o` or `C-k` | Up sexp | Move to parent sexp |
| `M-i` or `C-j` | Down sexp | Move into child sexp |
| `L` | Up forward | Select parent, then move up |
| `H` | Up backward | Select parent, backward |

```elisp
;; Navigation example:
(defun foo ()
  |(+ 1 2)    ; Press M-n → move to (+ 1 2)
  (* 3 4))    ; Press M-o → select (defun ...)
```

#### Safe Deletion

| Key | Command | Description |
|-----|---------|-------------|
| `d` | Cut (safe) | Delete respecting balance |
| `D` | Delete (safe) | Delete without kill-ring |

Paredit prevents deletion that would unbalance parentheses:

```elisp
;; Trying to delete ") x" is prevented:
(foo bar)| x (baz)
         ^~~~~^
```

#### Slurping & Barfing

| Key | Command | Description |
|-----|---------|-------------|
| `<` | Barf/slurp | Slurp backward or barf forward |
| `>` | Slurp/barf | Slurp forward or barf backward |

Context-dependent behavior:

```elisp
;; Before (cursor at |):
|(foo bar) baz

;; After >:
(foo bar baz)|

;; Before:
(foo bar| baz)

;; After <:
(foo bar)| baz
```

#### Other Paredit Commands

| Key | Command | Description |
|-----|---------|-------------|
| `M-r` | Raise sexp | Replace parent with current sexp |
| `M-?` | Convolute | Swap nested expressions |
| `M-d` | Kill sexp forward | Delete following sexp |

### Text Objects

Paredit adds WORD text objects that skip parentheses:

| Key | Command | Description |
|-----|---------|-------------|
| `mW` or `miW` | Inner WORD | Select WORD (skips parens) |
| `maW` | Around WORD | Select WORD with space |

### Insert State

In Insert state, Paredit's automatic parenthesis balancing is active:

| Key | Command | Description |
|-----|---------|-------------|
| `(` | Open paren | Insert balanced `()` |
| `)` | Close paren | Move past or insert |
| `[` | Open bracket | Insert balanced `[]` |
| `"` | Double quote | Insert balanced `""` |
| `\` | Backslash | Escape next character |
| `;` | Semicolon | Comment-aware insertion |
| `M-;` | Comment | Paredit-aware commenting |

### Tips

- Use `M-o`/`M-i` (or `C-k`/`C-j`) to navigate the syntax tree
- Slurp and barf with `<` and `>` for restructuring
- `M-r` (raise) is powerful for eliminating wrapper forms
- Safe deletion prevents most structural errors

### Example Workflow

```elisp
;; Start with:
(if |(condition)
    (do-something)
  (do-else))

;; 1. M-n to move to (do-something)
;; 2. M-r to raise it:
(if (do-something)
  (do-else))

;; 3. M-o to select the if form
;; 4. M-r again:
(do-something)
```

## hel-org

Integration with [Org mode](https://orgmode.org/) for structured document editing and navigation.

### Installation

```elisp
(use-package hel-org
  :after (hel org))
```

### Features

#### AST Navigation

Navigate Org's document structure:

| Key | Command | Description |
|-----|---------|-------------|
| `M-o` or `C-h` | Up element | Select parent element |
| `M-i` or `C-j` | Down element | Select first child |
| `M-n` | Next element | Select next sibling |
| `M-p` | Previous element | Select previous sibling |
| `zu` | Up heading | Jump to parent heading |

```org
* Top Level        ← M-o selects this
** Subsection      ← M-i enters here
   Content         ← M-i again selects content
** Another         ← M-n moves here
```

#### Org-Specific Motions

| Key | Command | Description |
|-----|---------|-------------|
| `gh` | First non-blank | Like gh, but Org-aware |
| `]s` / `[s` | Next/prev sentence | Org sentence motion |
| `].` / `[.` | Next/prev sentence | Alternative binding |

#### Structural Editing

| Key | Command | Description |
|-----|---------|-------------|
| `<` | Promote | Promote heading/list item |
| `>` | Demote | Demote heading/list item |
| `H` / `L` | Shift left/right | Date/priority shift |
| `M-h/j/k/l` | Meta motion | Org meta-commands |
| `M-H/J/K/L` | Meta shift | Org shift-meta-commands |
| `C-S-h/j/k/l` | Control shift | Control-shift commands |

#### Insertion

| Key | Command | Description |
|-----|---------|-------------|
| `[RET` | Insert above | New heading/item above |
| `]RET` | Insert below | New heading/item below |

Context-aware insertion:

```org
* Heading           * Heading
  Content    →        Content
                    * |           (cursor here)
```

#### Org-Specific Text Objects

##### Headings

| Key | Command | Description |
|-----|---------|-------------|
| `mh` or `mih` | Inner heading | Select subtree |

##### Emphasis

For `*bold*`, `/italic/`, `_underline_`, `+strike+`:

| Key | Command | Description |
|-----|---------|-------------|
| `m/` or `mi/` | Inner emphasis | Select text only |
| `ma/` | Around emphasis | Include markers |
| `m*`, `m_`, `m+` | Other emphasis | Same behavior |

##### Verbatim/Code

For `=verbatim=` and `~code~`:

| Key | Command | Description |
|-----|---------|-------------|
| `m=` or `mi=` | Inner verbatim | Select text only |
| `ma=` | Around verbatim | Include markers |
| `m~` or `mi~` | Inner code | Select text only |
| `ma~` | Around code | Include markers |

##### Sentences

| Key | Command | Description |
|-----|---------|-------------|
| `m.` or `mis` | Inner sentence | Select Org sentence |
| `mas` | Around sentence | Include whitespace |

#### Copy/Paste

| Key | Command | Description |
|-----|---------|-------------|
| `d` | Cut | Org-aware cut |
| `p` | Paste after | Org-aware paste |
| `P` | Paste before | Org-aware paste |

### Special Modes

#### Org Capture

| Key | Command | Description |
|-----|---------|-------------|
| `Z R` | Refile | Refile capture |
| `Z Z` | Finalize | Save capture |
| `Z Q` | Abort | Cancel capture |

#### Org Src Edit

When editing source blocks:

| Key | Command | Description |
|-----|---------|-------------|
| `zx` | Save | Save and exit |
| `Z Z` | Exit | Save and exit |
| `Z Q` | Abort | Cancel changes |
| `C-c C-c` | Exit | Alternative exit |

#### Org Table Edit

| Key | Command | Description |
|-----|---------|-------------|
| `zx` | Finish | Finish editing |
| `Z Z` | Finish | Alternative binding |
| `Z Q` | Abort | Cancel changes |

### Date Picker

In Org date picker (e.g., `C-c .`):

| Key | Command | Description |
|-----|---------|-------------|
| `M-h/l` | Day | Move day backward/forward |
| `M-j/k` | Week | Move week forward/backward |
| `M-H/L` | Month | Move month backward/forward |
| `M-J/K` | Year | Move year forward/backward |

### Tips

- Use `M-o`/`M-i` to climb the document tree
- `<` and `>` work on headings, list items, and tables
- Text objects make it easy to manipulate Org markup
- AST navigation is much faster than traditional motions

### Example Workflows

#### Restructuring

```org
** Current heading
   Content here
   More content

# Steps:
1. Select heading: mih
2. Promote: <
3. Result:

* Current heading
  Content here
  More content
```

#### Adding emphasis

```org
This is important text

# Steps:
1. Select "important": miw
2. Surround with emphasis: ms*

Result:
This is *important* text
```

#### Navigating large documents

```org
* Chapter 1          ← Start here
** Section 1.1       ← M-i to enter
   Content           ← M-n to next
** Section 1.2       ← M-o to go up
   Content           ← M-o again to Chapter level
* Chapter 2          ← M-n to next chapter
```

## Creating Custom Extensions

You can create your own Hel extensions to integrate with other packages or add custom functionality.

### Basic Structure

```elisp
;;; hel-myextension.el --- My Hel Extension

(require 'hel-core)
(require 'hel-common)
(require 'other-package)

;; Define commands
(hel-define-command my-custom-command ()
  "My custom command description."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  ;; Implementation
  )

;; Set up keybindings
(hel-keymap-set other-package-mode-map :state 'normal
  "g m" #'my-custom-command)

;; Add hooks
(add-hook 'other-package-mode-hook #'my-setup-function)

(provide 'hel-myextension)
```

### Adding State-Specific Keybindings

```elisp
;; For a specific mode
(with-eval-after-load 'my-mode
  (hel-keymap-set my-mode-map :state 'normal
    "g r" #'my-refresh-command
    "g d" #'my-goto-command))
```

### Creating Commands with Multiple Cursor Support

```elisp
(hel-define-command my-increment-numbers (start)
  "Insert incremental numbers starting from START."
  :multiple-cursors nil  ; We'll handle cursors ourselves
  (interactive "nStart from: ")
  (let ((n (1- start)))
    (hel-with-each-cursor
      (insert (number-to-string (cl-incf n))))))
```

### Adding Text Objects

```elisp
;; Define a "thing" for thingatpt
(put 'my-thing 'forward-op #'my-forward-thing-function)
(put 'my-thing 'bounds-of-thing-at-point #'my-bounds-function)

;; Add mark commands
(hel-define-command hel-mark-inner-my-thing (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'my-thing count))

;; Bind to keys
(hel-keymap-global-set :state 'normal
  "m t" #'hel-mark-inner-my-thing)
```

### Adding Surround Patterns

```elisp
(defun my-mode-surround-setup ()
  "Set up surround patterns for my-mode."
  (hel-surround-add-pair ?< (cons "{{" "}}")
    :search ("{{" . "}}")))

(add-hook 'my-mode-hook #'my-mode-surround-setup)
```

### Integration Checklist

When integrating with another package:

1. ☐ Set initial state for the mode
2. ☐ Add Normal state keybindings
3. ☐ Add Motion state keybindings (if needed)
4. ☐ Define custom commands with multiple cursor support
5. ☐ Add text objects (if applicable)
6. ☐ Configure surround patterns (if applicable)
7. ☐ Add mode to incompatible list (if it causes issues with multiple cursors)
8. ☐ Set up hooks for auto-configuration
9. ☐ Add advices for command compatibility
10. ☐ Document the integration

### Example: Full Integration

```elisp
;;; hel-mymode.el --- Hel integration for mymode

(require 'hel-core)
(require 'hel-common)
(require 'mymode)

;; Set initial state
(hel-set-initial-state 'mymode 'normal)

;; Define commands
(hel-define-command hel-mymode-special-action ()
  "Perform special action in mymode."
  :multiple-cursors t
  (interactive)
  (mymode-do-something))

;; Keybindings
(hel-keymap-set mymode-map :state 'normal
  "g r" #'mymode-refresh
  "g s" #'hel-mymode-special-action
  "RET" #'mymode-enter)

;; Make commands work with multiple cursors
(put 'mymode-insert 'multiple-cursors t)
(put 'mymode-delete 'multiple-cursors t)

;; Add advice for compatibility
(hel-advice-add 'mymode-jump :around #'hel-jump-command-a)

;; Setup hook
(defun hel-mymode-setup ()
  "Set up Hel for mymode."
  (hel-surround-add-pair ?m (cons "[[" "]]")))

(add-hook 'mymode-hook #'hel-mymode-setup)

(provide 'hel-mymode)
```

## See Also

- [Keybindings Reference](keybindings.md)
- [Customization Guide](customization.md)
- [Multiple Cursors Guide](multiple-cursors.md)
