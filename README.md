# js-ts-defs

[![MELPA](https://melpa.org/packages/js-ts-defs-badge.svg)](https://melpa.org/#/js-ts-defs) [![CI](https://github.com/jacksonrayhamilton/js-ts-defs/actions/workflows/test.yml/badge.svg)](https://github.com/jacksonrayhamilton/js-ts-defs/actions/workflows/test.yml)

Find JavaScript variable definitions using tree-sitter.

## Overview

This package provides `js-ts-defs-jump-to-definition` which jumps to the definition of the JavaScript identifier at point. It uses tree-sitter to parse JavaScript code and build a scope structure to accurately resolve variable definitions.

## Features

- Jump to definitions of variables, functions, classes, and imports
- Handles JavaScript scoping rules (var, let, const)
- Supports destructuring patterns and function parameters
- Works with arrow functions and regular functions
- Handles block scoping and lexical declarations

## Prerequisites

Before using this package, you need to:

1. Install the tree-sitter JavaScript grammar:

   a. First, set up the language source by adding this to your configuration:
   ```elisp
   ;; For Emacs 29.x, use version 0.20.1
   (add-to-list 'treesit-language-source-alist
                '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1"))

   ;; For Emacs 30.x, use version 0.23.1
   (add-to-list 'treesit-language-source-alist
                '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1"))
   ```

   b. Then install the grammar:
   ```elisp
   M-x treesit-install-language-grammar RET javascript RET
   ```

2. Set up `js-ts-mode`:

   a. Open JavaScript files in the new tree-sitter-enabled major mode, `js-ts-mode`, by adding this to your configuration:
   ```elisp
   (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
   ```

   b. If you have any existing `js-mode-hook` configurations, migrate them to `js-ts-mode-hook`:
   ```elisp
   ;; Change this:
   (add-hook 'js-mode-hook ...)

   ;; To this:
   (add-hook 'js-ts-mode-hook ...)
   ```

## Usage

Call `M-x js-ts-defs-jump-to-definition` to jump to the definition of the identifier at point. Use `M-,` to jump back.

## Recommended Setup

Bind the function to `M-.` in `js-ts-mode`:

```elisp
(add-hook 'js-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'js-ts-defs-jump-to-definition)))
```

This gives you:
- `M-.` - Jump to definition
- `M-,` - Jump back

## API

This package exposes several functions for programmatic use:

### `js-ts-defs-build-scope`

```elisp
(js-ts-defs-build-scope root-node)
```

Builds a scope structure from a tree-sitter root node. Returns a nested scope object containing variable definitions and their positions.

**Parameters:**
- `root-node`: A tree-sitter root node from `(treesit-buffer-root-node)`

**Returns:** A scope object (see structure below)

### `js-ts-defs-find-definition`

```elisp
(js-ts-defs-find-definition scope identifier position)
```

Finds the definition of an identifier within a scope structure.

**Parameters:**
- `scope`: A scope object returned by `js-ts-defs-build-scope`
- `identifier`: String name of the identifier to find
- `position`: Buffer position where the identifier is used

**Returns:** Buffer position of the definition, or `nil` if not found

### Scope Object Structure

The scope object is a property list with the following structure:

```elisp
(:type "program"           ; Scope type: "program", "function", "block", "for"
 :start 1                  ; Starting buffer position
 :end 100                  ; Ending buffer position
 :variables #<hash-table>  ; Hash table mapping identifier names to positions
 :children (...)           ; List of child scope objects
 :is-arrow t)              ; Optional: present for arrow functions
```

**Scope Types:**
- `"program"`: Top-level/global scope
- `"function"`: Function scope (including arrow functions)
- `"block"`: Block scope (for let/const declarations)
- `"for"`: For-loop scope (for let/const in loop initializers)

**Example Usage:**

```elisp
;; Build scope from current buffer
(let* ((root-node (treesit-buffer-root-node))
       (scope (js-ts-defs-build-scope root-node)))

  ;; Find definition of "myVar" at position 150
  (js-ts-defs-find-definition scope "myVar" 150))
```

## Requirements

- Emacs 29.1 or later
- JavaScript tree-sitter grammar installed
