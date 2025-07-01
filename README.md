# buffer-background

Display colors as buffer backgrounds in GNU Emacs.

## Overview

`buffer-background` is an Emacs package that allows you to display solid colors as backgrounds for your buffers. This focused implementation provides reliable color backgrounds with transparency and automatic assignment to specific buffers based on buffer name, major mode, file extension, or custom predicates. Each buffer can have its own unique background settings.

## Features

- **Zero Dependencies**: Uses only built-in Emacs face remapping
- **Color Backgrounds**: Solid colors (hex codes or color names) with opacity
- **Per-Buffer Settings**: Each buffer can have unique background properties
- **Visual Effects**:
  - Adjustable opacity/transparency
  - Proper color blending with default theme
- **Auto-Assignment**: Automatically apply backgrounds based on:
  - Buffer name (exact match or regexp)
  - Major mode
  - File extension
  - Custom predicates (e.g., remote files, compilation buffers)
- **Interactive Commands**: Full set of commands for easy management
- **Reliable Implementation**: Uses face remapping for proper integration
- **Customizable**: Comprehensive customization options via `customize-group`

## Installation

### Using `use-package` with VC

```elisp
(use-package buffer-background
  :vc (:url "https://github.com/theesfeld/buffer-background")
  :ensure t
  :custom
  ;; Set default opacity
  (buffer-background-opacity 0.3)
  :config
  ;; Configure different backgrounds for different buffer types
  (setq buffer-background-color-alist
        '(;; Simple color assignments
          ("*scratch*" . "#2d2d2d")
          ("*Messages*" . "#1a1a1a")
          
          ;; Color with custom opacity
          ("*Warnings*" . (:color "#3d1a1a" :opacity 0.8))
          
          ;; Major modes with colors
          (org-mode . (:color "#1e1e2e" :opacity 0.9))
          (python-mode . (:color "#002b36" :opacity 0.8))
          
          ;; File extensions
          ((file . "txt") . (:color "#1c1c1c" :opacity 0.7))
          
          ;; Custom predicates for special conditions
          ((lambda (buf)
             (file-remote-p default-directory))
           . (:color "#1a1a3d" :opacity 0.8))))
  
  ;; Enable global mode for automatic buffer assignment
  (buffer-background-global-mode 1)
  
  ;; Optional: Set up keybindings
  :bind (("C-c b c" . buffer-background-set-color)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b x" . buffer-background-clear)
         ("C-c b o" . buffer-background-set-opacity)
         ("C-c b s" . buffer-background-show-color-source)))
```

### Manual Installation

1. Download `buffer-background.el`
2. Place it in your Emacs `load-path`
3. Add to your init file:

```elisp
(require 'buffer-background)
```

## Quick Start

1. **Set color background for current buffer:**
   ```elisp
   M-x buffer-background-set-color RET #2d2d2d
   ```

2. **Toggle background on/off:**
   ```elisp
   M-x buffer-background-toggle
   ```

3. **Set up automatic backgrounds:**
   ```elisp
   ;; Using buffer-background-color-alist
   (setq buffer-background-color-alist
         '(;; Simple assignments
           ("*scratch*" . "#2d2d2d")
           ("*Messages*" . "#1a1a1a")
           
           ;; With custom opacity
           (org-mode . (:color "#1e1e2e" :opacity 0.9))
           (python-mode . (:color "#002b36" :opacity 0.8))))
   (buffer-background-global-mode 1)
   ```

## Commands

### Interactive Commands

| Command | Description |
|---------|-------------|
| `buffer-background-set-color` | Set background color for current buffer |
| `buffer-background-toggle` | Toggle background mode on/off |
| `buffer-background-clear` | Remove background from current buffer |
| `buffer-background-set-opacity` | Set transparency level interactively |
| `buffer-background-show-color-source` | Show which color would be used for current buffer |

### Convenience Commands

| Command | Description |
|---------|-------------|
| `buffer-background-enable-for-scratch` | Enable background for *scratch* buffer |
| `buffer-background-enable-for-messages` | Enable background for *Messages* buffer |

## Customization Options

Access customization via `M-x customize-group RET buffer-background RET` or set variables directly:

### Core Settings

```elisp
;; Buffer-specific background assignments
(setq buffer-background-color-alist
      '(;; Simple color assignments
        ("*scratch*" . "#2d2d2d")
        ("*Messages*" . "#1a1a1a")
        
        ;; With detailed settings
        ("*Warnings*" . (:color "#3d1a1a" :opacity 0.8))
        (org-mode . (:color "#1e1e2e" :opacity 0.9))
        ((mode . python-mode) . (:color "#002b36" :opacity 0.8))
        ((file . "txt") . (:color "#1c1c1c" :opacity 0.7))
        ((lambda (buf) (file-remote-p default-directory))
         . (:color "#1a1a3d" :opacity 0.8))))

;; Default opacity level (0.0 - 1.0)
(setq buffer-background-opacity 0.3)
```

### Auto-Assignment

```elisp
;; Enable automatic background assignment
(setq buffer-background-auto-enable t)

;; Enable global mode
(buffer-background-global-mode 1)
```

## Buffer Pattern Matching

### For `buffer-background-color-alist`:

- **Exact strings**: `"*scratch*"` matches buffer name exactly
- **Regexp patterns**: `"\\*Help.*\\*"` matches all Help buffers
- **Major mode symbols**: `org-mode` matches all org-mode buffers
- **Mode cons cells**: `(mode . python-mode)` alternative syntax
- **File extension**: `(file . "txt")` matches files ending in .txt
- **Custom predicates**: `(lambda (buf) ...)` for complex matching

### Color Specifications

Colors can be specified as:
- **Hex colors**: `"#ff0000"`, `"#2d2d2d"`
- **Named colors**: `"red"`, `"dark blue"`
- **With opacity**: `(:color "#ff0000" :opacity 0.5)`

## Programmatic Usage

```elisp
;; Apply background to specific buffer
(with-current-buffer "*scratch*"
  (buffer-background-set-color "#2d2d2d")
  (setq-local buffer-background-opacity 0.8)
  (buffer-background-mode 1))

;; Set different backgrounds for different buffer types
(add-hook 'org-mode-hook 
          (lambda ()
            (when (string-match "README" (buffer-name))
              (buffer-background-set-color "#1e1e2e"))))

;; Use with hooks
(add-hook 'buffer-background-after-enable-hook
          (lambda () (message "Background applied to %s" (buffer-name))))
```

## Troubleshooting

### Background Not Showing

1. Check if buffer-background-mode is enabled: `M-x buffer-background-toggle`
2. Verify color is valid: Try a simple hex color like `#ff0000`
3. Check if global mode is enabled: `M-x buffer-background-global-mode`

### Auto-Assignment Not Working

1. Verify `buffer-background-global-mode` is enabled
2. Check `buffer-background-color-alist` patterns match your buffers
3. Ensure `buffer-background-auto-enable` is t
4. Use `buffer-background-show-color-source` to test pattern matching

## Examples

### Basic Setup

```elisp
(use-package buffer-background
  :vc (:url "https://github.com/theesfeld/buffer-background")
  :ensure t
  :custom
  (buffer-background-opacity 0.3)
  :config
  ;; Configure which buffers get backgrounds
  (setq buffer-background-color-alist
        '(("*scratch*" . "#2d2d2d")
          ("*Messages*" . "#1a1a1a")))
  (buffer-background-global-mode 1))
```

### Advanced Configuration

```elisp
(use-package buffer-background
  :vc (:url "https://github.com/theesfeld/buffer-background")
  :ensure t
  :custom
  ;; Set global defaults
  (buffer-background-opacity 0.3)
  (buffer-background-auto-enable t)
  :config
  ;; Comprehensive buffer-to-color mappings
  (setq buffer-background-color-alist
        '(;; === EXACT BUFFER NAME MATCHING ===
          ("*scratch*" . (:color "#2d2d2d" :opacity 0.8))
          ("*Messages*" . "#1a1a1a")
          ("*Warnings*" . (:color "#3d1a1a" :opacity 0.9))
          
          ;; === REGEXP PATTERN MATCHING ===
          ("\\*Help.*\\*" . (:color "#1e1e2e" :opacity 0.85))
          ("\\*Compile.*\\*" . (:color "#2d2d2d" :opacity 0.9))
          ("\\*.*shell.*\\*" . (:color "#1a1a2d" :opacity 0.8))
          
          ;; === MAJOR MODE ASSIGNMENTS ===
          (org-mode . (:color "#002b36" :opacity 0.8))        ; Solarized dark
          (python-mode . (:color "#1a1a2d" :opacity 0.8))     ; Blue tint
          (emacs-lisp-mode . (:color "#2d1a2d" :opacity 0.8)) ; Purple tint
          (c-mode . (:color "#1a1a1a" :opacity 0.85))
          
          ;; === ALTERNATIVE MODE SYNTAX ===
          ((mode . js-mode) . (:color "#2d2d1a" :opacity 0.75))      ; Yellow tint
          ((mode . typescript-mode) . (:color "#1a2d2d" :opacity 0.75)) ; Cyan tint
          ((mode . css-mode) . (:color "#1a2b3c" :opacity 0.75))
          
          ;; === FILE EXTENSION MATCHING ===
          ((file . "md") . (:color "#f8f8f2" :opacity 0.05))  ; Light for readability
          ((file . "txt") . (:color "#1c1c1c" :opacity 0.7))
          ((file . "json") . (:color "#1a1a1a" :opacity 0.7))
          ((file . "yaml") . (:color "#2a2a1a" :opacity 0.7))
          
          ;; === CUSTOM PREDICATE MATCHING ===
          ;; Remote files (TRAMP)
          ((lambda (buf)
             (file-remote-p default-directory))
           . (:color "#1a1a3d" :opacity 0.8))
          
          ;; All programming modes
          ((lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'prog-mode)))
           . (:color "#1a1a1a" :opacity 0.6))
          
          ;; Test files
          ((lambda (buf)
             (string-match-p "\\(test\\|spec\\)" (buffer-name buf)))
           . (:color "#0a2a0a" :opacity 0.85))   ; Dark green
          
          ;; Dired buffers
          ((lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'dired-mode)))
           . (:color "#2a2a2a" :opacity 0.7))))
  
  ;; Enable global mode for automatic buffer assignment
  (buffer-background-global-mode 1)
  
  ;; Comprehensive keybindings
  :bind (("C-c b c" . buffer-background-set-color)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b x" . buffer-background-clear)
         ("C-c b o" . buffer-background-set-opacity)
         ("C-c b s" . buffer-background-show-color-source))
  
  ;; Optional hooks
  :hook ((buffer-background-after-enable . (lambda () 
                                             (message "Background applied to %s" (buffer-name))))))
```

## How It Works

The package uses **face remapping** (`face-remap-add-relative`) to change the background color of the `default` face in each buffer. This approach:

- ✅ **Properly integrates** with Emacs' face system
- ✅ **Works reliably** across different themes and configurations  
- ✅ **Respects transparency** through proper color blending
- ✅ **Doesn't interfere** with text display or scrolling

## Requirements

- GNU Emacs 27.1 or later
- Color support (standard in all modern Emacs builds)

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## Changelog

### Version 2.1.0 (Current - function/color branch)
- **BREAKING**: Removed all image functionality to focus on reliable color backgrounds
- **NEW**: Uses face remapping instead of overlays for proper background integration
- **SIMPLIFIED**: `buffer-background-color-alist` replaces `buffer-background-image-alist`
- **IMPROVED**: Color backgrounds now work reliably and don't interfere with text
- **ENHANCED**: Better opacity handling with proper color blending
- Verified working implementation with comprehensive tests

### Version 2.0.0
- Enhanced `buffer-background-image-alist` to support per-buffer settings
- Added color background support (now removed in 2.1.0)
- Per-buffer settings for all properties
- Support for detailed plist specifications

### Version 1.0.0
- Initial release with image support