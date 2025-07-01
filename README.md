# buffer-background

Display images as buffer backgrounds in GNU Emacs.

## Overview

`buffer-background` is an Emacs package that allows you to display images or solid colors as backgrounds for your buffers. It supports various image formats (PNG, JPEG, SVG, GIF, BMP, TIFF) and color specifications, with comprehensive customization options including transparency, scaling modes, positioning, rotation, and automatic assignment to specific buffers based on buffer name, major mode, file extension, or custom predicates. Each buffer can have its own unique background settings.

## Features

- **Zero Dependencies**: Uses only built-in Emacs image and SVG support
- **Multiple Background Types**: 
  - Image files (PNG, JPEG, SVG, GIF, BMP, TIFF)
  - Solid colors (hex codes or color names)
- **Per-Buffer Settings**: Each buffer can have unique background properties
- **Flexible Scaling**: Fit, fill, tile, or actual size modes
- **Visual Effects**:
  - Adjustable opacity/transparency
  - Grayscale conversion
  - Image rotation (0°, 90°, 180°, 270°)
  - Blur effects (for future enhancement)
  - Brightness and contrast adjustments (planned)
- **Auto-Assignment**: Automatically apply backgrounds based on:
  - Buffer name (exact match or regexp)
  - Major mode
  - File extension
  - Custom predicates (e.g., remote files, compilation buffers)
- **Interactive Commands**: Full set of commands for easy management
- **Performance Optimized**: Image caching and efficient overlay management
- **Customizable**: Comprehensive customization options via `customize-group`

## Installation

### Manual Installation

1. Download `buffer-background.el`
2. Place it in your Emacs `load-path`
3. Add to your init file:

```elisp
(require 'buffer-background)
```

### Using `use-package`

```elisp
(use-package buffer-background
  :load-path "path/to/buffer-background"
  :custom
  ;; Set default/fallback values (can be overridden per-buffer)
  (buffer-background-image-file "~/Pictures/background.png")
  (buffer-background-opacity 0.3)
  (buffer-background-scale 'fit)
  (buffer-background-grayscale nil)
  :config
  ;; Configure different backgrounds for different buffer types
  (setq buffer-background-image-alist
        '(;; Simple image assignment
          ("*scratch*" . "~/images/scratch.png")
          
          ;; Color background
          ("*Messages*" . "#1a1a1a")
          
          ;; Image with custom settings
          ("\\*Help.*\\*" . (:image "~/images/help.png"
                             :opacity 0.15
                             :scale fit))
          
          ;; Major mode with color
          (org-mode . (:color "#002b36"  ; Solarized dark
                       :opacity 0.9))
          
          ;; Major mode with image and effects
          (python-mode . (:image "~/images/python.svg"
                          :opacity 0.2
                          :grayscale t
                          :scale fill))
          
          ;; File extension with tiled pattern
          ((file . "txt") . (:image "~/images/paper-texture.png"
                             :scale tile
                             :opacity 0.1))
          
          ;; Custom predicate for remote files
          ((lambda (buf)
             (file-remote-p default-directory))
           . (:color "dark blue"
              :opacity 0.9))
          
          ;; Dark background for compilation
          ((lambda (buf)
             (derived-mode-p 'compilation-mode))
           . (:color "#0a0a0a"
              :opacity 0.95))))
  
  ;; Enable global mode for automatic buffer assignment
  (buffer-background-global-mode 1)
  
  ;; Optional: Set up keybindings
  :bind (("C-c b s" . buffer-background-select-image)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b c" . buffer-background-clear)
         ("C-c b o" . buffer-background-set-opacity)
         ("C-c b g" . buffer-background-toggle-grayscale)
         ("C-c b r" . buffer-background-reload)
         ("C-c b i" . buffer-background-show-image-source))
  
  ;; Optional: Hooks
  :hook ((buffer-background-after-enable . (lambda () 
                                             (message "Background enabled!"))))
```

## Quick Start

1. **Enable background for current buffer:**
   ```elisp
   M-x buffer-background-select-image
   ```

2. **Toggle background on/off:**
   ```elisp
   M-x buffer-background-toggle
   ```

3. **Set up automatic backgrounds:**
   ```elisp
   ;; Using buffer-background-image-alist (per-spec configuration)
   (setq buffer-background-image-alist
         '(;; Simple assignments
           ("*scratch*" . "~/images/scratch.png")
           ("*Messages*" . "#1a1a1a")  ; Dark color
           
           ;; With custom settings
           (org-mode . (:image "~/images/org.jpg"
                        :opacity 0.2
                        :grayscale t))
           
           ;; Color with opacity
           ((file . "py") . (:color "#002b36"
                             :opacity 0.85))))
   (buffer-background-global-mode 1)
   ```

## Commands

### Interactive Commands

| Command | Description |
|---------|-------------|
| `buffer-background-select-image` | Choose and set background image for current buffer |
| `buffer-background-toggle` | Toggle background mode on/off |
| `buffer-background-clear` | Remove background from current buffer |
| `buffer-background-set-opacity` | Set transparency level interactively |
| `buffer-background-set-scale` | Set scaling mode interactively |
| `buffer-background-toggle-grayscale` | Toggle grayscale conversion |
| `buffer-background-apply-to-buffer` | Apply background to a specific buffer |
| `buffer-background-reload` | Reload current background image |
| `buffer-background-clear-cache` | Clear image processing cache |
| `buffer-background-show-image-source` | Show which background would be used for current buffer |

### Convenience Commands

| Command | Description |
|---------|-------------|
| `buffer-background-enable-for-scratch` | Enable background for *scratch* buffer |
| `buffer-background-enable-for-messages` | Enable background for *Messages* buffer |

## Customization Options

Access customization via `M-x customize-group RET buffer-background RET` or set variables directly:

### Core Settings

```elisp
;; Buffer-specific background assignments (required for any backgrounds)
(setq buffer-background-image-alist
      '(;; Simple assignments
        ("*scratch*" . "~/images/scratch.png")
        ("*Messages*" . "#1a1a1a")  ; Color background
        
        ;; With detailed settings
        ("\\*Help.*\\*" . (:image "~/images/help.png"
                           :opacity 0.15))
        (org-mode . (:color "#002b36"
                     :opacity 0.9))
        ((mode . python-mode) . (:image "~/images/python.svg"
                                 :opacity 0.2
                                 :grayscale t))
        ((file . "txt") . (:image "~/images/texture.png"
                           :scale tile
                           :opacity 0.1))
        ((lambda (buf) (file-remote-p default-directory))
         . (:color "dark blue"
            :opacity 0.9))))

;; Opacity level (0.0 - 1.0)
(setq buffer-background-opacity 0.3)

;; Scaling mode: 'fit, 'fill, 'tile, or 'actual
(setq buffer-background-scale 'fit)

;; Image positioning (for 'actual scaling)
(setq buffer-background-position 'center)
```

### Visual Effects

```elisp
;; Convert to grayscale
(setq buffer-background-grayscale t)

;; Blur level (0-10, currently for future use)
(setq buffer-background-blur 0)

;; Margin around image
(setq buffer-background-margin 0)
```

### Auto-Assignment

```elisp
;; Using buffer-background-image-alist (required for any backgrounds)
;; Automatically assigns different images/colors based on buffer criteria
(setq buffer-background-image-alist
      '(;; Exact buffer names
        ("*scratch*" . "~/images/scratch.png")
        ("*Messages*" . "#1a1a1a")  ; Color background
        
        ;; Regexp patterns
        ("\\*Help.*\\*" . "~/images/help.png")
        ("\\*Compile.*\\*" . (:color "#2d2d2d" :opacity 0.9))
        
        ;; Major modes
        (org-mode . (:color "#002b36" :opacity 0.8))
        (python-mode . (:image "~/images/python.svg" :opacity 0.2))
        
        ;; Alternative syntax for modes
        ((mode . js-mode) . "~/images/javascript.png")
        ((mode . typescript-mode) . "~/images/typescript.png")
        
        ;; File extensions
        ((file . "txt") . (:image "~/images/text.png" :scale tile :opacity 0.1))
        ((file . "md") . (:color "#f8f8f2" :opacity 0.05))
        
        ;; Custom predicates
        ((lambda (buf) (file-remote-p default-directory))
         . (:color "dark blue" :opacity 0.3))
        ((lambda (buf) (bound-and-true-p compilation-mode))
         . (:color "#0a0a0a" :opacity 0.95))))

;; Enable auto-assignment
(setq buffer-background-auto-enable t)

;; Enable global mode
(buffer-background-global-mode 1)
```

## Scaling Modes

- **`fit`**: Scale image to fit buffer while preserving aspect ratio
- **`fill`**: Scale image to fill entire buffer (may crop)
- **`tile`**: Repeat image at original size to fill buffer
- **`actual`**: Display image at actual size (use with positioning)

## Buffer Pattern Matching

### For `buffer-background-image-alist`:

- **Exact strings**: `"*scratch*"` matches buffer name exactly
- **Regexp patterns**: `"\\*Help.*\\*"` matches all Help buffers
- **Major mode symbols**: `org-mode` matches all org-mode buffers
- **Mode cons cells**: `(mode . python-mode)` alternative syntax
- **File extension**: `(file . "txt")` matches files ending in .txt
- **Custom predicates**: `(lambda (buf) ...)` for complex matching

### Background Assignment Requirements

**Important**: Backgrounds are only applied when explicitly configured in `buffer-background-image-alist`. There is no default or fallback behavior.

## Programmatic Usage

```elisp
;; Apply background to specific buffer
(with-current-buffer "*scratch*"
  (buffer-background-set-image "~/Pictures/code-bg.png")
  (setq-local buffer-background-opacity 0.2)
  (buffer-background-mode 1))

;; Set different backgrounds for different buffer types
(add-hook 'org-mode-hook 
          (lambda ()
            (when (string-match "README" (buffer-name))
              (buffer-background-set-image "~/Pictures/docs-bg.png"))))

;; Use with hooks
(add-hook 'buffer-background-after-enable-hook
          (lambda () (message "Background applied to %s" (buffer-name))))
```

## Performance Tips

- **Image Cache**: The package caches processed images automatically
- **Clear Cache**: Use `buffer-background-clear-cache` after changing many settings
- **Image Size**: Smaller images load faster, especially for tiling
- **Opacity**: Lower opacity values may perform better

## Troubleshooting

### Background Not Showing

1. Verify image file exists and is readable
2. Check Emacs image support: `M-x image-type-available-p RET png RET`
3. Try different image formats
4. Check buffer has content (overlay needs text to display over)

### Performance Issues

1. Clear image cache: `M-x buffer-background-clear-cache`
2. Use smaller image files
3. Reduce opacity for better performance
4. Consider using 'actual or 'tile scaling for large images

### Auto-Assignment Not Working

1. Verify `buffer-background-global-mode` is enabled
2. Check `buffer-background-image-alist` patterns match your buffers
3. Ensure `buffer-background-auto-enable` is t
4. Use `buffer-background-show-image-source` to test pattern matching
5. Verify your buffer names/modes match the configured patterns exactly

## Examples

### Basic Setup

```elisp
(use-package buffer-background
  :custom
  (buffer-background-opacity 0.1)
  (buffer-background-scale 'tile)
  :config
  ;; Configure which buffers get backgrounds
  (setq buffer-background-image-alist
        '(("*scratch*" . "~/Pictures/subtle-pattern.png")
          ("*Messages*" . (:color "#1a1a1a" :opacity 0.8))))
  (buffer-background-global-mode 1))
```

### Advanced Configuration

This comprehensive example demonstrates the full power of the per-buffer specification system using `buffer-background-image-alist`:

```elisp
(use-package buffer-background
  :custom
  ;; Set global defaults (can be overridden per-buffer)
  (buffer-background-opacity 0.2)
  (buffer-background-scale 'fit)
  (buffer-background-grayscale nil)
  (buffer-background-auto-enable t)
  :config
  ;; Comprehensive buffer-to-background mappings using per-spec approach
  (setq buffer-background-image-alist
        '(;; === EXACT BUFFER NAME MATCHING ===
          ;; Simple string assignment (uses global defaults)
          ("*dashboard*" . "~/Pictures/dashboard-bg.png")
          
          ;; Detailed plist specifications with per-buffer overrides
          ("*scratch*" . (:image "~/Pictures/code-bg.png"
                          :opacity 0.15
                          :scale fit
                          :grayscale nil))
          
          ("*Messages*" . (:image "~/Pictures/log-texture.png"
                          :opacity 0.1
                          :scale tile
                          :grayscale t))
          
          ;; Color backgrounds with transparency
          ("*Backtrace*" . (:color "#2d1b1b"  ; Dark red tint
                           :opacity 0.9))
          
          ;; === REGEXP PATTERN MATCHING ===
          ;; Help system buffers
          ("\\*Help.*\\*" . (:color "#1e1e2e"  ; Catppuccin mocha base
                            :opacity 0.85))
          
          ;; All compilation-related buffers
          ("\\*Compile.*\\*" . (:color "#2d2d2d"
                               :opacity 0.9))
          
          ;; Shell and terminal buffers
          ("\\*.*shell.*\\*" . (:image "~/Pictures/terminal-bg.png"
                               :opacity 0.3
                               :scale tile))
          
          ;; === MAJOR MODE ASSIGNMENTS ===
          ;; Org mode with subtle background
          (org-mode . (:color "#002b36"  ; Solarized dark base03
                      :opacity 0.8))
          
          ;; Programming languages with themed backgrounds
          (python-mode . (:image "~/Pictures/python-logo-subtle.svg"
                         :opacity 0.2
                         :grayscale t
                         :scale fill
                         :position bottom-right))
          
          (emacs-lisp-mode . (:image "~/Pictures/emacs-spiral.png"
                             :opacity 0.25
                             :scale fit))
          
          (c-mode . (:color "#1a1a1a"
                    :opacity 0.85))
          
          (rust-mode . (:image "~/Pictures/rust-gear.png"
                       :opacity 0.15
                       :scale actual
                       :position center))
          
          ;; === ALTERNATIVE MODE SYNTAX ===
          ;; Web development modes
          ((mode . js-mode) . (:image "~/Pictures/js-bg.png"
                              :opacity 0.18
                              :scale fill))
          
          ((mode . typescript-mode) . (:image "~/Pictures/ts-bg.png"
                                      :opacity 0.18
                                      :scale fill))
          
          ((mode . css-mode) . (:color "#1a2b3c"
                               :opacity 0.75))
          
          ((mode . html-mode) . (:image "~/Pictures/html-structure.svg"
                                :opacity 0.12
                                :grayscale t))
          
          ;; === FILE EXTENSION MATCHING ===
          ;; Markdown files
          ((file . "md") . (:color "#f8f8f2"  ; Light background for readability
                           :opacity 0.05))
          
          ;; Text files with paper texture
          ((file . "txt") . (:image "~/Pictures/paper-texture.png"
                            :scale tile
                            :opacity 0.1))
          
          ;; Configuration files
          ((file . "json") . (:color "#1a1a1a"
                             :opacity 0.7))
          
          ((file . "yaml") . (:color "#2a2a1a"
                             :opacity 0.7))
          
          ((file . "toml") . (:color "#1a2a1a"
                             :opacity 0.7))
          
          ;; Image files - no background to avoid interference
          ((file . "png") . nil)
          ((file . "jpg") . nil)
          ((file . "svg") . nil)
          
          ;; === CUSTOM PREDICATE MATCHING ===
          ;; Remote files (TRAMP)
          ((lambda (buf)
             (and (buffer-file-name buf)
                  (file-remote-p (buffer-file-name buf))))
           . (:color "dark blue"
              :opacity 0.3))
          
          ;; All programming modes
          ((lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'prog-mode)))
           . (:image "~/Pictures/code-matrix.png"
              :opacity 0.08
              :grayscale t
              :scale tile))
          
          ;; Test files
          ((lambda (buf)
             (string-match-p "\\(test\\|spec\\)" (buffer-name buf)))
           . (:color "#0a2a0a"  ; Dark green for tests
              :opacity 0.85))
          
          ;; Large files (>1MB) - lighter background for performance
          ((lambda (buf)
             (and (buffer-file-name buf)
                  (> (file-attribute-size (file-attributes (buffer-file-name buf))) 1048576)))
           . (:color "#1a1a1a"
              :opacity 0.5))
          
          ;; Git commit messages
          ((lambda (buf)
             (string-match-p "COMMIT_EDITMSG" (buffer-name buf)))
           . (:image "~/Pictures/git-branch-bg.png"
              :opacity 0.15
              :scale fit))
          
          ;; Dired buffers
          ((lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'dired-mode)))
           . (:color "#2a2a2a"
              :opacity 0.7))))
  
  ;; Enable global mode for automatic buffer assignment
  (buffer-background-global-mode 1)
  
  ;; Comprehensive keybindings
  :bind (("C-c b s" . buffer-background-select-image)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b c" . buffer-background-clear)
         ("C-c b o" . buffer-background-set-opacity)
         ("C-c b g" . buffer-background-toggle-grayscale)
         ("C-c b r" . buffer-background-reload)
         ("C-c b i" . buffer-background-show-image-source)
         ("C-c b a" . buffer-background-apply-to-buffer)
         ("C-c b C" . buffer-background-clear-cache))
  
  ;; Optional hooks for additional customization
  :hook ((buffer-background-after-enable . (lambda () 
                                             (message "Background applied to %s" (buffer-name))))
         (buffer-background-after-disable . (lambda () 
                                              (message "Background removed from %s" (buffer-name))))))
```

## Requirements

- GNU Emacs 30.1 or later
- Image support compiled into Emacs (standard in most distributions)

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## Changelog

### Version 2.0.0
- **BREAKING**: Enhanced `buffer-background-image-alist` to support per-buffer settings
- Added color background support using built-in SVG functionality
- Per-buffer settings for all properties (opacity, scale, grayscale, etc.)
- New settings: brightness, contrast, rotation, blur (some for future enhancement)
- Support for detailed plist specifications with custom properties
- Backwards compatible with simple string assignments
- Enhanced `buffer-background-show-image-source` to show color backgrounds
- Improved caching system for complex specifications

### Version 1.1.0
- Added `buffer-background-image-alist` for flexible buffer-to-image mapping
- Support for major mode matching
- Support for file extension matching
- Support for custom predicate functions
- Added `buffer-background-show-image-source` command
- Enhanced auto-enable logic to check image-alist
- Improved documentation and examples

### Version 1.0.0
- Initial release
- Full feature implementation
- Comprehensive customization options
- Auto-assignment system
- Image processing and caching