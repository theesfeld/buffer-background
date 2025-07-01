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
   ;; Method 1: Using buffer-background-image-alist (recommended)
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
   
   ;; Method 2: Using legacy auto-buffers (simple)
   (setq buffer-background-auto-buffers '("*scratch*" "*Messages*"))
   (setq buffer-background-image-file "~/Pictures/my-background.png")
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
;; Default/fallback image file
(setq buffer-background-image-file "~/Pictures/background.png")

;; Buffer-specific background assignments
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
;; Method 1: Using buffer-background-image-alist (recommended)
;; Automatically assigns different images based on buffer criteria
(setq buffer-background-image-alist
      '(;; Exact buffer names
        ("*scratch*" . "~/images/scratch.png")
        ("*Messages*" . "~/images/messages.png")
        
        ;; Regexp patterns
        ("\\*Help.*\\*" . "~/images/help.png")
        ("\\*Compile.*\\*" . "~/images/compile.png")
        
        ;; Major modes
        (org-mode . "~/images/org.jpg")
        (python-mode . "~/images/python.svg")
        
        ;; Alternative syntax for modes
        ((mode . js-mode) . "~/images/javascript.png")
        ((mode . typescript-mode) . "~/images/typescript.png")
        
        ;; File extensions
        ((file . "txt") . "~/images/text.png")
        ((file . "md") . "~/images/markdown.png")
        
        ;; Custom predicates
        ((lambda (buf) (file-remote-p default-directory))
         . "~/images/remote.png")
        ((lambda (buf) (bound-and-true-p compilation-mode))
         . "~/images/build.png")))

;; Method 2: Legacy auto-buffers (simple but limited)
(setq buffer-background-auto-buffers 
      '("*scratch*" 
        "*Messages*" 
        "^\\*Help.*\\*$"     ; Regexp pattern for Help buffers
        "*compilation*"))

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

### For `buffer-background-auto-buffers` (legacy):

- **Exact strings**: `"*scratch*"` matches exactly
- **Regexp patterns**: `"^\\*Help.*\\*$"` matches all Help buffers

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
2. Check `buffer-background-auto-buffers` patterns
3. Ensure `buffer-background-auto-enable` is t
4. Test pattern matching manually

## Examples

### Basic Setup

```elisp
(use-package buffer-background
  :custom
  (buffer-background-image-file "~/Pictures/subtle-pattern.png")
  (buffer-background-opacity 0.1)
  (buffer-background-scale 'tile)
  :config
  (buffer-background-global-mode 1))
```

### Advanced Configuration

```elisp
(use-package buffer-background
  :custom
  ;; Different settings for different use cases
  (buffer-background-auto-buffers 
   '("*scratch*" "*Messages*" "^\\*Help.*\\*$" "*compilation*"))
  :config
  ;; Enable global mode
  (buffer-background-global-mode 1)
  
  ;; Set specific backgrounds for different buffer types
  (defun my/setup-buffer-backgrounds ()
    "Set up custom buffer backgrounds."
    ;; Scratch buffer - coding background
    (with-current-buffer "*scratch*"
      (buffer-background-set-image "~/Pictures/code-bg.png")
      (setq-local buffer-background-opacity 0.15))
    
    ;; Messages buffer - subtle pattern
    (when (get-buffer "*Messages*")
      (with-current-buffer "*Messages*"
        (buffer-background-set-image "~/Pictures/log-bg.png")
        (setq-local buffer-background-opacity 0.1)
        (setq-local buffer-background-grayscale t))))
  
  ;; Apply custom backgrounds after global mode is enabled
  (add-hook 'buffer-background-global-mode-hook #'my/setup-buffer-backgrounds)
  
  ;; Keybindings
  :bind-keymap ("C-c b" . buffer-background-mode-map)
  :bind (("C-c b s" . buffer-background-select-image)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b c" . buffer-background-clear)))
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