# buffer-background

Display images as buffer backgrounds in GNU Emacs.

## Overview

`buffer-background` is an Emacs package that allows you to display images as backgrounds for your buffers. It supports various image formats (PNG, JPEG, SVG, GIF, BMP, TIFF) and provides comprehensive customization options including transparency, scaling modes, positioning, and automatic assignment to specific buffers.

## Features

- **Zero Dependencies**: Uses only built-in Emacs image support
- **Multiple Image Formats**: PNG, JPEG, SVG, GIF, BMP, TIFF
- **Flexible Scaling**: Fit, fill, tile, or actual size modes
- **Transparency Control**: Adjustable opacity levels
- **Auto-Assignment**: Automatically apply backgrounds to specific buffers
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
  ;; Set default image file
  (buffer-background-image-file "~/Pictures/background.png")
  ;; Set transparency level (0.0 = fully transparent, 1.0 = fully opaque)
  (buffer-background-opacity 0.3)
  ;; Set scaling mode (fit, fill, tile, actual)
  (buffer-background-scale 'fit)
  ;; Enable grayscale conversion
  (buffer-background-grayscale nil)
  ;; Set buffers for automatic background assignment
  (buffer-background-auto-buffers '("*scratch*" "*Messages*" "^\\*Help.*\\*$"))
  ;; Enable automatic assignment
  (buffer-background-auto-enable t)
  :config
  ;; Enable global mode for automatic buffer assignment
  (buffer-background-global-mode 1)
  
  ;; Optional: Set up keybindings
  :bind (("C-c b s" . buffer-background-select-image)
         ("C-c b t" . buffer-background-toggle)
         ("C-c b c" . buffer-background-clear)
         ("C-c b o" . buffer-background-set-opacity)
         ("C-c b g" . buffer-background-toggle-grayscale)
         ("C-c b r" . buffer-background-reload))
  
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

### Convenience Commands

| Command | Description |
|---------|-------------|
| `buffer-background-enable-for-scratch` | Enable background for *scratch* buffer |
| `buffer-background-enable-for-messages` | Enable background for *Messages* buffer |

## Customization Options

Access customization via `M-x customize-group RET buffer-background RET` or set variables directly:

### Core Settings

```elisp
;; Default image file
(setq buffer-background-image-file "~/Pictures/background.png")

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
;; Buffers for automatic background assignment
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

The `buffer-background-auto-buffers` list accepts:

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

### Version 1.0.0
- Initial release
- Full feature implementation
- Comprehensive customization options
- Auto-assignment system
- Image processing and caching