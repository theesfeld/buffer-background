# Buffer Background - Color Functionality

This branch focuses **exclusively on color backgrounds** - all image functionality has been removed to ensure color backgrounds work perfectly.

## Quick Start

```elisp
;; Load the package
(load-file "buffer-background.el")

;; Set a color background for the current buffer
(buffer-background-set-color "#2d2d2d")

;; Toggle background on/off
(buffer-background-toggle)
```

## Automatic Color Assignment

Set up automatic color assignment based on buffer names, modes, or custom criteria:

```elisp
(setq buffer-background-color-alist
      '(;; Buffer names
        ("*scratch*" . "#2d2d2d")
        ("*Messages*" . "#1a1a1a")
        ("*Warnings*" . "#3d1a1a")
        
        ;; Major modes with opacity
        (org-mode . (:color "#1e1e2e" :opacity 0.9))
        (python-mode . (:color "#002b36" :opacity 0.8))
        
        ;; File extensions
        ((file . "txt") . (:color "#1c1c1c" :opacity 0.7))
        
        ;; Custom conditions
        ((lambda (buf) (file-remote-p default-directory))
         . (:color "#1a1a3d" :opacity 0.8))))

;; Enable automatic assignment
(buffer-background-global-mode 1)
```

## Interactive Commands

- `M-x buffer-background-set-color` - Set color for current buffer
- `M-x buffer-background-set-opacity` - Set transparency (0.0-1.0)
- `M-x buffer-background-toggle` - Toggle background on/off
- `M-x buffer-background-clear` - Remove background
- `M-x buffer-background-show-color-source` - Show which color would be used

## How It Works

The package uses **face remapping** (`face-remap-add-relative`) to change the background color of the `default` face in each buffer. This is much more reliable than overlays and properly integrates with Emacs' face system.

## Testing

Run the verification test:
```bash
emacs --batch --load verify-color.el
```

Load interactive tests:
```elisp
(load-file "test-colors.el")
;; Then try: M-x test-color-basics
```

## Key Improvements

1. **Removed all image functionality** - Focus solely on colors
2. **Use face remapping instead of overlays** - More reliable and proper integration
3. **Simplified API** - `buffer-background-color-alist` instead of complex image specs
4. **Verified working** - Tests confirm color backgrounds work correctly

## Color Specifications

Colors can be specified as:
- Hex colors: `"#ff0000"`, `"#2d2d2d"`
- Named colors: `"red"`, `"dark blue"`
- With opacity: `(:color "#ff0000" :opacity 0.5)`