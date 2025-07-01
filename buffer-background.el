;;; buffer-background.el --- Display images as buffer backgrounds -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: TJ <tj@emacs.su>
;; Version: 1.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, faces, multimedia
;; URL: https://github.com/theesfeld/buffer-background

;;; Commentary:

;; This package provides functionality to display images as buffer backgrounds
;; in GNU Emacs. It supports various image formats (PNG, SVG, JPEG) and offers
;; comprehensive customization options including transparency, scaling, positioning,
;; and automatic assignment to specific buffers based on buffer name, mode, or
;; custom predicates.

;; Usage:
;;   (require 'buffer-background)
;;   (buffer-background-mode 1)  ; Enable in current buffer
;;   (buffer-background-select-image)  ; Choose image interactively
;;   
;; Automatic assignment by buffer criteria:
;;   (setq buffer-background-image-alist
;;         '(("*scratch*" . "~/images/scratch.png")
;;           ("\\*Messages\\*" . "~/images/messages.png")
;;           (org-mode . "~/images/org.jpg")
;;           ((mode . python-mode) . "~/images/python.svg")
;;           ((file . "txt") . "~/images/text.png")))
;;   (buffer-background-global-mode 1)  ; Enable auto-assignment
;;   
;; Legacy automatic assignment:
;;   (setq buffer-background-auto-buffers '("*scratch*" "*Messages*"))
;;   (buffer-background-global-mode 1)  ; Enable auto-assignment

;; Customization:
;;   M-x customize-group RET buffer-background RET

;;; Code:

(require 'image)
(require 'cl-lib)

;;; Customization Group

(defgroup buffer-background nil
  "Display images as buffer backgrounds."
  :group 'convenience
  :group 'faces
  :prefix "buffer-background-")

;;; Customization Variables

(defcustom buffer-background-image-file nil
  "Default image file to use as buffer background.
When nil, no background image is displayed."
  :type '(choice (const :tag "No image" nil)
                 (file :tag "Image file"))
  :group 'buffer-background)

(defcustom buffer-background-image-alist nil
  "Alist mapping buffer criteria to background images.
Each element should be a cons cell (CRITERIA . IMAGE-FILE), where:

CRITERIA can be:
- A string: matches buffer name exactly
- A regexp: matches buffer name by pattern (detected by regexp chars)
- A symbol: matches major-mode
- A cons cell (mode . SYMBOL): matches specific major mode
- A cons cell (name . STRING/REGEXP): matches buffer name
- A cons cell (file . EXTENSION): matches file extension
- A function: predicate that receives buffer and returns non-nil

IMAGE-FILE is the path to the image file to use.

Example:
  \\='((\"*scratch*\" . \"~/images/scratch.png\")
    (\"\\\\*Help.*\\\\*\" . \"~/images/help.png\")
    (org-mode . \"~/images/org.jpg\")
    ((mode . python-mode) . \"~/images/python.svg\")
    ((file . \"txt\") . \"~/images/text.png\")
    ((lambda (buf) (tramp-tramp-file-p default-directory)) . \"~/images/remote.png\"))"
  :type '(alist :key-type (choice (string :tag "Buffer name")
                                  (regexp :tag "Buffer pattern")
                                  (symbol :tag "Major mode")
                                  (cons :tag "Specific match"
                                        (choice (const :tag "Mode" mode)
                                                (const :tag "Buffer name" name)
                                                (const :tag "File extension" file))
                                        (choice string regexp symbol))
                                  (function :tag "Predicate function"))
                :value-type (file :tag "Image file"))
  :group 'buffer-background)

(defcustom buffer-background-opacity 0.3
  "Opacity level for buffer background images.
A float between 0.0 (fully transparent) and 1.0 (fully opaque)."
  :type '(float :tag "Opacity")
  :group 'buffer-background)

(defcustom buffer-background-scale 'fit
  "How to scale the background image.
- `fit': Scale image to fit buffer dimensions while preserving aspect ratio
- `fill': Scale image to fill buffer dimensions, may crop image
- `tile': Tile the image at original size
- `actual': Display image at actual size"
  :type '(choice (const :tag "Fit to buffer" fit)
                 (const :tag "Fill buffer" fill)
                 (const :tag "Tile image" tile)
                 (const :tag "Actual size" actual))
  :group 'buffer-background)

(defcustom buffer-background-position 'center
  "Position of the background image within the buffer.
Only applies when using `actual' scaling."
  :type '(choice (const :tag "Center" center)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top-left" top-left)
                 (const :tag "Top-right" top-right)
                 (const :tag "Bottom-left" bottom-left)
                 (const :tag "Bottom-right" bottom-right))
  :group 'buffer-background)

(defcustom buffer-background-auto-buffers nil
  "List of buffer name patterns for automatic background assignment.
Each element can be a string (exact match) or a regexp pattern.
Example: \\='(\"*scratch*\" \"*Messages*\" \"^\\\\*Help.*\\\\*$\")"
  :type '(repeat (string :tag "Buffer name pattern"))
  :group 'buffer-background)

(defcustom buffer-background-grayscale nil
  "Convert background images to grayscale when non-nil."
  :type 'boolean
  :group 'buffer-background)

(defcustom buffer-background-blur 0
  "Blur level for background images.
An integer between 0 (no blur) and 10 (maximum blur)."
  :type '(integer :tag "Blur level")
  :group 'buffer-background)

(defcustom buffer-background-margin 0
  "Margin around the background image in pixels."
  :type '(integer :tag "Margin")
  :group 'buffer-background)

(defcustom buffer-background-auto-enable t
  "Enable automatic background assignment for matching buffers."
  :type 'boolean
  :group 'buffer-background)

;;; Internal Variables

(defvar-local buffer-background--overlay nil
  "Overlay used to display the background image in the current buffer.")

(defvar buffer-background--image-cache (make-hash-table :test 'equal)
  "Cache for processed background images.")

;;; Utility Functions

(defun buffer-background--clear-cache ()
  "Clear the image cache."
  (clrhash buffer-background--image-cache))

(defun buffer-background--buffer-matches-pattern-p (buffer-name pattern)
  "Check if BUFFER-NAME matches PATTERN.
PATTERN can be a string (exact match) or regexp."
  (if (string-match-p "\\[\\]\\\\^$.*+?{}()|" pattern)
      (string-match-p pattern buffer-name)
    (string-equal buffer-name pattern)))

(defun buffer-background--should-auto-enable-p (buffer-name)
  "Check if background should be automatically enabled for BUFFER-NAME."
  (and buffer-background-auto-enable
       (or ;; Check if buffer matches auto-buffers list
           (and buffer-background-auto-buffers
                (cl-some (lambda (pattern)
                           (buffer-background--buffer-matches-pattern-p buffer-name pattern))
                         buffer-background-auto-buffers))
           ;; Check if buffer has an image in the alist
           (buffer-background--find-image-for-buffer (current-buffer)))))

(defun buffer-background--match-criteria-p (criteria buffer)
  "Check if CRITERIA matches BUFFER.
CRITERIA can be a string, regexp, symbol, cons cell, or function."
  (cond
   ;; String: exact buffer name match or regexp pattern
   ((stringp criteria)
    (if (string-match-p "\\[\\]\\\\^$.*+?{}()|" criteria)
        (string-match-p criteria (buffer-name buffer))
      (string-equal criteria (buffer-name buffer))))
   
   ;; Symbol: major mode match
   ((symbolp criteria)
    (eq criteria (buffer-local-value 'major-mode buffer)))
   
   ;; Cons cell: specific match type
   ((consp criteria)
    (pcase (car criteria)
      ('mode (eq (cdr criteria) (buffer-local-value 'major-mode buffer)))
      ('name (let ((name (cdr criteria)))
               (if (stringp name)
                   (if (string-match-p "\\[\\]\\\\^$.*+?{}()|" name)
                       (string-match-p name (buffer-name buffer))
                     (string-equal name (buffer-name buffer)))
                 nil)))
      ('file (let ((file (buffer-file-name buffer)))
               (and file
                    (string-equal (file-name-extension file)
                                  (cdr criteria)))))
      (_ nil)))
   
   ;; Function: predicate
   ((functionp criteria)
    (with-current-buffer buffer
      (funcall criteria buffer)))
   
   (t nil)))

(defun buffer-background--find-image-for-buffer (&optional buffer)
  "Find the appropriate background image for BUFFER.
Returns the image file path or nil. BUFFER defaults to current buffer."
  (let ((buffer (or buffer (current-buffer))))
    ;; First check buffer-local setting
    (or (buffer-local-value 'buffer-background-image-file buffer)
        ;; Then check the alist
        (when buffer-background-image-alist
          (cl-loop for (criteria . image) in buffer-background-image-alist
                   when (buffer-background--match-criteria-p criteria buffer)
                   return image))
        ;; Finally fall back to default
        buffer-background-image-file)))

;;; Image Processing Functions

(defun buffer-background--process-image (image-file)
  "Process IMAGE-FILE according to current customization settings.
Returns a processed image specification."
  (when (and image-file (file-exists-p image-file))
    (let* ((cache-key (list image-file
                           buffer-background-opacity
                           buffer-background-scale
                           buffer-background-grayscale
                           buffer-background-blur))
           (cached-image (gethash cache-key buffer-background--image-cache)))
      (if cached-image
          cached-image
        (let* ((base-image (create-image image-file nil nil :ascent 'center))
               (processed-image (buffer-background--apply-transformations base-image)))
          (puthash cache-key processed-image buffer-background--image-cache)
          processed-image)))))

(defun buffer-background--apply-transformations (image)
  "Apply transformations to IMAGE based on customization settings."
  (let ((spec (copy-sequence (cdr image))))
    ;; Apply grayscale conversion
    (when buffer-background-grayscale
      (setq spec (plist-put spec :conversion 'laplace)))
    
    ;; Apply opacity (using mask)
    (when (and buffer-background-opacity (< buffer-background-opacity 1.0))
      (setq spec (plist-put spec :mask 'heuristic)))
    
    ;; Apply scaling
    (let ((dimensions (buffer-background--calculate-dimensions)))
      (when dimensions
        (setq spec (plist-put spec :width (car dimensions)))
        (setq spec (plist-put spec :height (cdr dimensions)))))
    
    (cons (car image) spec)))

(defun buffer-background--calculate-dimensions ()
  "Calculate image dimensions based on buffer size and scaling mode."
  (let* ((window (get-buffer-window (current-buffer)))
         (window-width (and window (window-text-width window t)))
         (window-height (and window (window-text-height window t))))
    (when (and window-width window-height)
      (pcase buffer-background-scale
        ('fit
         (cons window-width window-height))
        ('fill
         (cons window-width window-height))
        ('tile
         nil) ; No scaling for tiling
        ('actual
         nil))))) ; No scaling for actual size

;;; Overlay Management

(defun buffer-background--create-overlay (image)
  "Create or update overlay with IMAGE in the current buffer."
  (when image
    ;; Remove existing overlay
    (buffer-background--remove-overlay)
    
    ;; Create new overlay
    (let ((overlay (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put overlay 'buffer-background t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'priority -100) ; Low priority to stay in background
      
      ;; Create the image display string
      (let* ((image-string (propertize " " 'display image))
             (background-string (if (eq buffer-background-scale 'tile)
                                   (buffer-background--create-tiled-string image-string)
                                 image-string)))
        (overlay-put overlay 'before-string background-string))
      
      (setq buffer-background--overlay overlay))))

(defun buffer-background--create-tiled-string (image-string)
  "Create a tiled version of IMAGE-STRING to fill the buffer."
  (let* ((window (get-buffer-window (current-buffer)))
         (window-width (and window (window-text-width window t)))
         (window-height (and window (window-text-height window t))))
    (if (and window-width window-height)
        (let ((lines nil))
          (dotimes (_ (ceiling (/ window-height 20))) ; Approximate line height
            (let ((line ""))
              (dotimes (_ (ceiling (/ window-width 20))) ; Approximate char width
                (setq line (concat line image-string)))
              (push (concat line "\n") lines)))
          (apply #'concat (nreverse lines)))
      image-string)))

(defun buffer-background--remove-overlay ()
  "Remove background overlay from current buffer."
  (when (and buffer-background--overlay 
             (overlay-buffer buffer-background--overlay))
    (delete-overlay buffer-background--overlay)
    (setq buffer-background--overlay nil)))

;;; Minor Mode Definition

(define-minor-mode buffer-background-mode
  "Toggle buffer background image display.
When enabled, displays an image as the background of the current buffer."
  :lighter " BG"
  :group 'buffer-background
  (if buffer-background-mode
      (buffer-background--enable)
    (buffer-background--disable)))

(defun buffer-background--enable ()
  "Enable buffer background in current buffer."
  (when-let ((image-file (buffer-background--find-image-for-buffer)))
    (when-let ((image (buffer-background--process-image image-file)))
      (buffer-background--create-overlay image)
      ;; Add hooks to update on window changes
      (add-hook 'window-size-change-functions #'buffer-background--update-overlay-hook nil t)
      (add-hook 'window-configuration-change-hook #'buffer-background--update-overlay nil t))))

(defun buffer-background--disable ()
  "Disable buffer background in current buffer."
  (buffer-background--remove-overlay)
  (remove-hook 'window-size-change-functions #'buffer-background--update-overlay-hook t)
  (remove-hook 'window-configuration-change-hook #'buffer-background--update-overlay t))

(defun buffer-background--update-overlay ()
  "Update overlay when window configuration changes."
  (when (and buffer-background-mode buffer-background--overlay)
    (buffer-background--enable)))

(defun buffer-background--update-overlay-hook (frame)
  "Hook function to update overlay when window size changes."
  (when (eq frame (selected-frame))
    (buffer-background--update-overlay)))

;;; Auto-Assignment System

(defvar buffer-background--auto-assignment-timer nil
  "Timer for checking buffer auto-assignment.")

(define-minor-mode buffer-background-global-mode
  "Global mode for automatic buffer background assignment."
  :global t
  :group 'buffer-background
  (if buffer-background-global-mode
      (buffer-background--enable-global)
    (buffer-background--disable-global)))

(defun buffer-background--enable-global ()
  "Enable global buffer background auto-assignment."
  (add-hook 'buffer-list-update-hook #'buffer-background--check-auto-assignment)
  (add-hook 'after-change-major-mode-hook #'buffer-background--check-current-buffer)
  ;; Check existing buffers
  (buffer-background--check-all-buffers))

(defun buffer-background--disable-global ()
  "Disable global buffer background auto-assignment."
  (remove-hook 'buffer-list-update-hook #'buffer-background--check-auto-assignment)
  (remove-hook 'after-change-major-mode-hook #'buffer-background--check-current-buffer)
  (when buffer-background--auto-assignment-timer
    (cancel-timer buffer-background--auto-assignment-timer)
    (setq buffer-background--auto-assignment-timer nil)))

(defun buffer-background--check-auto-assignment ()
  "Check if any buffers need auto-assignment of backgrounds."
  ;; Use a timer to avoid excessive checking
  (unless buffer-background--auto-assignment-timer
    (setq buffer-background--auto-assignment-timer
          (run-with-idle-timer 0.5 nil #'buffer-background--do-auto-assignment))))

(defun buffer-background--do-auto-assignment ()
  "Perform auto-assignment check for all buffers."
  (setq buffer-background--auto-assignment-timer nil)
  (buffer-background--check-all-buffers))

(defun buffer-background--check-all-buffers ()
  "Check all buffers for auto-assignment eligibility."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (buffer-background--check-current-buffer))))

(defun buffer-background--check-current-buffer ()
  "Check if current buffer should have background auto-enabled."
  (when (and (not buffer-background-mode)
             (buffer-background--should-auto-enable-p (buffer-name)))
    (buffer-background-mode 1)))

;;; Interactive Commands

;;;###autoload
(defun buffer-background-set-image (image-file)
  "Set background IMAGE-FILE for the current buffer."
  (interactive "fSelect background image: ")
  (setq-local buffer-background-image-file image-file)
  (when buffer-background-mode
    (buffer-background--enable))
  (unless buffer-background-mode
    (buffer-background-mode 1))
  (message "Background image set: %s" (file-name-nondirectory image-file)))

;;;###autoload
(defun buffer-background-select-image ()
  "Interactively select and set background image for current buffer."
  (interactive)
  (let ((image-file (read-file-name "Select background image: " nil nil t nil
                                   (lambda (name)
                                     (string-match-p "\\.[pP][nN][gG]\\|\\.[jJ][pP][eE]?[gG]\\|\\.[sS][vV][gG]\\|\\.[gG][iI][fF]\\|\\.[bB][mM][pP]\\|\\.[tT][iI][fF][fF]?\\'" name)))))
    (buffer-background-set-image image-file)))

;;;###autoload
(defun buffer-background-toggle ()
  "Toggle buffer background mode in current buffer."
  (interactive)
  (buffer-background-mode 'toggle)
  (message "Buffer background %s" (if buffer-background-mode "enabled" "disabled")))

;;;###autoload
(defun buffer-background-clear ()
  "Clear background image from current buffer."
  (interactive)
  (setq-local buffer-background-image-file nil)
  (when buffer-background-mode
    (buffer-background-mode -1))
  (message "Background image cleared"))

;;;###autoload
(defun buffer-background-apply-to-buffer (buffer-name image-file)
  "Apply background IMAGE-FILE to buffer named BUFFER-NAME."
  (interactive 
   (list (read-buffer "Apply background to buffer: " nil t)
         (read-file-name "Select background image: " nil nil t nil
                        (lambda (name)
                          (string-match-p "\\.[pP][nN][gG]\\|\\.[jJ][pP][eE]?[gG]\\|\\.[sS][vV][gG]\\|\\.[gG][iI][fF]\\|\\.[bB][mM][pP]\\|\\.[tT][iI][fF][fF]?\\'" name)))))
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (buffer-background-set-image image-file))
      (error "Buffer %s does not exist" buffer-name))))

;;;###autoload
(defun buffer-background-set-opacity (opacity)
  "Set background image OPACITY for current buffer."
  (interactive "nOpacity (0.0-1.0): ")
  (setq opacity (max 0.0 (min 1.0 opacity)))
  (setq-local buffer-background-opacity opacity)
  (when buffer-background-mode
    (buffer-background--clear-cache)
    (buffer-background--enable))
  (message "Background opacity set to %.2f" opacity))

;;;###autoload
(defun buffer-background-set-scale (scale)
  "Set background image SCALE mode for current buffer."
  (interactive (list (intern (completing-read "Scale mode: " 
                                             '("fit" "fill" "tile" "actual") 
                                             nil t))))
  (setq-local buffer-background-scale scale)
  (when buffer-background-mode
    (buffer-background--clear-cache)
    (buffer-background--enable))
  (message "Background scale set to %s" scale))

;;;###autoload
(defun buffer-background-toggle-grayscale ()
  "Toggle grayscale mode for background image in current buffer."
  (interactive)
  (setq-local buffer-background-grayscale (not buffer-background-grayscale))
  (when buffer-background-mode
    (buffer-background--clear-cache)
    (buffer-background--enable))
  (message "Background grayscale %s" (if buffer-background-grayscale "enabled" "disabled")))

;;;###autoload
(defun buffer-background-clear-cache ()
  "Clear the image processing cache."
  (interactive)
  (buffer-background--clear-cache)
  (message "Image cache cleared"))

;;;###autoload
(defun buffer-background-reload ()
  "Reload the background image in current buffer."
  (interactive)
  (when buffer-background-mode
    (buffer-background--clear-cache)
    (buffer-background--enable)
    (message "Background image reloaded")))

;;;###autoload
(defun buffer-background-show-image-source ()
  "Show which image would be used for the current buffer."
  (interactive)
  (let ((image-file (buffer-background--find-image-for-buffer)))
    (if image-file
        (message "Background image for %s: %s" (buffer-name) image-file)
      (message "No background image configured for %s" (buffer-name)))))

;;; Convenience Functions

(defun buffer-background-enable-for-scratch ()
  "Enable buffer background for *scratch* buffer."
  (interactive)
  (when-let ((scratch-buffer (get-buffer "*scratch*")))
    (with-current-buffer scratch-buffer
      (call-interactively #'buffer-background-select-image))))

(defun buffer-background-enable-for-messages ()
  "Enable buffer background for *Messages* buffer."
  (interactive)
  (when-let ((messages-buffer (get-buffer "*Messages*")))
    (with-current-buffer messages-buffer
      (call-interactively #'buffer-background-select-image))))

;;; Documentation and Examples

;; Usage Examples:
;;
;; Basic usage:
;;   (require 'buffer-background)
;;   (buffer-background-select-image)  ; Choose image for current buffer
;;   (buffer-background-toggle)        ; Toggle background on/off
;;
;; Set up automatic backgrounds using buffer criteria:
;;   (setq buffer-background-image-alist
;;         '(;; Exact buffer name matches
;;           ("*scratch*" . "~/images/scratch.png")
;;           ("*Messages*" . "~/images/messages.png")
;;           
;;           ;; Buffer name patterns (regexp)
;;           ("\\*Help.*\\*" . "~/images/help.png")
;;           ("\\*Compile.*\\*" . "~/images/compile.png")
;;           
;;           ;; Major mode matches
;;           (org-mode . "~/images/org.jpg")
;;           (python-mode . "~/images/python.svg")
;;           
;;           ;; Specific mode matches (alternative syntax)
;;           ((mode . js-mode) . "~/images/javascript.png")
;;           ((mode . typescript-mode) . "~/images/typescript.png")
;;           
;;           ;; File extension matches
;;           ((file . "txt") . "~/images/text.png")
;;           ((file . "md") . "~/images/markdown.png")
;;           
;;           ;; Custom predicates
;;           ((lambda (buf)
;;              (string-prefix-p "/ssh:" default-directory))
;;            . "~/images/remote.png")
;;           ((lambda (buf) 
;;              (bound-and-true-p compilation-mode))
;;            . "~/images/build.png")))
;;   (buffer-background-global-mode 1)
;;
;; Use-package configuration:
;;   (use-package buffer-background
;;     :config
;;     (setq buffer-background-image-alist
;;           '(("*scratch*" . "~/images/scratch.png")
;;             ("\\*Messages\\*" . "~/images/messages.png")
;;             (org-mode . "~/images/org.jpg")
;;             ((mode . python-mode) . "~/images/python.svg")
;;             ((file . "txt") . "~/images/text.png")
;;             ;; TRAMP remote files
;;             ((lambda (buf)
;;                (file-remote-p default-directory))
;;              . "~/images/remote.png")))
;;     (setq buffer-background-opacity 0.3)
;;     (setq buffer-background-scale 'fit)
;;     (buffer-background-global-mode 1))
;;
;; Legacy automatic backgrounds:
;;   (setq buffer-background-auto-buffers '("*scratch*" "*Messages*"))
;;   (setq buffer-background-image-file "~/Pictures/background.png")
;;   (buffer-background-global-mode 1)
;;
;; Customize appearance:
;;   (setq buffer-background-opacity 0.2)      ; More transparent
;;   (setq buffer-background-scale 'tile)      ; Tile the image
;;   (setq buffer-background-grayscale t)      ; Convert to grayscale
;;
;; Interactive commands:
;;   M-x buffer-background-select-image        ; Choose image file
;;   M-x buffer-background-set-opacity         ; Set transparency
;;   M-x buffer-background-set-scale           ; Set scaling mode
;;   M-x buffer-background-toggle-grayscale   ; Toggle grayscale
;;   M-x buffer-background-apply-to-buffer    ; Apply to specific buffer
;;   M-x buffer-background-clear              ; Remove background
;;   M-x buffer-background-show-image-source  ; Show which image would be used
;;
;; Programmatic usage:
;;   (with-current-buffer "*scratch*"
;;     (buffer-background-set-image "~/Pictures/my-bg.png")
;;     (setq-local buffer-background-opacity 0.3)
;;     (buffer-background-mode 1))

;;; Hooks and Customization

(defcustom buffer-background-mode-hook nil
  "Hook run when buffer-background-mode is enabled or disabled."
  :type 'hook
  :group 'buffer-background)

(defcustom buffer-background-before-enable-hook nil
  "Hook run before enabling buffer background in a buffer."
  :type 'hook
  :group 'buffer-background)

(defcustom buffer-background-after-enable-hook nil
  "Hook run after enabling buffer background in a buffer."
  :type 'hook
  :group 'buffer-background)

;; Add hooks to the enable/disable functions
(advice-add 'buffer-background--enable :before 
            (lambda () (run-hooks 'buffer-background-before-enable-hook)))
(advice-add 'buffer-background--enable :after 
            (lambda () (run-hooks 'buffer-background-after-enable-hook)))

;;; Footer

(provide 'buffer-background)

;;; buffer-background.el ends here
