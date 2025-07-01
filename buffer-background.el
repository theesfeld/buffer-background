;;; buffer-background.el --- Display images as buffer backgrounds -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: TJ <tj@emacs.su>
;; Version: 2.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, faces, multimedia
;; URL: https://github.com/theesfeld/buffer-background

;;; Commentary:

;; This package provides functionality to display images or colors as buffer 
;; backgrounds in GNU Emacs. It supports various image formats (PNG, SVG, JPEG) 
;; and solid colors, with comprehensive customization options including transparency,
;; scaling, positioning, rotation, and automatic assignment to specific buffers 
;; based on buffer name, mode, file extension, or custom predicates.

;; Usage:
;;   (require 'buffer-background)
;;   (buffer-background-mode 1)  ; Enable in current buffer
;;   (buffer-background-select-image)  ; Choose image interactively
;;   
;; Automatic assignment with per-buffer settings:
;;   (setq buffer-background-image-alist
;;         '(;; Simple image assignment
;;           ("*scratch*" . "~/images/scratch.png")
;;           
;;           ;; Color background
;;           ("*Messages*" . "#1a1a1a")
;;           
;;           ;; Image with custom settings
;;           (org-mode . (:image "~/images/org.jpg"
;;                        :opacity 0.2
;;                        :grayscale t))
;;           
;;           ;; Color with opacity
;;           ((mode . python-mode) . (:color "#002b36"
;;                                    :opacity 0.8))
;;           
;;           ;; Tiled pattern with low opacity
;;           ((file . "txt") . (:image "~/images/pattern.png"
;;                              :scale tile
;;                              :opacity 0.1))))
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
  "Alist mapping buffer criteria to background specifications.
Each element should be a cons cell (CRITERIA . SPEC), where:

CRITERIA can be:
- A string: matches buffer name exactly
- A regexp: matches buffer name by pattern (detected by regexp chars)
- A symbol: matches major-mode
- A cons cell (mode . SYMBOL): matches specific major mode
- A cons cell (name . STRING/REGEXP): matches buffer name
- A cons cell (file . EXTENSION): matches file extension
- A function: predicate that receives buffer and returns non-nil

SPEC can be:
- A string: path to an image file
- A color string: hex color like \"#1a1a1a\" or color name like \"dark slate gray\"
- A plist: detailed specification with properties:
  :image FILE - image file path
  :color COLOR - background color (hex or name)
  :opacity FLOAT - opacity level (0.0-1.0)
  :scale SYMBOL - scaling mode (fit, fill, tile, actual)
  :position SYMBOL - image position (center, top, bottom, etc.)
  :grayscale BOOLEAN - convert to grayscale
  :blur INTEGER - blur level (0-10)
  :margin INTEGER - margin in pixels
  :brightness FLOAT - brightness adjustment (0.0-2.0, 1.0 = normal)
  :contrast FLOAT - contrast adjustment (0.0-2.0, 1.0 = normal)
  :rotation DEGREES - rotation angle (0, 90, 180, 270)

Example:
  \\='((\"*scratch*\" . \"~/images/scratch.png\")
    (\"*Messages*\" . \"#1a1a1a\")  ; Dark background color
    (org-mode . (:image \"~/images/org.jpg\"
                 :opacity 0.2
                 :grayscale t))
    ((mode . python-mode) . (:color \"#002b36\"  ; Solarized dark
                             :opacity 0.8))
    ((file . \"txt\") . (:image \"~/images/text.png\"
                         :scale tile
                         :opacity 0.1))
    ((lambda (buf) (file-remote-p default-directory))
     . (:image \"~/images/remote.png\"
        :brightness 0.7
        :blur 2)))"
  :type '(alist :key-type (choice (string :tag "Buffer name")
                                  (regexp :tag "Buffer pattern")
                                  (symbol :tag "Major mode")
                                  (cons :tag "Specific match"
                                        (choice (const :tag "Mode" mode)
                                                (const :tag "Buffer name" name)
                                                (const :tag "File extension" file))
                                        (choice string regexp symbol))
                                  (function :tag "Predicate function"))
                :value-type (choice (file :tag "Image file")
                                   (string :tag "Color (hex or name)")
                                   (plist :tag "Detailed specification"
                                          :options ((:image (file :tag "Image file"))
                                                   (:color (string :tag "Background color"))
                                                   (:opacity (float :tag "Opacity (0.0-1.0)"))
                                                   (:scale (choice (const fit)
                                                                  (const fill)
                                                                  (const tile)
                                                                  (const actual)))
                                                   (:position (choice (const center)
                                                                     (const top)
                                                                     (const bottom)
                                                                     (const left)
                                                                     (const right)
                                                                     (const top-left)
                                                                     (const top-right)
                                                                     (const bottom-left)
                                                                     (const bottom-right)))
                                                   (:grayscale (boolean :tag "Convert to grayscale"))
                                                   (:blur (integer :tag "Blur level (0-10)"))
                                                   (:margin (integer :tag "Margin in pixels"))
                                                   (:brightness (float :tag "Brightness (0.0-2.0)"))
                                                   (:contrast (float :tag "Contrast (0.0-2.0)"))
                                                   (:rotation (choice (const 0)
                                                                     (const 90)
                                                                     (const 180)
                                                                     (const 270)))))))
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

(defvar-local buffer-background--current-spec nil
  "Current background specification for this buffer.")

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
           ;; Check if buffer has a spec in the alist
           (buffer-background--find-spec-for-buffer (current-buffer)))))

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

(defun buffer-background--find-spec-for-buffer (&optional buffer)
  "Find the appropriate background specification for BUFFER.
Returns a normalized plist specification or nil. BUFFER defaults to current buffer."
  (let ((buffer (or buffer (current-buffer)))
        (spec nil))
    ;; First check buffer-local setting
    (when-let ((local-file (buffer-local-value 'buffer-background-image-file buffer)))
      (setq spec (list :image local-file)))
    
    ;; Then check the alist
    (unless spec
      (when buffer-background-image-alist
        (cl-loop for (criteria . value) in buffer-background-image-alist
                 when (buffer-background--match-criteria-p criteria buffer)
                 do (setq spec (buffer-background--normalize-spec value))
                 and return nil)))
    
    ;; Finally fall back to default
    (unless spec
      (when buffer-background-image-file
        (setq spec (list :image buffer-background-image-file))))
    
    ;; Apply global defaults to spec
    (when spec
      (buffer-background--apply-defaults spec))))

(defun buffer-background--normalize-spec (spec)
  "Normalize SPEC into a plist format.
SPEC can be a string (image file or color), or a plist."
  (cond
   ;; Already a plist
   ((and (listp spec) (keywordp (car spec)))
    spec)
   ;; String - determine if it's a color or file
   ((stringp spec)
    (if (or (string-match-p "^#[0-9a-fA-F]\\{6\\}$" spec)  ; Hex color
            (color-defined-p spec))                          ; Named color
        (list :color spec)
      (list :image spec)))
   (t nil)))

(defun buffer-background--apply-defaults (spec)
  "Apply global default values to SPEC where not specified."
  (let ((result (copy-sequence spec)))
    ;; Only apply defaults if not already specified in spec
    (unless (plist-member result :opacity)
      (setq result (plist-put result :opacity buffer-background-opacity)))
    (unless (plist-member result :scale)
      (setq result (plist-put result :scale buffer-background-scale)))
    (unless (plist-member result :position)
      (setq result (plist-put result :position buffer-background-position)))
    (unless (plist-member result :grayscale)
      (setq result (plist-put result :grayscale buffer-background-grayscale)))
    (unless (plist-member result :blur)
      (setq result (plist-put result :blur buffer-background-blur)))
    (unless (plist-member result :margin)
      (setq result (plist-put result :margin buffer-background-margin)))
    (unless (plist-member result :brightness)
      (setq result (plist-put result :brightness 1.0)))
    (unless (plist-member result :contrast)
      (setq result (plist-put result :contrast 1.0)))
    (unless (plist-member result :rotation)
      (setq result (plist-put result :rotation 0)))
    result))

;;; Image Processing Functions

(defun buffer-background--create-color-background (color opacity)
  "Create a color background with COLOR and OPACITY.
Uses Emacs built-in SVG support to create a colored rectangle."
  (require 'svg)
  (let* ((window (get-buffer-window (current-buffer)))
         (width (or (and window (window-text-width window t)) 80))
         (height (or (and window (window-text-height window t)) 40))
         (svg (svg-create width height)))
    ;; Convert color to RGB values
    (let ((rgb (color-name-to-rgb color)))
      (when rgb
        (svg-rectangle svg 0 0 width height
                       :fill color
                       :fill-opacity opacity)))
    (svg-image svg)))

(defun buffer-background--process-spec (spec)
  "Process background SPEC into a display specification.
SPEC is a plist with :image or :color and other properties."
  (cond
   ;; Color background
   ((plist-get spec :color)
    (buffer-background--create-color-background
     (plist-get spec :color)
     (plist-get spec :opacity)))
   
   ;; Image background
   ((plist-get spec :image)
    (buffer-background--process-image-with-spec spec))
   
   (t nil)))

(defun buffer-background--process-image-with-spec (spec)
  "Process image according to SPEC plist.
Returns a processed image specification."
  (let ((image-file (plist-get spec :image)))
    (when (and image-file (file-exists-p image-file))
      (let* ((cache-key (list image-file spec))
             (cached-image (gethash cache-key buffer-background--image-cache)))
        (if cached-image
            cached-image
          (let* ((base-image (create-image image-file nil nil :ascent 'center))
                 (processed-image (buffer-background--apply-transformations-with-spec 
                                  base-image spec)))
            (puthash cache-key processed-image buffer-background--image-cache)
            processed-image))))))

(defun buffer-background--apply-transformations-with-spec (image spec)
  "Apply transformations to IMAGE based on SPEC plist."
  (let ((img-spec (copy-sequence (cdr image)))
        (grayscale (plist-get spec :grayscale))
        (opacity (plist-get spec :opacity))
        (scale (plist-get spec :scale))
        (rotation (plist-get spec :rotation))
        (brightness (plist-get spec :brightness))
        (contrast (plist-get spec :contrast)))
    
    ;; Apply grayscale conversion
    (when grayscale
      (setq img-spec (plist-put img-spec :conversion 'laplace)))
    
    ;; Apply opacity (using mask)
    (when (and opacity (< opacity 1.0))
      (setq img-spec (plist-put img-spec :mask 'heuristic)))
    
    ;; Apply rotation
    (when (and rotation (not (zerop rotation)))
      (setq img-spec (plist-put img-spec :rotation rotation)))
    
    ;; Apply scaling
    (let ((dimensions (buffer-background--calculate-dimensions-with-spec spec)))
      (when dimensions
        (setq img-spec (plist-put img-spec :width (car dimensions)))
        (setq img-spec (plist-put img-spec :height (cdr dimensions)))))
    
    ;; Note: Brightness and contrast adjustments would require image manipulation
    ;; which is not directly supported by Emacs built-in image display.
    ;; These could be implemented using external tools or ignored.
    
    (cons (car image) img-spec)))

(defun buffer-background--calculate-dimensions-with-spec (spec)
  "Calculate image dimensions based on buffer size and SPEC scaling mode."
  (let* ((window (get-buffer-window (current-buffer)))
         (window-width (and window (window-text-width window t)))
         (window-height (and window (window-text-height window t)))
         (scale (plist-get spec :scale))
         (margin (plist-get spec :margin)))
    (when (and window-width window-height)
      ;; Adjust for margins if specified
      (when (and margin (> margin 0))
        (setq window-width (- window-width (* 2 margin)))
        (setq window-height (- window-height (* 2 margin))))
      (pcase scale
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
  (when-let ((spec (buffer-background--find-spec-for-buffer)))
    (when-let ((background (buffer-background--process-spec spec)))
      (buffer-background--create-overlay background)
      ;; Store the spec for later updates
      (setq-local buffer-background--current-spec spec)
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
  "Show which background would be used for the current buffer."
  (interactive)
  (let ((spec (buffer-background--find-spec-for-buffer)))
    (if spec
        (cond
         ((plist-get spec :color)
          (message "Background for %s: color %s (opacity %.2f)" 
                   (buffer-name) 
                   (plist-get spec :color)
                   (plist-get spec :opacity)))
         ((plist-get spec :image)
          (message "Background for %s: image %s (opacity %.2f, scale %s)" 
                   (buffer-name)
                   (plist-get spec :image)
                   (plist-get spec :opacity)
                   (plist-get spec :scale)))
         (t
          (message "Background for %s: %s" (buffer-name) spec)))
      (message "No background configured for %s" (buffer-name)))))

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
;;         '(;; Simple image files
;;           ("*scratch*" . "~/images/scratch.png")
;;           
;;           ;; Color backgrounds
;;           ("*Messages*" . "#1a1a1a")
;;           ("*Warnings*" . "dark red")
;;           
;;           ;; Images with custom settings
;;           ("\\*Help.*\\*" . (:image "~/images/help.png"
;;                              :opacity 0.15
;;                              :scale fit))
;;           
;;           ;; Major modes with colors
;;           (org-mode . (:color "#002b36"  ; Solarized dark
;;                        :opacity 0.9))
;;           
;;           ;; Major modes with images and effects
;;           (python-mode . (:image "~/images/python.svg"
;;                           :opacity 0.2
;;                           :grayscale t
;;                           :scale fill))
;;           
;;           ;; File extensions with tiled patterns
;;           ((file . "txt") . (:image "~/images/paper-texture.png"
;;                              :scale tile
;;                              :opacity 0.1))
;;           
;;           ;; Custom predicates with settings
;;           ((lambda (buf)
;;              (file-remote-p default-directory))
;;            . (:image "~/images/remote.png"
;;               :opacity 0.3
;;               :blur 2))
;;           
;;           ;; Dark theme for compilation buffers
;;           ((lambda (buf) 
;;              (bound-and-true-p compilation-mode))
;;            . (:color "#0a0a0a"
;;               :opacity 0.95))))
;;   (buffer-background-global-mode 1)
;;
;; Use-package configuration:
;;   (use-package buffer-background
;;     :config
;;     (setq buffer-background-image-alist
;;           '(;; Mix of images and colors
;;             ("*scratch*" . (:image "~/images/scratch.png"
;;                             :opacity 0.2))
;;             ("\\*Messages\\*" . "#1a1a1a")
;;             (org-mode . (:color "#001f27"
;;                          :opacity 0.85))
;;             ((mode . python-mode) . (:image "~/images/python.svg"
;;                                      :opacity 0.15
;;                                      :grayscale t))
;;             ((file . "txt") . (:image "~/images/texture.png"
;;                                :scale tile
;;                                :opacity 0.1))
;;             ;; TRAMP remote files
;;             ((lambda (buf)
;;                (file-remote-p default-directory))
;;              . (:color "dark blue"
;;                 :opacity 0.9))))
;;     ;; Global defaults (can be overridden per-buffer)
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
