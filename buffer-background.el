;;; buffer-background.el --- Display colors as buffer backgrounds -*- lexical-binding: t -*-

;; Author: William Theesfeld <tj@emacs.su>
;; Version: 2.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: buffer, background, faces
;; URL: https://github.com/theesfeld/buffer-background

;; Copyright (C) 2025 William Theesfeld

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides functionality to display colors as buffer
;; backgrounds in GNU Emacs.  It supports solid colors with opacity and
;; automatic assignment to specific buffers based on buffer name, mode,
;; file extension, or custom predicates.

;; Usage:
;;   (require 'buffer-background)
;;   (buffer-background-mode 1)  ; Enable in current buffer
;;   (buffer-background-set-color "#1a1a1a")  ; Set color
;;
;; Automatic assignment with per-buffer settings:
;;   (setq buffer-background-color-alist
;;         '(;; Color backgrounds
;;           ("*scratch*" . "#2d2d2d")
;;           ("*Messages*" . "#1a1a1a")
;;
;;           ;; Color with opacity
;;           ((mode . python-mode) . (:color "#002b36"
;;                                    :opacity 0.8))
;;
;;           ;; Major modes with colors
;;           (org-mode . (:color "#1e1e2e"
;;                        :opacity 0.9))))
;;   (buffer-background-global-mode 1)  ; Enable auto-assignment

;; Customization:
;;   M-x customize-group RET buffer-background RET

;;; Code:

(require 'cl-lib)
(require 'face-remap)

;;; Customizations

(defgroup buffer-background nil
  "Display colors as buffer backgrounds."
  :group 'convenience
  :group 'faces
  :prefix "buffer-background-")

;;; Customizable Variables

(defcustom buffer-background-color nil
  "Default color to use as buffer background.
When nil, no background color is displayed."
  :type '(choice (const :tag "No color" nil)
                 (string :tag "Color (hex or name)"))
  :group 'buffer-background)

(defcustom buffer-background-color-alist nil
  "Alist mapping buffer criteria to color specifications.
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
- A color string: hex color like \"#1a1a1a\" or color name
  like \"dark slate gray\"
- A plist: detailed specification with properties:
  :color COLOR - background color (hex or name)
  :opacity FLOAT - opacity level (0.0-1.0)

Example:
  \\='((\"*scratch*\" . \"#2d2d2d\")
    (\"*Messages*\" . \"#1a1a1a\")  ; Dark background color
    (org-mode . (:color \"#1e1e2e\"
                 :opacity 0.9))
    ((mode . \"python-mode\") . (:color \"#002b36\"  ; Solarized dark
                             :opacity 0.8))
    ((file . \"txt\") . (:color \"#1c1c1c\"
                        :opacity 0.7)))"
  :type '(alist :key-type (choice (string :tag "Buffer name")
                                  (regexp :tag "Buffer pattern")
                                  (symbol :tag "Major mode")
                                  (cons :tag "Specific match"
                                        (choice (const :tag "Mode" mode)
                                                (const :tag "Buffer name" name)
                                                (const :tag "File extension" file))
                                        (choice string regexp symbol))
                                  (function :tag "Predicate function"))
                :value-type (choice (string :tag "Color (hex or name)")
                                    (plist :tag "Detailed specification"
                                           :options ((:color (string :tag "Background color"))
                                                     (:opacity (float :tag "Opacity (0.0-1.0)"))))))
  :group 'buffer-background)

(defcustom buffer-background-opacity 0.3
  "Opacity level for buffer background colors.
A float between 0.0 (fully transparent) and 1.0 (fully opaque)."
  :type '(float :tag "Opacity")
  :group 'buffer-background)

(defcustom buffer-background-auto-buffers nil
  "List of buffer name patterns for automatic background assignment.
Each element can be a string (exact match) or a regexp pattern.
Example: \\='(\"*scratch*\" \"*Messages*\" \"^\\\\*Help.*\\\\*$\")"
  :type '(repeat (string :tag "Buffer name pattern"))
  :group 'buffer-background)

(defcustom buffer-background-auto-enable t
  "Enable automatic background assignment for matching buffers."
  :type 'boolean
  :group 'buffer-background)

;;; Variables

(defvar-local buffer-background--face-cookie nil
  "Cookie for face remapping in the current buffer.")

(defvar-local buffer-background--current-spec nil
  "Current background specification for this buffer.")

(defvar-local buffer-background--user-disabled nil
  "Non-nil if user has explicitly disabled background for this buffer.")

;;; Utility Functions

(defun buffer-background--mix-colors (fg-color bg-color alpha)
  "Mix FG-COLOR with BG-COLOR using ALPHA opacity.
ALPHA should be between 0.0 and 1.0."
  (let ((fg-rgb (color-name-to-rgb fg-color))
        (bg-rgb (color-name-to-rgb bg-color)))
    (if (and fg-rgb bg-rgb)
        (apply #'format "#%02x%02x%02x"
               (cl-mapcar (lambda (fg bg)
                           (round (* 255 (+ (* alpha fg) (* (- 1 alpha) bg)))))
                         fg-rgb bg-rgb))
      fg-color)))

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

   ((symbolp criteria)
    (eq criteria (buffer-local-value 'major-mode buffer)))

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
   ((functionp criteria)
    (with-current-buffer buffer
      (funcall criteria buffer)))

   (t nil)))

(defun buffer-background--find-spec-for-buffer (&optional buffer)
  "Find the appropriate background specification for BUFFER.
Returns a normalized plist specification or nil.
BUFFER defaults to current buffer."
  (let ((buffer (or buffer (current-buffer)))
        (spec nil))
    (when buffer-background-color-alist
      (cl-loop for (criteria . value) in buffer-background-color-alist
               when (buffer-background--match-criteria-p criteria buffer)
               do (setq spec (buffer-background--normalize-spec value))
               and return nil))

    ;; Fallback to global default if no match
    (unless spec
      (when buffer-background-color
        (setq spec (buffer-background--normalize-spec buffer-background-color))))

    ;; Apply global defaults to spec
    (when spec
      (buffer-background--apply-defaults spec))))

(defun buffer-background--normalize-spec (spec)
  "Normalize SPEC into a plist format.
SPEC can be a string (color), or a plist."
  (cond
   ;; plist
   ((and (listp spec) (keywordp (car spec)))
    spec)
   ;; string colors
   ((stringp spec)
    (if (or (string-match-p "^#[0-9a-fA-F]\\{6\\}$" spec)  ; Hex color
            (color-defined-p spec))                          ; Named color
        (list :color spec)
      nil))
   (t nil)))

(defun buffer-background--apply-defaults (spec)
  "Apply global default values to SPEC where not specified."
  (let ((result (copy-sequence spec)))
    ;; Only apply defaults if not already specified in spec
    (unless (plist-member result :opacity)
      (setq result (plist-put result :opacity buffer-background-opacity)))
    result))

;;; Actually applying the background

(defun buffer-background--apply-color-background (color opacity)
  "Apply COLOR background with OPACITY to the current buffer using face remapping."
  (buffer-background--remove-background)

  (let* ((default-bg (or (face-background 'default) "#ffffff"))
         (final-color (if (and opacity (< opacity 1.0))
                         (buffer-background--mix-colors color default-bg opacity)
                       color)))
    ;; Use face remapping to change the default background
    (setq buffer-background--face-cookie
          (face-remap-add-relative 'default :background final-color))))

(defun buffer-background--process-spec (spec)
  "Process background SPEC and apply it to the current buffer.
SPEC is a plist with :color and other properties."
  (when (plist-get spec :color)
    (let ((color (plist-get spec :color))
          (opacity (plist-get spec :opacity)))
      (buffer-background--apply-color-background color opacity))))

(defun buffer-background--remove-background ()
  "Remove background from current buffer."
  (when buffer-background--face-cookie
    (face-remap-remove-relative buffer-background--face-cookie)
    (setq buffer-background--face-cookie nil)))

;;; Minor Mode Definition

(define-minor-mode buffer-background-mode
  "Toggle buffer background color display.
When enabled, displays a color as the background of the current buffer."
  :lighter " BG"
  :group 'buffer-background
  (if buffer-background-mode
      (buffer-background--enable)
    (buffer-background--disable)))

(defun buffer-background--enable ()
  "Enable buffer background in current buffer."
  ;; Check if already enabled to prevent looping
  (unless buffer-background--face-cookie
    (run-hooks 'buffer-background-before-enable-hook)
    (when-let ((spec (buffer-background--find-spec-for-buffer)))
      (buffer-background--process-spec spec)
      ;; Store the spec for later
      (setq-local buffer-background--current-spec spec)
      (message "Background enabled!"))
    (run-hooks 'buffer-background-after-enable-hook)))

(defun buffer-background--disable ()
  "Disable buffer background in current buffer."
  (buffer-background--remove-background))

;;; Auto assignment

(defvar buffer-background--auto-assignment-timer nil
  "Timer for checking buffer auto-assignment.")

(define-minor-mode buffer-background-global-mode
  "Global mode for automatic buffer background assignment."
  :global t
  :require 'buffer-background
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
             (not buffer-background--face-cookie)  ; Double-check no background exists
             (not buffer-background--user-disabled)  ; Respect user's explicit disable
             (buffer-background--should-auto-enable-p (buffer-name)))
    (buffer-background-mode 1)))

;;; Autoload user commands

;;;###autoload
(defun buffer-background-set-color (color)
  "Set background COLOR for the current buffer."
  (interactive "sBackground color (hex or name): ")
  (setq-local buffer-background-color color)
  (setq-local buffer-background--user-disabled nil)  ; Clear disable flag
  (when buffer-background-mode
    (buffer-background--enable))
  (unless buffer-background-mode
    (buffer-background-mode 1))
  (message "Background color set: %s" color))

;;;###autoload
(defun buffer-background-toggle ()
  "Toggle buffer background mode in current buffer."
  (interactive)
  (if buffer-background-mode
      ;; Disabling: set user-disabled flag to prevent auto-enable
      (progn
        (setq-local buffer-background--user-disabled t)
        (buffer-background-mode -1))
    ;; Enabling: clear user-disabled flag
    (progn
      (setq-local buffer-background--user-disabled nil)
      (buffer-background-mode 1)))
  (message "Buffer background %s" (if buffer-background-mode "enabled" "disabled")))

;;;###autoload
(defun buffer-background-clear ()
  "Clear background color from current buffer."
  (interactive)
  (setq-local buffer-background-color nil)
  (setq-local buffer-background--user-disabled t)
  (when buffer-background-mode
    (buffer-background-mode -1))
  (message "Background color cleared"))

;;;###autoload
(defun buffer-background-set-opacity (opacity)
  "Set background color OPACITY for current buffer."
  (interactive "nOpacity (0.0-1.0): ")
  (setq opacity (max 0.0 (min 1.0 opacity)))
  (setq-local buffer-background-opacity opacity)
  (when buffer-background-mode
    (buffer-background--enable))
  (message "Background opacity set to %.2f" opacity))

;;;###autoload
(defun buffer-background-show-color-source ()
  "Show which background color would be used for the current buffer."
  (interactive)
  (let ((spec (buffer-background--find-spec-for-buffer)))
    (if spec
        (message "Background for %s: color %s (opacity %.2f)"
                 (buffer-name)
                 (plist-get spec :color)
                 (plist-get spec :opacity))
      (message "No background configured for %s" (buffer-name)))))

;;; Convenience Functions

(defun buffer-background-enable-for-scratch ()
  "Enable buffer background for *scratch* buffer."
  (interactive)
  (when-let ((scratch-buffer (get-buffer "*scratch*")))
    (with-current-buffer scratch-buffer
      (call-interactively #'buffer-background-set-color))))

(defun buffer-background-enable-for-messages ()
  "Enable buffer background for *Messages* buffer."
  (interactive)
  (when-let ((messages-buffer (get-buffer "*Messages*")))
    (with-current-buffer messages-buffer
      (call-interactively #'buffer-background-set-color))))

;;; Hooks and Customization

(defcustom buffer-background-mode-hook nil
  "Hook run when \"buffer-background-mode\" is enabled or disabled."
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

;;; Footer

(provide 'buffer-background)

;;; buffer-background.el ends here
