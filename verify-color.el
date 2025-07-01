;;; verify-color.el --- Verify color background functionality

;; Load the package
(load-file "buffer-background.el")

;; Test in a temporary buffer
(with-temp-buffer
  ;; Set the buffer name for easier identification
  (rename-buffer "*test-color*")
  
  ;; Test setting a color
  (buffer-background-set-color "#ff0000")
  
  ;; Check if the face remapping was applied
  (if buffer-background--face-cookie
      (message "SUCCESS: Face remapping cookie found: %s" buffer-background--face-cookie)
    (message "FAILURE: No face remapping cookie found"))
  
  ;; Test the spec finding
  (let ((spec (buffer-background--find-spec-for-buffer)))
    (if spec
        (message "SUCCESS: Found spec: %s" spec)
      (message "FAILURE: No spec found")))
  
  ;; Test opacity mixing
  (let ((mixed (buffer-background--mix-colors "#ff0000" "#ffffff" 0.5)))
    (message "Color mixing test: #ff0000 + #ffffff @ 50%% = %s" mixed))
  
  ;; Clean up
  (buffer-background-mode -1))

;; Test auto-assignment
(setq buffer-background-color-alist
      '(("*scratch*" . "#2d2d2d")
        ("*Messages*" . "#1a1a1a")))

(message "Test configuration loaded")
(message "Manual tests to try:")
(message "1. M-x buffer-background-set-color RET #ff0000")
(message "2. Switch to *scratch* buffer and check if it gets #2d2d2d")
(message "3. M-x buffer-background-global-mode to enable auto-assignment")

;;; verify-color.el ends here