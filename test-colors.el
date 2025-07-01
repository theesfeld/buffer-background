;;; test-colors.el --- Test color functionality

;; Simple test configuration
(setq buffer-background-color-alist
      '(("*scratch*" . "#2d2d2d")
        ("*Messages*" . "#1a1a1a")
        (emacs-lisp-mode . (:color "#1e1e2e" :opacity 0.9))))

;; Test functions
(defun test-color-basics ()
  "Test basic color functionality."
  (interactive)
  (with-current-buffer "*scratch*"
    (buffer-background-set-color "#ff0000")  ; Red background
    (message "Set red background for *scratch*"))
  
  (with-current-buffer "*Messages*"
    (buffer-background-set-color "#0000ff")  ; Blue background
    (message "Set blue background for *Messages*")))

(defun test-auto-assignment ()
  "Test automatic assignment."
  (interactive)
  (buffer-background-global-mode 1)
  (message "Enabled global mode - should auto-assign colors"))

(defun test-opacity ()
  "Test opacity setting."
  (interactive)
  (buffer-background-set-color "#ff0000")  ; Red
  (buffer-background-set-opacity 0.5)     ; 50% opacity
  (message "Set red background with 50%% opacity"))

(message "Test functions loaded. Try:")
(message "M-x test-color-basics")
(message "M-x test-auto-assignment") 
(message "M-x test-opacity")

;;; test-colors.el ends here