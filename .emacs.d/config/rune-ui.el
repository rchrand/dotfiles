(defadvice load-theme  
  (before theme-dont-propagate activate) 
  (mapc #'disable-theme custom-enabled-themes))

;; (setq calendar-location-name "Aarhus, Denmark")
;; (setq calendar-latitude 56.199614)
;; (setq calendar-longitude 10.172997)

(load-theme 'ample t)

(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :height 105) ; Font size
(global-font-lock-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(scroll-bar-mode 0)
(tool-bar-mode 0)

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode RET to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
(hidden-mode-line-mode 1)

(require 'perspective)
(persp-mode)

(provide 'rune-ui)
