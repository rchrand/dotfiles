;;; popups.el --- Small, deliberate popup-buffer setup -*- lexical-binding: t; -*-

(require 'seq)

(defun rchrand/popper-fit-window-height (window)
  "Fit popup WINDOW between 12 lines and 40 percent of the frame."
  (fit-window-to-buffer
   window
   (max 12 (floor (* 0.4 (frame-height))))
   12))

(use-package popper
  :demand t
  :bind (("C-`" . popper-toggle))
  :init
  (setq popper-reference-buffers '("^\\*Work Dashboard\\*$")
        popper-window-height #'rchrand/popper-fit-window-height)
  :config
  (popper-mode 1))

(defun rchrand/org-dashboard--popup-window (&optional buffer)
  "Return BUFFER's dashboard side window, if it has one."
  (seq-find (lambda (window) (window-parameter window 'window-side))
            (get-buffer-window-list
             (or buffer (current-buffer)) nil 'visible)))

(defun rchrand/org-dashboard--dismiss-popup ()
  "Dismiss the dashboard when it is displayed as a side-window popup."
  (when-let* ((window (rchrand/org-dashboard--popup-window)))
    (quit-window nil window)))

(defun rchrand/org-dashboard--call-in-main-window (function &rest arguments)
  "Dismiss the dashboard popup, then call FUNCTION with ARGUMENTS."
  (rchrand/org-dashboard--dismiss-popup)
  (apply function arguments))

(defun rchrand/org-dashboard-open-today ()
  "Dismiss the dashboard popup and open today's note."
  (interactive)
  (rchrand/org-dashboard--call-in-main-window #'rchrand/org-daily-open))

(defun rchrand/org-dashboard-open-date ()
  "Dismiss the dashboard popup and prompt for another daily note."
  (interactive)
  (rchrand/org-dashboard--dismiss-popup)
  (call-interactively #'rchrand/org-daily-open-date))

(defun rchrand/org-dashboard-capture-focus ()
  "Dismiss the dashboard popup and capture a focus item."
  (interactive)
  (rchrand/org-dashboard--call-in-main-window #'rchrand/org-capture-focus))

(defun rchrand/org-dashboard-capture-note ()
  "Dismiss the dashboard popup and capture a note."
  (interactive)
  (rchrand/org-dashboard--call-in-main-window #'rchrand/org-capture-note))

(defun rchrand/org-dashboard-open-briefing ()
  "Dismiss the dashboard popup and open the AI briefing."
  (interactive)
  (rchrand/org-dashboard--call-in-main-window #'rchrand/org-open-briefing))

(defun rchrand/org-dashboard-open-knowledge-candidates ()
  "Dismiss the dashboard popup and open knowledge candidates."
  (interactive)
  (rchrand/org-dashboard--call-in-main-window
   #'rchrand/org-open-knowledge-candidates))

(provide 'popups)
;;; popups.el ends here
