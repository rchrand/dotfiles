;; This piece of code is taken from Prelude

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (whitespace-cleanup))

(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(defun toggle-line-number ()
  (interactive)
  (require 'linum-relative)
  (if (equal t global-linum-mode)
      (global-linum-mode 0)
    (global-linum-mode 1)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c k") 'toggle-line-number)

(provide 'rune-functions)
