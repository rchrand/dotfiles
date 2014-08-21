;;; package -- Summary
;;; Commentary:
;;; Code:
(scroll-bar-mode -1)

;; Window control
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-0") 'delete-window)

;; Font settings
(set-frame-font "Anonymous Pro For Powerline")
(set-face-attribute 'default nil :height 160)

;; Word wrap
(global-visual-line-mode t)
(setq-default word-wrap t)

(setq whitespace-line-column 9999999)
;;; ui.el ends here
