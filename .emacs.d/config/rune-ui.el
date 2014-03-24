;; Themes
(load-theme 'spacegray t)

;; Fonts
(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :height 125) ; Font size
(global-font-lock-mode 1)

;; UI misc thing
(defalias 'yes-or-no-p 'y-or-n-p)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq inhibit-startup-echo-area-message t)

;; Disable the clutter
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(require 'perspective)
(persp-mode)

(provide 'rune-ui)
