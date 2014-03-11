(defadvice load-theme  
  (before theme-dont-propagate activate) 
  (mapc #'disable-theme custom-enabled-themes))

(setq calendar-location-name "Aarhus, Denmark")
(setq calendar-latitude 56.199614)
(setq calendar-longitude 10.172997)

(load-theme 'spacegray t)

(set-frame-font "Inconsolata")
(set-face-attribute 'default nil :height 110) ; Font size
(global-font-lock-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(require 'perspective)
(persp-mode)

(provide 'rune-ui)
