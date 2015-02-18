;;; package -- Summary
;;; Commentary:
;;; Code:
(scroll-bar-mode -1)

;; Font settings
(set-face-attribute 'default nil :height 140)
(set-frame-font "Meslo LG L")

;; Color themes
;; (prelude-require-package 'noctilux-theme)
;; (load-theme 'noctilux t)
(disable-theme 'zenburn)

;; Mode line
(prelude-require-package 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)

;; Word wrap
;; (global-visual-line-mode t)
;; (setq-default word-wrap t)

(setq whitespace-line-column 9999999)
;;; ui.el ends here
