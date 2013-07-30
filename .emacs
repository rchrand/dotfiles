;; Evil 
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Things that enhnace the Evil ESC, so that it actual quits when pressing ESC
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; Enhances evil
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Packages manager for Emacs
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;; Keybindings
(global-set-key [(f5)] 'ack)
(global-set-key (kbd "C-x ;") 'kill-some-buffers)
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-O") 'rotate-windows)
;; End of Window Manipulation

;; Setting some things up for ido-mode
(require 'ido)
(setq ido-enable-flex-matching t) ;; enables fuzzy matching
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)
(ido-mode t)

;;;Plugins
(add-hook 'after-init-hook #'global-flycheck-mode) ;Flycheck
(add-to-list 'load-path "~/.emacs.d/elpa/powerline-20130623.1829") ; Powerline?
(require 'powerline)
(add-to-list 'load-path "~/.emacs.d/elpa/sr-speedbar-20130309.1959") ; Speedbar
(require 'sr-speedbar)
(global-set-key [(f2)] 'sr-speedbar-toggle)
(global-set-key [(f3)] 'sr-speedbar-refresh-toggle)


;;; Misc
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode +1)
(global-linum-mode t)

(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up

(set-frame-font "Inconsolata")
(set-face-attribute 'default nil :height 110)

(fset 'yes-or-no-p 'y-or-n-p) ;; Makes the default yes and no to just y and n
(setq inhibit-startup-echo-area-message t) ; Removes the Startup-screen
(setq inhibit-startup-message t) ; Removes the Startup-screen
(setq initial-buffer-choice "~/Documents/Org/TODO.org") ; Starts my TODO.org file instead of standard screen

;; System Name
(setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))

;; The only way i could load my theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "b23dd4e47555ae7514a70ca758dce093bd032c602061be09a350be83c227fd2f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
