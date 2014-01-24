(add-to-list 'load-path (concat user-emacs-directory "/config"))
(setq inhibit-splash-screen t)
;(setq initial-buffer-choice "~/Documents/Org/Everything.org") ; Starts my TODO.org file instead of standard screen

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'f)
(require 's)
(require 'dash)
(require 'dash-functional)

(require 'better-defaults)

(require 'rune-evil)
(require 'rune-ac)
(require 'rune-flycheck)
(require 'rune-ui)
(require 'rune-ido)
(require 'rune-modeline)
(require 'rune-editor)
(require 'rune-orgmode)
(require 'rune-misc)
(require 'rune-lisp)
(require 'rune-keybinding)

(if (eq system-type 'darwin)
    (require 'rune-osx))
