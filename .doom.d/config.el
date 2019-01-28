;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 13))

(setq flycheck-ruby-reek-executable "/Users/rchrand/.asdf/shims/reek")
(setq flycheck-ruby-rubocop-executable "/Users/rchrand/.asdf/shims/rubocop")

(map!
  :gnvime "M-s" #'save-buffer

 ;; Easier window navigation
 :n "C-h"   #'evil-window-left
 :n "C-j"   #'evil-window-down
 :n "C-k"   #'evil-window-up
 :n "C-l"   #'evil-window-right)
