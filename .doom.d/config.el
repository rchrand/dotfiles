;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rune Andersen"
      user-mail-address "rune.andersen92@gmail.com")

(after! rustic
  (setq rustic-lsp-server 'rls))

(setq doom-font (font-spec :family "Fira Code" :size 13))

(map!
 ;; Easier window navigation
 :n "C-h"   #'evil-window-left
 :n "C-j"   #'evil-window-down
 :n "C-k"   #'evil-window-up
 :n "C-l"   #'evil-window-right)

(setq doom-theme 'doom-material)

(custom-set-faces `(region ((t (:background "#262626")))))

(setq display-line-numbers-type t)
