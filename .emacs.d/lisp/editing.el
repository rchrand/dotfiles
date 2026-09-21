;;; editing.el --- Editing helpers and general UX -*- lexical-binding: t; -*-

;; Builtin packages
(use-package elec-pair
  :straight nil
  :config
  (electric-pair-mode +1))

;; Keep deletion recoverable, and let the mark ring act as navigation history
;; without `C-x C-x' unexpectedly highlighting an inactive region.
(setq delete-by-moving-to-trash t
      exchange-point-and-mark-highlight-region nil)

;; Make ordinary word movement and editing understand camelCase in code.
(add-hook 'prog-mode-hook #'subword-mode)

(use-package re-builder
  :straight nil
  :init
  (setq reb-re-syntax 'string))

(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1))

;; Perspective tracks buffers by name.  Leave `uniquify' disabled so names stay
;; stable when workspaces are saved or switched.
(setq uniquify-buffer-name-style nil)

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-create-destination-dirs 'ask
        dired-vc-rename-file t
        dired-recursive-deletes 'top
        dired-recursive-copies 'always
        dired-dwim-target t)
  (require 'dired-x))

(use-package so-long
  :straight nil
  :config
  (global-so-long-mode 1))

(use-package winner
  :straight nil
  :config
  (winner-mode 1))

(use-package repeat
  :straight nil
  :config
  (repeat-mode 1))

(use-package whitespace
  :straight nil
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode))
  :init
  ;; Only clean trailing whitespace; avoid global cleanup that can break Makefiles/formatters
  (defun rchrand/cleanup-trailing-whitespace ()
    (delete-trailing-whitespace))
  (add-hook 'before-save-hook #'rchrand/cleanup-trailing-whitespace)
  :config
  (setq whitespace-style '(face trailing)))

(use-package perspective
  :bind ("C-x B" . persp-list-buffers)
  :custom (persp-mode-prefix-key (kbd "s-p"))
  :init (persp-mode))

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold))))

(use-package all-the-icons)

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 0.5
        gcmh-high-cons-threshold (* 64 1024 1024)))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :diminish
  :config
  (global-anzu-mode))

;; Fix org-element warnings in non-org buffers first
(with-eval-after-load 'org-element
  (defun org-element-at-point-safe (&optional epom cached-only)
    "Safe wrapper for org-element-at-point that checks for org-mode first."
    (if (derived-mode-p 'org-mode)
        (org-element-at-point epom cached-only)
      nil))
  (advice-add 'org-element-at-point :around
              (lambda (orig-fun &optional epom cached-only)
                (if (derived-mode-p 'org-mode)
                    (funcall orig-fun epom cached-only)
                  nil))))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package super-save
  :diminish
  :config
  (setq super-save-triggers (delete 'ace-window super-save-triggers))
  (add-to-list 'super-save-triggers 'switch-window)
  (super-save-mode +1))

(use-package embrace
  :bind (("C-c C-," . embrace-commander)))

(use-package undo-fu
  :demand t
  :bind (([remap undo] . undo-fu-only-undo)
         ([remap redo] . undo-fu-only-redo)
         ("C-_" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)
         ("C-M-_" . undo-fu-only-redo-all))
  :init
  (setq undo-limit 256000
        undo-strong-limit 2000000
        undo-outer-limit 36000000))

(use-package undo-fu-session
  :after undo-fu
  :init
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session/" rchrand-savefile-dir)
        undo-fu-session-incompatible-files
        '("\\.gpg\\'" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst))
  (undo-fu-session-global-mode 1))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char)
         ("C-c ." . avy-goto-word-or-subword-1))
  :config
  (setq avy-background t))

(use-package rg
  :config
  (setq rg-executable (or (executable-find "rg") "/opt/homebrew/bin/rg")
        rg-group-result t
        rg-hide-command t
        rg-show-columns nil
        rg-show-header t
        rg-custom-type-aliases nil
        rg-default-alias-fallback "all")
  (rg-enable-default-bindings))

(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-o" . crux-smart-open-line-above)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package pulsar
  :init
  (setq pulsar-delay 0.05
        pulsar-iterations 5)
  :config
  (pulsar-global-mode 1))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (goggles-delete 'disable)
  (goggles-define rchrand/goggles-undo
                  undo-fu-only-undo undo-fu-only-redo)
  (goggles-define rchrand/goggles-register-paste insert-register)
  (goggles-define rchrand/goggles-kill-word
                  backward-kill-word kill-word))

(provide 'editing)
;;; editing.el ends here
