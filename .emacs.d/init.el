;;; init.el -- summary: Personal Emacs config
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;
;;; Initial setup
;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Rune Hessner"
      user-mail-address "rune@landfolk.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024))

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; ... but ask first
(setq confirm-kill-emacs nil)

(defconst rchrand-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p rchrand-savefile-dir)
  (make-directory rchrand-savefile-dir))

;; Set the mac commands
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline nil)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; auto-create missing folders
(defun er-auto-create-missing-dirs ()
  "Make missing parent directories automatically."
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))


(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;

;; Install Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(straight-use-package 'org)

;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable the scroll bar
(scroll-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1))

;; let's pick a nice font
(cond
 ((find-font (font-spec :name "JetBrains Mono"))
  (set-frame-font "JetBrains Mono 16"))
 ((find-font (font-spec :name "Fira Code"))
  (set-frame-font "Fira Code-14"))
 ((find-font (font-spec :name "Menlo"))
  (set-frame-font "Menlo-14"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-14"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-14")))

;; Mode line settings
(display-line-numbers-mode)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Keybindings
;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-S-<left>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<right>") #'shrink-window-horizontally)
(global-set-key (kbd "C-S-<up>") #'enlarge-window)
(global-set-key (kbd "C-S-<down>") #'shrink-window)

;; Unset suspend-frame
(global-unset-key (kbd "C-z"))

(defun rchrand/goto-init-file ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") #'rchrand/goto-init-file)

(defun rchrand/goto-todo-file ()
  "Open the todo file."
  (interactive)
  (find-file "~/org/todo.org"))

(global-set-key (kbd "C-c T") #'rchrand/goto-todo-file)

(defun rchrand/reload-init-file ()
  "Reload the init file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c R") #'rchrand/reload-init-file)


;; Builtin packages

(use-package elec-pair
  ;; "Automatic closes parens when opening them"
  :straight nil
  :config
  (electric-pair-mode +1))

(use-package hl-line
  ;; "Hightlight current line"
  :straight nil
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  ;; "Makes buffers with the same name unique (e.g) same file name"
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package dired
  :straight nil
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package whitespace
  :straight nil
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook)))
    ;; (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Project.el

(use-package project
  :straight nil
  :bind (("C-c p f" . project-find-file)
	 ("C-c p d" . project-dired)
	 ("C-c p b" . project-switch-to-buffer)
	 ("C-c p s" . rg))
  )

;; Theme
(setq custom-safe-themes t)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-material-dark t)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :straight t
  :diminish rainbow-delimiters-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package switch-window
  :straight t
  :bind (("C-x o" . switch-window)
         ("C-x B" . balance-windows)))

(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  :init
  (persp-mode))

(use-package hl-todo
  :straight t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold))))

(use-package  all-the-icons
  :straight t)


(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode +1))


;; Editor

(use-package anzu
  :straight t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :diminish
  :config
  (global-anzu-mode))

(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package zop-to-char
  :straight t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package super-save
  :straight t
  :diminish
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package embrace
  :straight t
  :bind (("C-c C-," . embrace-commander)))

(use-package undo-tree
  :straight t
  :diminish
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

(use-package avy
  :straight t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char)
         ("C-c ." . avy-goto-word-or-subword-1))
  :config
  (setq avy-background t))

(use-package rg
  :straight t
  :config
  (setq rg-executable "/opt/homebrew/bin/rg"))

(use-package ivy
  :straight t
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")

  (ivy-initial-inputs-alist '((org-refile . "^")
                              (org-agenda-refile . "^")
                              (org-capture-refile . "^")
                              (counsel-M-x . "")
                              (counsel-describe-function . "")
                              (counsel-describe-variable . "")
                              (man . "^")
                              (woman . "^")))
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-thing-at-point)
         ("C-r" . swiper)))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("M-y" . counsel-yank-pop)
         ("C-c /" . counsel-rg)
         ("C-x j" . counsel-file-jump)
         ("C-x C-j" . counsel-dired-jump)
         ("C-x C-," . counsel-mark-ring)
         ("C-c i" . counsel-menu)))

(use-package crux
  :straight t
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

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :straight t
  :defer t
  :bind (("s-g" . git-timemachine)))

(use-package volatile-highlights
  :straight t
  :diminish
  :config
  (volatile-highlights-mode +1))

;; Org-mode
(use-package org-mode
  :straight t
  :defer nil
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c c" . org-capture))
  :config
  (setq org-agenda-files '("~/org"))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (setq org-fast-tag-selection-single-key t)
  (setq org-reverse-note-order t)
  (setq org-use-fast-todo-selection t)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?Z)
  (setq org-priority-default ?C)
  (setq org-agenda-deadline-leaders
          '("" "" "%2d d. ago: ")
        org-deadline-warning-days 0
        org-agenda-span 7
        org-agenda-start-day "-0d"
        org-agenda-skip-function-global
          '(org-agenda-skip-entry-if 'todo 'done))
  (setq org-todo-keywords
      '( (sequence "STARTED(s)" "WAITING(w)" "TODO(t)" "|" "DONE(d)")
         (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)") ))

  (setq org-todo-keyword-faces
	'(("STARTED" . (:foreground "systemYellowColor" :weight bold))
          ("DONE" . (:foreground "ForestGreen" :weight bold))
          ("WAITING" . (:foreground "MediumPurple2" :weight bold))
          ("CANCELED" . (:foreground "systemRedColor" :weight bold))
          ("DELEGATED" . (:foreground "DeepSkyBlue" :weight bold))
          ("SOMEDAY" . (:foreground "PaleVioletRed2" :weight bold))
          ))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/todo.org")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
          ("i" "inbox" entry
           (file "~/org/inbox.org")
           "* %?")
          )
        )

  (setq org-todo-sort-order '("STARTED" "WAITING" "TODO" "DELEGATED" "CANCELED" "DONE"))
  )

(defun my/org-custom-todo-sort-key ()
  (let* ((my-order '("STARTED" "WAITING" "TODO" "DELEGATED" "CANCELED" "DONE"))
         (todo (org-get-todo-state)))
    (or (cl-position todo my-order :test #'equal)
        (length my-order)))) ; put unknown keywords at the end


(use-package org-bullets
  :straight t
  :config
  (org-bullets-mode 1))

(use-package org-sticky-header
  :straight t
  :config
  (org-sticky-header-mode 1))

;; Languages
(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package zig-mode
  :straight t
  :mode (("\\.zig\\'" . zig-mode)))
