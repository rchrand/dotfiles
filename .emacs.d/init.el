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
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

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
  (set-frame-font "JetBrains Mono 14"))
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

;; Theme

(setq custom-safe-themes t)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-material-dark t)
  (doom-themes-org-config))

;; Theme changer

(use-package switch-window
  :straight t
  :bind (("C-x o" . switch-window)
         ("C-x B" . balance-windows)))

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

(use-package zig-mode
  :straight t
  :mode (("\\.zig\\'" . zig-mode)))
  
; (use-package volatile-highlights
;   :straight t
;   :diminish
;   :config
;   (volatile-highlights-mode +1))

; (use-package diff-hl
;   :config
;   (global-diff-hl-mode +1)
;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(put 'dired-find-alternate-file 'disabled nil)
