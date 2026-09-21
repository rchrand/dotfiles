;;; init.el -- summary: Personal Emacs config  -*- lexical-binding: t; -*-
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

;; Keep customizations and history files inside savefile/
(setq custom-file (expand-file-name "custom.el" rchrand-savefile-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set the mac commands
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Prefer concise confirmation prompts.
(setq use-short-answers t)

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; enable transient mark mode (visual selection)
(transient-mark-mode t)

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
;; Default to installing packages via straight unless overridden locally
(setq straight-use-package-by-default t)
;; Ensure Org is treated as built-in by straight (do not fetch it)
(straight-use-package '(org :type built-in))

;; Ensure :diminish works across packages that use it
(use-package diminish)

;; Make custom lisp modules discoverable
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;
;;; Active modules
;;;;;;;;;;;;;;;;;;;;

(load "ui" nil 'nomessage)
(load "environment" nil 'nomessage)
(load "editing" nil 'nomessage)
(load "completion" nil 'nomessage)
(load "git-config" nil 'nomessage)
(load "project-config" nil 'nomessage)
(load "dashboard-config" nil 'nomessage)
(load "misc" nil 'nomessage)
(load "c-config" nil 'nomessage)
(load "lsp-config" nil 'nomessage)
(load "structural-editing" nil 'nomessage)
(load "common-lisp-config" nil 'nomessage)
(load "python-config" nil 'nomessage)
(load "landfolk-config" nil 'nomessage)
(load "prose" nil 'nomessage)
(load "theme" nil 'nomessage)

;; Disabled, but intentionally kept for reference and possible reuse:
;; (load "popups" nil 'nomessage)
;; (load "org-work" nil 'nomessage)
;; (load "org-config" nil 'nomessage)
;; (load "journal" nil 'nomessage)
;; (load "rust-config" nil 'nomessage)
;; (load "zig-config" nil 'nomessage)

(provide 'init)
;;; init.el ends here
