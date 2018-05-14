(require 'package) 
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))


(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (require 'diminish))
(require 'bind-key)

;; Default settings
(show-paren-mode nil)
(setq visible-bell nil)
(delete-selection-mode 1)

(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(defalias 'yes-or-no-p 'y-or-n-p)

(set-default 'indent-tabs-mode nil)
(setq-default tab-width 2)
(setq sentence-end-double-space nil)

;; Remove that UI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; OSX
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (use-package exec-path-from-shell
    :ensure t
    :init (exec-path-from-shell-initialize)))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Keybindings

(bind-key* "<C-return>" 'other-window)
(bind-key "s-0" 'delete-window)
(bind-key "s-1" 'delete-other-windows)
(bind-key "s-2" 'split-window-below)
(bind-key "s-3" 'split-window-right)
(bind-key* "M-s" 'save-buffer)

(bind-key "C-c I" (lambda ()
                    (interactive)
                    (find-file "~/.emacs.d/init.el")))
(bind-key "C-c T" (lambda ()
                    (interactive)
                    (find-file "~/Dropbox/org/todo.org")))
(bind-key* "C-c TAB" (lambda ()
                       (interactive)
                       (switch-to-buffer (other-buffer (current-buffer) 1))))

(bind-key "s-b" 'ivy-switch-buffer)
(bind-key "s-r" 'counsel-projectile-rg)
(bind-key "C-x C-j" 'dired-jump)
(global-set-key "\M-`" 'other-frame)

;; Packages
(use-package ivy
  :ensure t
  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))

    (use-package smex
      :ensure t)

    (use-package ivy-hydra
      :ensure t)

    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full) ;Show the full virtual file paths

    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ;Default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Do not show "./" and "../" in the `counsel-find-file' completion list
    (setq ivy-extra-directories nil)    ;Default value: ("../" "./")

    (defun modi/ivy-set-prompt-text-properties (prompt std-props)
      "Add a different face for the \"<..>\" string in `counsel-set-variable'."
      (ivy--set-match-props prompt "<\\(.*\\)>"
                            `(face font-lock-string-face ,@std-props) 1)
      (ivy-set-prompt-text-properties-default prompt std-props))
    (setq ivy-set-prompt-text-properties-function 'modi/ivy-set-prompt-text-properties)

    (bind-keys
     :map ivy-minibuffer-map
     ;; Exchange the default bindings for C-j and C-m
     ("C-m" . ivy-alt-done)             ;RET, default C-j
     ("C-j" . ivy-done)                 ;Default C-m
     ("C-S-m" . ivy-immediate-done))

    (bind-keys
     :map ivy-occur-mode-map
     ("j" . ivy-occur-next-line)
     ("k" . ivy-occur-previous-line)
     ("b" . backward-char)
     ("f" . forward-char)
     ("v" . ivy-occur-press)            ;Default f
     ("RET" . ivy-occur-press))

    (with-eval-after-load 'setup-windows-buffers
      (bind-keys
       :map ivy-minibuffer-map
       ("C-x k" . modi/kill-buffer-dwim) ;Aborts recursive edit
       ("C-)" . modi/kill-buffer-dwim))) ;Aborts recursive edit

    ;; Bind C-k to kill a buffer directly from the list shown on doing M-x ivy-switch-buffer.
    ;; https://github.com/abo-abo/swiper/issues/164
    (defun modi/ivy-kill-buffer ()
      (interactive)
      (ivy-set-action 'kill-buffer)
      (ivy-done))
    (bind-keys
     :map ivy-switch-buffer-map
     ("C-k" . modi/ivy-kill-buffer))))

(defun swiper-under-point ()
  "Use swiper for searching at symbol under cursor."
  (interactive)
  (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("M-*" . swiper-under-point)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c a" . counsel-ag))
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;(use-package mix-format
;; :ensure t)

(use-package elixir-mode
  :ensure t
  :mode "\\.ex(s)\\'"
  ;; :hook (lambda () (add-hook 'before-save-hook 'mix-format-before-save))
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :ensure t
  :config
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
                            (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line)))

(use-package zop-to-char
  :ensure t
  :bind ("M-z" . zop-to-char))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook (prog-mode text-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package company
  :ensure t
  :defer t

  :init (global-company-mode)
  :config
  (progn
    (setq
     company-idle-delay 0.2
     company-selection-wrap-around t
     company-minimum-prefix-length 2
     company-require-match nil
     company-dabbrev-ignore-case nil
     company-dabbrev-downcase nil
     company-show-numbers t
     company-tooltip-align-annotations t)
    )
  :diminish company-mode)

(use-package aggressive-indent
  :disabled
  :ensure t
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode))

(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (require 'smartparens-ruby)
  (require 'smartparens-elixir)
  (setq-default sp-autoinsert-pair t)
  (smartparens-global-strict-mode t))

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :commands web-mode
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-enable-auto-pairing nil)
  (setq-default web-mode-enable-auto-indentation nil)
  (setq-default web-mode-auto-quote-style 2)
  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "jsx" (file-name-extension buffer-file-name))
                   (string-equal "js" (file-name-extension buffer-file-name)))
                (flycheck-select-checker 'javascript-eslint))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   (string-equal "ts" (file-name-extension buffer-file-name)))
                (setup-tide-mode))))
  )

(use-package emacs-lisp-mode
  :bind (("C-c b" . eval-buffer))
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :ensure t
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :ensure t
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :config
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package projectile
  :ensure    projectile
  :config    (projectile-global-mode t)
  :bind ("C-c C-f" . projectile-find-file)
  :init      (progn
               (setq projectile-completion-system 'ivy)

               (use-package counsel-projectile
                 :ensure t
                 :init (counsel-projectile-mode))))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (progn
;;     (load-theme 'zenburn t)))

(use-package leuven-theme
  :ensure t
  :config
  (progn
    (load-theme 'leuven t)))

(use-package rg
  :ensure t)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

(add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ; Load Dired X when Dired is loaded.
(setq dired-omit-mode-mode t)

(use-package bundler :ensure t :defer t)
(use-package projectile-rails :ensure t :defer t
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package ruby-mode
  :ensure t
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
         (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package minitest
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'minitest-mode))

(use-package visual-fill-column
  :ensure t)

(use-package org
  :init
  (progn
    ;; Fontify org-mode code blocks
    (setq org-src-fontify-natively t)

    ;; Essential Settings
    (setq org-log-done 'time)
    (setq org-html-doctype "html5")
    (setq org-export-headline-levels 6)
    (setq org-export-with-smart-quotes t)

    ;; Custom TODO keywords
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NOW(n@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    ;; Set up latex
    (setq org-export-with-LaTeX-fragments t)
    (setq org-latex-create-formula-image-program 'imagemagick)

    ;; local variable for keeping track of pdf-process options
    (setq pdf-processp nil))
  :config
  ;; Fancy bullet rendering.
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  ;; Insert links from clipboard.
  (use-package org-cliplink
    :ensure t
    :config
    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (magit-auto-revert-mode))

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo)))

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nil nil t)
 '(package-selected-packages
   '(leuven-theme elpy zenburn-theme web-mode zop-to-char yaml-mode which-key use-package solarized-theme smex smartparens rg rainbow-delimiters org-cliplink org-bullets minitest magit macrostep ivy-hydra expand-region exec-path-from-shell evil-surround evil-leader evil-commentary enh-ruby-mode diminish counsel-projectile avy alchemist aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
