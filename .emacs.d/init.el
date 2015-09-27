(require 'package)
(require 'cl)

;; Load anything in site-lisp
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp")))

;; Needs melpa for any interesting packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun install-package (package)
  "Installs a single package"
  (unless (package-installed-p package)
    (package-install package)))

(defun install-packages (packages)
  "Installs multiple packages"
  (mapc #'install-package packages))

;; Bootstrap use-package and it's friends
(install-packages '(bind-key use-package diminish))

(require 'bind-key)
(require 'diminish)
(require 'use-package)

(use-package dash
  :ensure t)

;; General stuff
(show-paren-mode 1) ;; Always show parens
(fset 'yes-or-no-p 'y-or-n-p)
(setq load-prefer-newer t) ;; Perfer newer byte code
(setq gc-cons-threshold 50000000) ;; Set the GC up to 50mb
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)
(setq tab-always-indent 'complete) ;; Always indent
(setq blink-matching-paren nil)
(electric-indent-mode)
(setq longlines-show-hard-newlines t)
(global-visual-line-mode t)
(setq sentence-end-double-space nil)
(setq redisplay-dont-pause t) ;; Don't redraw while doing operations

;; Stop all kind of startup nonsense
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; Make the C-. a viable prefix
(defvar ctrl-period-map)
(define-prefix-command 'ctrl-period-map)
(bind-key* "C-." 'ctrl-period-map)

(bind-key* "C-. c" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(bind-key* "C-. i" (lambda () (interactive) (find-file "~/org/info.org")))
(bind-key* "C-. s"(lambda () (interactive) (find-file "~/org/DM551/DM551.tex")) )
(bind-key* "C-. p" (lambda () (interactive) (find-file "~/org/DM552/DM552.org")) )
(bind-key* "C-. m" (lambda () (interactive) (find-file "~/org/DM549/DM549.tex")) )
(bind-key* "C-. j" (lambda () (interactive) (find-file "~/org/journal.org")) )


;; Make dired delete recursive
(put 'dired-find-alternate-file 'disabled nil)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Swap the buttons
(setq ns-function-modifier 'hyper)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Backups
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Flyspell
(use-package flyspell
  :init
   (progn
    ;; Enable spell check in program comments
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    ;; Enable spell check in plain text / org-mode
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (setq flyspell-issue-welcome-flag nil)
    (setq flyspell-issue-message-flag nil)

    ;; ignore repeated words
    (setq flyspell-mark-duplications-flag nil)

    (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))

;;    (setq-default ispell-program-name "/usr/bin/aspell")
    (setq-default ispell-list-command "list"))
  )

;; Yas
(use-package yasnippet
  :disabled
  :ensure t
  :commands (yas-expand yas-minor-mode)
  ;;:diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (yas-global-mode 1)
  :config
  (yas-load-directory "~/.emacs.d/snippets/")

  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c j") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-c J") 'yas-insert-snippet))

;; Remove useless UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set that fringe
(setq window-combination-resize t)
(setq-default fringe-indicator-alist
              '((truncation . nil) (continuation . nil)))
;; (global-hl-line-mode t)
(blink-cursor-mode 0)

;; Changing the font to an awesome one.
(set-frame-font "Monoid 12")
(setq default-frame-alist '((font . "Monoid 12")))

;; (use-package gruber-darker-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

;; Better buffer window controls
(bind-key "s-0" 'delete-window)
(bind-key "s-1" 'delete-other-windows)
(bind-key "s-2" 'split-window-below)
(bind-key "s-3" 'split-window-right)
(bind-key* "<C-return>" 'other-window)

(bind-key "M-`" 'other-frame)
(bind-key "M-§" 'other-frame)
(bind-key "M-w" 'delete-frame)

;; Better defaults
(bind-key "<RET>" 'newline-and-indent)

;; Hippe is better than dabbdrev
(bind-key [remap dabbrev-expand] 'hippie-expand)

;; Eval'ing
(bind-key "C-c C-b" 'eval-buffer)

;; These functions are taken from the great @bbatovs Prelude.

;; Makes it easy to rename a file & buffer with C-c r
(defun prelude-rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(bind-key "C-c r" 'prelude-rename-buffer-and-file)

;; Cleans the whole buffer for whitespace and re-indents it with C-c n
(defcustom prelude-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'prelude)

(defun prelude-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (call-interactively 'untabify)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (call-interactively 'indent-region)
    (whitespace-cleanup)))

(bind-key "C-c n" 'prelude-cleanup-buffer-or-region)

(defun rca/replace-danish ()
  (interactive)
  (beginning-of-buffer)
  (query-replace "ae" "{\\ae}")
  (beginning-of-buffer)
  (query-replace "oe" "{\\o}")
  (beginning-of-buffer)
  (query-replace "aa" "{\\aa}"))

;; Company-mode
(use-package company
  :ensure company
  :diminish company-mode
  :commands company-mode
  :init
  (global-company-mode 1)
  :config
  (setq company-idle-delay 0.1
        company-show-numbers t))

;; Smartparens
(use-package smartparens-config
    :ensure smartparens
    :config
    (progn
      (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; Auctex
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :preface
  (defun my-auctex-config-hook ()
    (turn-on-auto-fill)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-PDF-mode t)
    (setq ispell-parser 'tex)
    (flyspell-mode))

  :config
  (add-hook 'TeX-mode-hook #'my-auctex-config-hook)

  (use-package company-auctex
    :ensure t
    :init
    (company-auctex-init)))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :config
  ;; Indent
  (turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  ;; Doc-mode
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))

  (use-package haskell-process
    :init
    (add-hook 'haskell-mode-hook 'haskell-process)
    :config
    (setq haskell-process-suggest-remove-import-lines t)
    (setq haskell-process-auto-import-loaded-modules t)
    (setq haskell-process-type 'stack-ghci
          haskell-process-path-ghci "stack"
          haskell-process-args-ghci "ghci")
    (setq haskell-process-log t))

  (bind-key "C-c `'" 'haskell-interactive-bring)

  (use-package haskell-interactive-mode
    :init
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    :config
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-h") 'hoogle))
  )

;; Emacs lisp editing
(use-package lisp-mode
  :mode  (("\\.el\\'" . emacs-lisp-mode)
          ("\\\*scratch\*\\'" . emacs-lisp-mode))
  :config)

  ;; Ruby/Rails/Web Development
(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("\\Gemfile\\'" . enh-ruby-mode)
         ("\\Rakefile\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode))
  :config
  (use-package ruby-end
    :ensure t
    :diminish ruby-end-mode))

(use-package js2-mode
  :ensure t
  :mode (
         ("\\.js\\'" . js2-mode)
         ("\\.json\\'" . js2-mode)
         ("\\.amethyst\\'" . js2-mode))
  :commands (js2-mode)
  :interpreter "node")

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (local-set-key (kbd "RET") 'newline-and-indent))

(use-package haml-mode
  :ensure t
  :mode ("\\.haml\\'" . haml-mode)
  :commands (haml-mode))

(use-package scss-mode
  :ensure t
  :mode (("\\.scss$\\'" . scss-mode)
         ("\\.sass$\\'" . scss-mode))
  :commands (scss-mode))

(use-package emmet-mode
  :ensure t
  :commands (emmet-expand-line emmet-expand)
  :defer 2
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))
  :config
  (progn
    (bind-key "C-j" 'emmet-expand-line emmet-mode-keymap)
    (bind-key "<C-return>" 'emmet-expand emmet-mode-keymap)
    (setq emmet-indentation 2)
    (defadvice emmet-preview-accept (after expand-and-fontify activate)
      "Update the font-face after an emmet expantion."
      (font-lock-fontify-buffer))))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :commands (yaml-mode))

;; Helm & Projectile & Perspective & Ido
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :demand t
  :bind (("C-c h" . helm-command-prefix)
         ("C-c h r" . helm-recentf)
         ("M-x" . helm-M-x))
  :config
  (use-package helm-mode)
  (helm-mode 1)
  :init
  (helm-mode 1)
  ;; Autoresize
  (helm-autoresize-mode 1)
  (setq helm-split-window-in-side-p t)

  ;; Fuzzy searching
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)

  ;; Defining keys
  (define-key helm-map (kbd "<tab>")  'helm-select-action) ; list actions using C-z

  (use-package helm-projectile
    :ensure t
    :init
    (helm-projectile-on))

  (use-package helm-swoop
    :ensure t
    :bind ("M-i" . helm-swoop))

  (use-package helm-pages
    :ensure t)

  (use-package helm-ag
    :ensure t))

(use-package perspective
  :ensure t
  :bind* ("M-s" . persp-switch)
  :init
  (persp-mode)
  :config
  (use-package persp-projectile
    :ensure t))

;; OSX
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org
  :preface
  (defun rca/org-archive ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (defun my-org-mode-hook ()
    (turn-on-auto-fill))

  :bind (("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-heading))
  :config
  ;; Generally configs
  (add-hook 'org-mode-hook 'my-org-mode-hook)

  ;; Set the sequence
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|")
          (sequence "SOMEDAY(e)" "STARTED(s)" "WAITING(w)" "REVIEW(v)" "DEFER(r)")
          (sequence "|" "DONE(d)" )))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "medium blue" :weight bold))
          ("STARTED" . (:foreground "OrangeRed" :weight bold))
          ("DONE" . (:foreground "ForestGreen" :weight bold))
          ("DEFER" . (:foreground "dark violet" :weight bold))
          ("REVIEW" . (:foreground "yellow" :weight bold))
          ("SOMEDAY" . (:foreground "DeepSkyBlue3" :weight bold))
          ("WAITING" . (:foreground "gray" :weight bold))))

  ;; Capture
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-cl" 'org-store-link)

  (setq org-capture-templates
        '(("r" "Reference" entry (file+headline "~/org/reference.org" "Bucket")
          "* %?" :prepend t)))

  ;; Agenda
  (define-key global-map "\C-ca" 'org-agenda)
  ;; (setq org-agenda-files (quote ("~/Dropbox/todo.org")))
  ;; (setq org-tag-alist '(("@HOME" . ?h) ("@WORK" . ?w)))
  (setq org-agenda-tags-column -100)
  (setq org-agenda-start-on-weekday)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-window-setup 'current-window)
  (add-to-list 'org-global-properties
               '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

  ;; Refile
  (setq org-refile-targets (quote ((nil :maxlevel . 10)
                                   (org-agenda-files :maxlevel . 10))))

  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)

  ;; Exports
  (setq org-html-doctype "html5"
        org-export-with-smart-quotes t
        org-export-with-LaTeX-fragments t
        org-export-create-formula-image-program 'imagemagick
        org-latex-listings 'minted
        pdf-processp nil)

  ;; Leuven
  (set-face-attribute 'org-level-1 nil :height 150)
  
  ;; Org-babel
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-fontify-whole-heading-line t))

;; Key-chord
(use-package key-chord
  :demand t
  :ensure t
  :init
  (key-chord-mode 1)
  :config
  (defun switch-to-last-used-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer)))

  (key-chord-define-global "JJ" 'switch-to-last-used-buffer)
  (key-chord-define-global "jk" 'avy-goto-char))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :commands magit-mode
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package golden-ratio
  :ensure t)

;; Evil-mode
(use-package evil
  :disabled
  :ensure t
  :commands evil-mode
  :init
  (evil-mode 1)
  :config
  (use-package evil-leader
    :ensure t
    :init
    (global-evil-leader-mode)
    :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (evil-leader/set-leader ",")

    (evil-leader/set-key
      "<SPC>" 'avy-goto-char
      "w"  'save-buffer
      "f" 'find-file
      "c" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "i" (lambda () (interactive) (find-file "~/org/info.org"))
      "sp" (lambda () (interactive) (find-file "~/org/DM551/DM551.tex"))
      "sc" (lambda () (interactive) (find-file "~/org/DM552/DM552.tex"))
      "sd" (lambda () (interactive) (find-file "~/org/DM549/DM549.tex"))
      "j" (lambda () (interactive) (find-file "~/org/journal.org"))
      "bb" 'switch-to-buffer
      "bs" 'switch-to-buffer
      "gs" 'magit-status
      "pp" 'projectile-switch-project
      "ps" 'projectile-find-file
      "pb" 'projectile-buffers-with-file
      "pa" 'helm-projectile-ag
      "x" 'save-buffers-kill-terminal
      )))

(use-package winner-mode
  :defer t)

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package guide-key
  :ensure t
  :defer t
  :init
  (guide-key-mode 1)
  :config
  (setq guide-key/guide-key-sequence t))

(use-package racket-mode
  :ensure t
  :defer t
  :config
  (setq racket-racket-program "/Applications/Racket v6.1.1/bin/racket"
        racket-raco-program "/Applications/Racket v6.1.1/bin/raco"))

(use-package erlang-start
  :disabled t
  :ensure erlang
  :commands (erlang-mode)
  :mode ("\\.erl\\'" . erlang-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)))

(use-package rainbow-blocks
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-blocks-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :commands (er/expand-region))

(use-package visible-mark
  :ensure t
  :init
  (global-visible-mark-mode 1))

(use-package ag
  :ensure t
  :commands (ag ag-project))

(use-package zop-to-char
  :ensure t
  :bind ("M-z" . zop-up-to-char))

(use-package slim-mode
  :ensure t
  :mode ("\\.slim\\'" . slim-mode))

(use-package avy
  :ensure t
  :commands (avy-goto-char))

(use-package dired-x ; Enable some nice dired features
  :config
  (progn
    ;; Omit hidden files by default (C-x M-o to show them)
  (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
          dired-omit-verbose nil)))

(use-package prolog-mode
  :mode ("\\.pl\\'" . prolog-mode))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Jeykll blogs
(use-package markdown-mode
  :ensure t)
  
(use-package hyde
  :ensure
  :mode ("\\.md\\'" . hyde-markdown-mode))

(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'" . csharp-mode))

(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode 1))
