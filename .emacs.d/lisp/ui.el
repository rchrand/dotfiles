;;; ui.el --- UI tweaks and keybindings -*- lexical-binding: t; -*-

;; Basic UI toggles
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

;; Conservative redisplay optimizations borrowed from Doom's core defaults.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1))

;; font selection
(defun rchrand/apply-default-font (&optional frame)
  "Apply preferred monospaced font to FRAME or current frame."
  (with-selected-frame (or frame (selected-frame))
    (cond
     ((find-font (font-spec :name "JetBrains Mono"))
      (set-frame-font "JetBrains Mono 14" nil t))
     ((find-font (font-spec :name "Fira Code"))
      (set-frame-font "Fira Code-14" nil t))
     ((find-font (font-spec :name "Menlo"))
      (set-frame-font "Menlo-14" nil t))
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (set-frame-font "DejaVu Sans Mono-14" nil t))
     ((find-font (font-spec :name "Inconsolata"))
      (set-frame-font "Inconsolata-14" nil t)))))

(rchrand/apply-default-font)
(add-hook 'after-make-frame-functions #'rchrand/apply-default-font)

;; Mode line settings
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; Never soft-wrap; keep long lines on one visual line
(setq-default truncate-lines t)
(setq-default word-wrap nil)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'text-mode-hook (lambda () (setq truncate-lines t)))

;; Disable line numbers in shells, terminals, and tree views
(dolist (mode '(term-mode eshell-mode shell-mode ghostel-mode vterm-mode treemacs-mode))
  (add-hook (intern (format "%s-hook" mode)) (lambda () (display-line-numbers-mode 0))))

;; frame title shows path or buffer
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Keybindings
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; Align regexp on C-x \ (use explicit vector to avoid kbd parsing quirks)
(global-set-key [?\C-x ?\\] #'align-regexp)
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-S-<left>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<right>") #'shrink-window-horizontally)
(global-set-key (kbd "C-S-<up>") #'enlarge-window)
(global-set-key (kbd "C-S-<down>") #'shrink-window)
(global-unset-key (kbd "C-z"))

(defun rchrand/goto-init-file ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun rchrand/open-keybindings-cheatsheet ()
  "Open the keybindings cheat sheet file."
  (interactive)
  (find-file (expand-file-name "KEYBINDINGS.org" user-emacs-directory)))

(defun rchrand/reload-init-file ()
  "Reload the init file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") #'rchrand/goto-init-file)
(global-set-key (kbd "C-c K") #'rchrand/open-keybindings-cheatsheet)
(global-set-key (kbd "C-c R") #'rchrand/reload-init-file)

(use-package switch-window
  :bind (("C-x o" . switch-window)))

(provide 'ui)
;;; ui.el ends here
