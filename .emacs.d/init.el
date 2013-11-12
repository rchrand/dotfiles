;;; Packages folders for Emacs
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(load "~/.emacs.d/plugins.el")
(load "~/.emacs.d/keybinding.el")
(load "~/.emacs.d/basic-emacs.el")
(load "~/.emacs.d/shell.el")
(load "~/.emacs.d/eshell-config.el")
(load "~/.emacs.d/org-mode-config.el")
(load "~/.emacs.d/evil-config.el")
;(load "~/.emacs.d/mode-line.el")
;(load "~/.emacs.d/magit.el")

;;; Setting some things up for ido-mode
(require 'ido)
(setq ido-enable-flex-matching t) ;; enables fuzzy matching
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)
(ido-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;;; Misc
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;(global-hl-line-mode +1)
;(global-linum-mode t)

(global-visual-line-mode 1) ; Removes the annyoing arrows when a line reach the end of the screen
(electric-pair-mode 1) ; Auto ending ( { and [

(transient-mark-mode 1) ; makes the region visible
;(line-number-mode 1)    ; makes the line number show up
(column-number-mode -1)  ; makes the column number show up
(global-subword-mode t)

(put 'dired-find-alternate-file 'disabled nil)

;(set-frame-font "Anonymous Pro")
(set-frame-font "Inconsolata")
(set-face-attribute 'default nil :height 101) ; Font size
(global-font-lock-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ;; Makes the default yes and no to just y and n
(setq inhibit-startup-echo-area-message t) ; Removes the Startup-screen
(setq inhibit-startup-message t) ; Removes the Startup-screen
(setq initial-buffer-choice "~/Documents/Org/Everything.org") ; Starts my TODO.org file instead of standard screen

(setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f")) ; System Name

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; Load theme-path
(load-theme 'zenburn t) ; load theme

(setq auto-save-default nil)
(setq make-backup-files nil)

;; Frame size
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 90))

;; Scroll behaveior
(setq redisplay-dont-pause t)
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 1000)
(setq scroll-preserve-screen-position 1 )

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Tabs&Spaces
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))

;; Mode line defaults
(setq-default mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 110)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))

    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 2 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray20" :background "gray40"
    :inverse-video nil
    :box '(:line-width 2 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 100)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
(put 'narrow-to-region 'disabled nil)