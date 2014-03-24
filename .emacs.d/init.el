(add-to-list 'load-path (concat user-emacs-directory "/config"))
(setq inhibit-splash-screen t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))


(require 'package)
(package-initialize)

(defconst *is-a-mac* (eq system-type 'darwin))

(defvar my-packages '(
                      ;; Utility
                      f s dash
                        ;; Core
                        auto-complete diminish dired-details
                        key-chord exec-path-from-shell
                        magit ignoramus perspective
                        linum-relative smart-mode-line
                        yasnippet
                        dired+

                        sos ;; Stackoverflow search

                        ;; Evil stuff
                        evil evil-numbers evil-paredit evil-leader
                        ace-jump-mode

                        ;; Searching
                        ag
                        findr fiplr grizzl
                        projectile
                        ;; helm

                        ;; Ido
                        flx-ido ido-vertical-mode ido-ubiquitous
                        smex

                        ;; Fly
                        flycheck flymake-cursor popwin popup

                        ;; Color themes
                        spacegray-theme

                        ;; Langauges
                        cider clojure-mode ;; Clojure
                        org org-plus-contrib ;; Org
                        geiser slime paredit rainbow-delimiters ;; Lisp
                        sml-mode yaml-mode ;; Misc

                        ;; Web development
                        emmet-mode less-css-mode
                        haml-mode scss-mode js3-mode jade-mode
                        rinari ruby-compilation ruby-end ;; Ruby
                        web-mode))

(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages missing-packages)
      (or (package-installed-p package)
          (push package missing-packages)))))

(let ((missing (my-missing-packages)))
  (when missing
    ;; Check for new packages (package versions)
    (package-refresh-contents)
    ;; Install the missing packages
    (mapc (lambda (package)
            (when (not (package-installed-p package))
              (package-install package))) missing)
    ;; Close the compilation log.
    (let ((compile-window (get-buffer-window "*Compile-Log*")))
      (if compile-window
          (delete-window compile-window)))))

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Starts my agenda up
(setq initial-buffer-choice "/Users/rune/Documents/projects/org-files/agenda/TODO.org")

;; My own configs in here
(require 'rune-utils)
(require 'rune-evil)
(require 'rune-ac)
(require 'rune-flycheck)
(require 'rune-javascript)
(require 'rune-web)
(require 'rune-ui)
(require 'rune-ido)
;; (require 'rune-modeline)
(require 'rune-editor)
(require 'rune-orgmode)
(require 'rune-misc)
(require 'rune-lisp)
;; (require 'rune-helm)
;; (require 'rune-haskell)
(require 'rune-ruby)
(require 'rune-functions)
(require 'rune-keybinding)
(require 'rune-dired)
(require 'rune-search)
(require 'rune-git)
