(add-to-list 'load-path (concat user-emacs-directory "/config"))
(setq inhibit-splash-screen t)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(package-initialize)


(defvar my-packages '(
		    ;; Utilities
		    f s dash
		    ;; Core
                    auto-complete diminish dired-details
                    key-chord exec-path-from-shell
                    magit ignoramus perspective
                    linum-relative

		    ;; Evil stuff
                    evil evil-numbers evil-paredit evil-leader
		    ace-jump-mode 

                    ;; Searching
                    ag
                    findr fiplr grizzl
                    projectile
                    helm

                    ;; Fly
                    flycheck flymake-cursor
                    popup popwin

                    ;; Color themes
                    spacegray-theme

                    ;; Langauges
                    cider clojure-mode ;; Clojure
                    org org-plus-contrib ;; Org
                    geiser slime paredit rainbow-delimiters ;; Lisp
                    sml-mode yaml-mode ;; Misc

                    ;; Web development
                    emmet-mode
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

;; My own configs in here
(require 'rune-evil)
(require 'rune-ac)
(require 'rune-flycheck)
(require 'rune-javascript)
(require 'rune-web)
(require 'rune-ui)
(require 'rune-ido)
(require 'rune-modeline)
(require 'rune-editor)
(require 'rune-orgmode)
(require 'rune-misc)
(require 'rune-lisp)
(require 'rune-helm)
;(require 'rune-haskell)
(require 'rune-ruby)
(require 'rune-functions)
(require 'rune-keybinding)
