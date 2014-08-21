;;; package -- Summary
;;; Commentary:
;;; Code:

;; Change startup buffer
(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (org-agenda nil "n")
                             (split-window-right)
                             (find-file "~/org/todo.org")))

;; Mac spefic settings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Extra key-chord config
(require 'key-chord)

(key-chord-define-global "xo" 'ace-window)
(key-chord-define-global ",f" 'ido-find-file)
(key-chord-define-global ",b" 'ido-switch-buffer)
(key-chord-define-global ",w" 'save-buffer)
(key-chord-define-global "YY" 'browse-kill-ring)

;; (key-chord-define-global "jk" nil)
;; (key-chord-define-global "uu" nil)
;; (key-chord-define-global "yy" nil)
;; (key-chord-define-global "jj" nil)
;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; Perspective
(prelude-require-packages '(perspective))
(require 'perspective)
(persp-mode 1)

;; Ido vertical
(prelude-require-packages '(ido-vertical-mode))
(ido-vertical-mode 1)

;; Misc settings
(setq echo-keystrokes 0.1)
(global-unset-key (kbd "s-n"))

;; (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-char-mode)

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "s-g") 'god-mode-all)

(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)

(define-key god-local-mode-map (kbd ">") 'end-of-buffer)
(define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)

(provide 'misc)
;;; misc.el ends here
