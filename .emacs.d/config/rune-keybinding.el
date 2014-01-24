;; A file for my keybindings

(global-set-key (kbd "C-c s s")
                      (lambda ()
                        (interactive)
                        (shell-command "bash ~/scripts/startup.sh")))


(global-set-key (kbd "C-c s d")
                      (lambda ()
                        (interactive)
                        (shell-command "bash ~/scripts/startupdk.sh")))

(global-set-key (kbd "M-[") 'forward-paragraph)
(global-set-key (kbd "M-]") 'backward-paragraph)

(global-set-key (kbd "C-x ;") 'kill-some-buffers)
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument

(global-set-key [f5] 'term)
(global-set-key [f6] 'ag)

(global-set-key (kbd "C-c C-j") 'emmet-expand-line) ; Emmet expand
(global-set-key (kbd "C-c x") 'magit-status)

(global-set-key (kbd "C-c i")
                (lambda()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c t")
                (lambda()
                  (interactive)
                  (find-file "~/org/TODO.org")))

(global-set-key (kbd "C-c e")
                (lambda()
                  (interactive)
                  (find-file "~/org/Everything.org")))




(provide 'rune-keybinding)
