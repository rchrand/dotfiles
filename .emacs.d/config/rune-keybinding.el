;; A file for my keybindings
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument

(global-set-key (kbd "C-c C-j") 'emmet-expand-line) ; Emmet expand
(global-set-key (kbd "C-c x") 'magit-status)

(global-set-key (kbd "C-c i")
		(lambda()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c t")
		(lambda()
		  (interactive)
		  (find-file "~/Documents/projects/org-files/agenda/TODO.org")))

(global-set-key (kbd "C-c e")
		(lambda()
		  (interactive)
		  (find-file "~/Documents/projects/org-files/Everything.org")))

(global-set-key (kbd "C-c o")
		(lambda()
		  (interactive)
		  (find-file "~/Documents/projects/org-files/SUM.org")))

(global-set-key (kbd "C-x C-f") '(message "Use ,f instead")) ; was digit-argument
(global-set-key (kbd "C-x b") '(message "Use ,b instead")) ; was digit-argument
(global-set-key (kbd "C-x C-s") '(message "Use ,w instead")) ; was digit-argument


(provide 'rune-keybinding)
