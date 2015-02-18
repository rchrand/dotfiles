;; Window control
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-0") 'delete-window)

;; Extra key-chord config
(require 'key-chord)

(key-chord-define-global "xo" 'ace-window)
(key-chord-define-global ",f" 'ido-find-file)
(key-chord-define-global ",b" 'ido-switch-buffer)
(key-chord-define-global ",w" 'save-buffer)
(key-chord-define-global ",x" nil) ;; TODO, Save buffer and close the buffer

;; Evil specific
(key-chord-define-global "uu" nil)
(key-chord-define-global "yy" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "jj" nil)
(key-chord-define-global "jk" nil)

(require 'evil)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Org-mode
(global-set-key (kbd "C-c p") 'gf/toggle-switch-to-project-org-file)
(global-set-key (kbd "M-n") 'outline-next-visible-heading)
(global-set-key (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key "\C-cc" 'org-capture)

;; Unicode chars matching the mac keymap with dvorak
(global-set-key (kbd "s-'") (lambda () (interactive) (insert "æ")))
(global-set-key (kbd "s-\"") (lambda () (interactive) (insert "Æ")))

(global-unset-key (kbd "s-o"))
(define-key prelude-mode-map (kbd "s-o") nil)
(global-set-key (kbd "s-o") (lambda () (interactive) (insert "ø")))
(global-set-key (kbd "s-O") (lambda () (interactive) (insert "Ø")))


(global-set-key (kbd "s-a") (lambda () (interactive) (insert "å")))
(global-set-key (kbd "s-A") (lambda () (interactive) (insert "Å")))

(provide 'keybindings)
;;; keybindings.el ends here
