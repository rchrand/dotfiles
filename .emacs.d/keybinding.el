;;; Keybindings
(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;;; Code:
(global-set-key (kbd "C-x ;") 'kill-some-buffers)
(define-key global-map (kbd "RET") 'newline-and-indent) ; When pressing RET (Enter) makes a new line and ident it

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

; Key-chord improvement
;(key-chord-define-global "ff" 'ido-find-file)
;(key-chord-define-global "bb" 'ido-switch-buffer)

;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)

(global-set-key [f5] 'shell)
(global-set-key [f2] 'project-explorer-open)
(global-set-key [f6] 'ag)

;(global-set-key (kbd "C-c y") 'copy-line)

(global-set-key (kbd "C-c C-j") 'emmet-expand-line) ; Emmet expand

;; Fipir
(global-set-key (kbd "C-c f") 'fiplr-find-file)
(global-set-key (kbd "C-c F") 'fiplr-find-directory)

;; Magit
(global-set-key (kbd "C-c x") 'magit-status)
