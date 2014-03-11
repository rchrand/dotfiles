(require 'evil)
(require 'evil-leader)
(require 'ace-jump-mode)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)

;; Leaders
(evil-leader/set-key
  "x"  'execute-extended-command
  "g"  'magit-status
  "f"  'find-file
  "b"  'helm-buffers-list
  "e"  'term
  "o"  'other-window
  "a"  'ag
  "w"  'save-buffer
  "s"  'fiplr-find-file
  "u"  'undo-tree-visualize
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'projectile-grep)

(global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt) ;; This increas a number by one
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(provide 'rune-evil)
