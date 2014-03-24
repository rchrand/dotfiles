(require 'helm)
(require 'helm-files)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-i") 'helm-select-action)

(setq helm-quick-update t
      helm-idle-delay 0.01
      helm-input-idle-delay 0.01)

(setq helm-ff-file-name-history-use-recentf t)

;(helm-mode 1)
(provide 'rune-helm)
