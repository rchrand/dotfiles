;; Startup buffer
(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (find-file "~/org/todo.org")))

;; Perspective
(require 'perspective)
(persp-mode 1)

;; Mac spefic settings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Ag
(prelude-require-packages '(ag))

;; Ido mode
(prelude-require-packages '(smex ido-vertical-mode))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(provide 'defaults)
;;; defaults.el ends here
