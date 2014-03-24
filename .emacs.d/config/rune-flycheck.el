(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
      flycheck-idle-change-delay 0.8)

;; popwin
(require 'popwin)
(popwin-mode 1)

(provide 'rune-flycheck)
