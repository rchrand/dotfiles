(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start t)
(setq-default ac-dwim nil)
(ac-config-default)

(setq ac-use-fuzzy 1)
(setq ac-auto-show-menu t)

;; Use Emacs built int TAB
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq completion-cycle-threshold 5)

(dolist (mode '(haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode js3-mode css-mode less-css-mode sql-mode
                sql-interactive-mode
                inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))

(provide 'rune-ac)
