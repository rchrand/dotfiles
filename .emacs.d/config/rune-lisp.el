;;; package --- Summary
;;; Commentary:
(add-to-list 'load-path "~/.emacs.d/vendor/")
;;; Code:
(require 'cider)

(require 'clojure-mode)
(setq ag-highligt-search t)

;(require 'xscheme)

(require 'slime)
(add-hook 'lisp-mode-hook 'enable-paredit-mode )
(add-hook 'scheme-mode-hook 'enable-paredit-mode )
(add-hook 'clojure-mode-hook 'enable-paredit-mode )

(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy))
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(provide 'rune-lisp)
;;; rune-lisp.el ends here
