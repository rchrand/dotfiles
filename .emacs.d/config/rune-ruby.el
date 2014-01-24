(require 'ruby-end)
(require 'haml-mode)
(require 'yaml-mode)
(require 'rinari)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
	  (lambda () (rinari-minor-mode)))
(add-hook 'web-mode-hook
	  (lambda () (rinari-minor-mode)))

(add-hook 'haml-mode-hook
	  (lambda () (rinari-minor-mode)))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide 'rune-ruby)
