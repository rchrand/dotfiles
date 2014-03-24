(require 'ruby-mode)
(require 'ruby-end)
(require 'haml-mode)
(require 'yaml-mode)
(require 'rinari)

(after-load 'rinari
	    (diminish 'rinari-minor-mode "Rin"))

(global-rinari-mode)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"
	       "\\Vagrantfile\\'" "\\Capfile\\'" "\\.gemspec\\'")

(setq ruby-use-encoding-map nil)

;; This is because of default AC
(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide 'rune-ruby)
