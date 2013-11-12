;;;; General stuff
; AC
(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

; Dash
(require 'dash)
(require 's)
(require 'f)

; pkg-info
(require 'pkg-info)

;; Magit
(require 'magit)

; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

;(require 'linum-relative)

(require 'rainbow-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require 'perspective)
(persp-mode)

; Ctrl-P like search
(require 'grizzl)
(require 'fiplr)

(setq fiplr-root-markers '(".git" ".svn")) ;; Root folders (Could be customized to some JS'projects)

(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'key-chord)
(key-chord-mode 1)

(require 'project-explorer)

(require 'ag)
(setq ag-highligt-search t)

(require 'smartparens)
(show-smartparens-global-mode +1)
;;;;;; Python
(require 'epc)

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;;;;; Web-development packages

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(require 'js3-mode)

(require 'php-mode)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'haml-mode-hook 'emmet-mode)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(require 'coffee-mode)

;;;;;; Lisp/Scheme/Clojure

(require 'xscheme)

(require 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup)

(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)
(require 'clojure-test-mode)
;;;;;;; Ruby & Rails development
(require 'ruby-end)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

(require 'rinari)

(add-hook 'ruby-mode-hook
	  (lambda () (rinari-minor-mode)))
(add-hook 'web-mode-hook
	  (lambda () (rinari-minor-mode)))

(add-hook 'haml-mode-hook
	  (lambda () (rinari-minor-mode)))

(require 'haml-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

