;; (prelude-require-packages '(helm helm-ag helm-swoop helm-projectile))

;; (require 'helm)
;; (require 'helm-config)

;; (setq helm-ff-transformer-show-only-basename nil
;;       ;; helm-adaptive-history-file             ers-helm-adaptive-history-file
;;       helm-boring-file-regexp-list           '("\\.git$" "\\.svn$" "\\.elc$")
;;       helm-yank-symbol-first                 t
;;       helm-buffers-fuzzy-matching            t
;;       helm-ff-auto-update-initial-value      t
;;       helm-input-idle-delay                  0.1
;;       helm-idle-delay                        0.1)

;; (autoload 'helm-descbinds      "helm-descbinds" t)
;; (autoload 'helm-eshell-history "helm-eshell"    t)
;; (autoload 'helm-esh-pcomplete  "helm-eshell"    t)

;; (global-set-key (kbd "C-h a")    #'helm-apropos)
;; (global-set-key (kbd "C-h i")    #'helm-info-emacs)
;; (global-set-key (kbd "C-h b")    #'helm-descbinds)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; ;; (add-hook 'eshell-mode-hook
;; ;;           #'(lambda ()
;; ;;               (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
;; ;;               (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

;; ;; (global-set-key (kbd "C-x b")   #'helm-mini)
;; ;; (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
;; ;; (global-set-key (kbd "C-x C-m") #'helm-M-x)
;; ;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; ;; (global-set-key (kbd "C-x C-r") #'helm-recentf)
;; ;; (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
;; ;; (global-set-key (kbd "M-y")     #'helm-show-kill-ring)
;; (global-set-key (kbd "M-x")     #'helm-M-x)
;; ;; (global-set-key (kbd "M-s o")   #'helm-swoop)
;; ;; (global-set-key (kbd "M-s /")   #'helm-multi-swoop)

;; ;; (require 'helm-config)
;; ;; (helm-mode t)
;; ;; (helm-adaptative-mode t)

;; (global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
;; (global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
;; (define-key helm-map (kbd "M-o") #'helm-previous-source)

;; (global-set-key (kbd "M-s s")   #'helm-ag)

;; ;; Auto resize
;; (helm-autoresize-mode 1)

;; (require 'org)
;; (define-key org-mode-map (kbd "C-x c o h") #'helm-org-headlines)
