;;; package -- Summary
;;; Commentary:
;;; Code:

;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-files)
;; (require 'helm-eshell)
;; (require 'helm-grep)

;; (global-unset-key (kbd "C-c h"))
;; (setq helm-command-prefix "C-c h")

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)

;; Make the keybindings behave
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-z") 'helm-select-action)

;; (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;; (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;; (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

;; (setq
;;  helm-google-suggest-use-curl-p t
;;  helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
;;  helm-quick-update t ; do not display invisible candidates
;;  helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
;;  helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
;;  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

;;  helm-split-window-default-side 'other ;; open helm buffer in another window
;;  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
;;  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
;;                                      '(picture-mode artist-mode))
;;  helm-candidate-number-limit 200 ; limit the number of displayed canidates
;;  helm-M-x-requires-pattern 0     ; show all candidates when set to 0
;;  helm-boring-file-regexp-list
;;  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
;;  helm-ff-file-name-history-use-recentf t
;;  helm-move-to-line-cycle-in-source t ; move to end or beginning of source
;;                                         ; when reaching top or bottom of source.
;;  ido-use-virtual-buffers t      ; Needed in helm-buffers-list
;;  helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
;;  )
