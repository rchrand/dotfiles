(add-to-list 'load-path "~/.emacs.d/vendor/")
(put 'dired-find-alternate-file 'disabled nil)

(require 'linum-relative)
(global-linum-mode)
   
(global-set-key (kbd "C-c k")
                (lambda ()
                  (interactive)
                  (if (equal t global-linum-mode)
                      (global-linum-mode 0)
                    (global-linum-mode 1))))


;; Smooth scrolling
(setq redisplay-dont-pause t
      mouse-wheel-progressive-speed nil
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Remove the backup files - urgh
(setq make-backup-files nil)

;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)


(provide 'rune-misc)
