;;; dashboard-config.el --- Lightweight startup dashboard -*- lexical-binding: t; -*-

(use-package dashboard
  :init
  (setq dashboard-items '((recents . 8)
                          (projects . 8)
                          (bookmarks . 5))
        dashboard-projects-backend 'projectile
        dashboard-startupify-list '(dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items)
        dashboard-center-content nil
        dashboard-path-style 'truncate-middle
        dashboard-show-shortcuts t
        initial-buffer-choice #'dashboard-open)
  :config
  (dashboard-setup-startup-hook))

(provide 'dashboard-config)
;;; dashboard-config.el ends here
