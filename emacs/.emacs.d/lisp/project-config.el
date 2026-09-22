;;; project-config.el --- Project navigation and search -*- lexical-binding: t; -*-

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (defun rchrand/projectile-switch-project-action ()
    "Switch to a project-named perspective, then select one of its files."
    (persp-switch (projectile-project-name))
    (projectile-find-file))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-generic-command "fd . -0 --type f --color=never"
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" rchrand-savefile-dir)
        projectile-switch-project-action
        #'rchrand/projectile-switch-project-action)
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --color=never -0"))
  (define-key projectile-command-map (kbd "B") #'rchrand/project-buffers-list)
  (define-key projectile-command-map (kbd "p") #'projectile-switch-project)
  (define-key projectile-command-map (kbd "m") #'ghostel-project)
  (define-key projectile-command-map (kbd "M") #'ghostel-project-list-buffers)
  (define-key projectile-command-map (kbd "s r") #'rchrand/project-search-dwim)
  (define-key projectile-command-map (kbd "s g") #'rchrand/project-search-dwim)
  (define-key projectile-command-map (kbd "/") #'rchrand/project-search-dwim)
  (define-key projectile-command-map (kbd "o") #'rchrand/find-file-in-project))

;; Ignore Zig build folders globally in Projectile
(with-eval-after-load 'projectile
  (dolist (dir '("zig-cache" "zig-out"))
    (add-to-list 'projectile-globally-ignored-directories dir)))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "r"
        wgrep-change-readonly-file t))

(defun rchrand/project-buffers-list ()
  "Show buffers for current project with enhanced display."
  (interactive)
  (if (projectile-project-p)
      (consult-project-buffer)
    (message "Not in a project")))

(defun rchrand/find-file-in-project ()
  "Find file in project with better UX."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (call-interactively #'find-file)))

(global-set-key (kbd "C-c F") #'rchrand/find-file-in-project)

(use-package recentf
  :straight nil
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-save-file (expand-file-name "recentf.eld" rchrand-savefile-dir)
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "COMMIT_EDITMSG" ".*\\.gz$"))
  (recentf-mode +1))

(defun rchrand/project-search-dwim ()
  "Smart project search that uses rg if available, falls back gracefully."
  (interactive)
  (consult-ripgrep (if (projectile-project-p)
                       (projectile-project-root)
                     default-directory)))

(global-set-key (kbd "C-c S") #'rchrand/project-search-dwim)

(provide 'project-config)
;;; project-config.el ends here
