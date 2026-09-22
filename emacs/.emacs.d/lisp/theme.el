;;; theme.el --- Theme and visuals -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(defconst rchrand/light-theme 'doom-solarized-light
  "Preferred light theme.")

(defconst rchrand/dark-theme 'doom-material-dark
  "Preferred dark theme.")

(defun rchrand/apply-theme (theme)
  "Disable active themes and load THEME."
  (interactive
   (list
    (intern
     (completing-read "Theme: "
                      (mapcar #'symbol-name (custom-available-themes))
                      nil t))))
  (require 'doom-themes)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(defun rchrand/load-light-theme ()
  "Load the preferred light theme."
  (interactive)
  (rchrand/apply-theme rchrand/light-theme))

(defun rchrand/load-dark-theme ()
  "Load the preferred dark theme."
  (interactive)
  (rchrand/apply-theme rchrand/dark-theme))

(defun rchrand/toggle-theme ()
  "Toggle between the preferred light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) rchrand/light-theme)
      (rchrand/load-dark-theme)
    (rchrand/load-light-theme)))

(straight-use-package 'doom-themes)

(use-package doom-themes
  :defer t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (add-hook 'after-init-hook #'rchrand/load-dark-theme))

(global-set-key (kbd "C-c t t") #'rchrand/toggle-theme)
(global-set-key (kbd "C-c t l") #'rchrand/load-light-theme)
(global-set-key (kbd "C-c t d") #'rchrand/load-dark-theme)
(global-set-key (kbd "C-c t s") #'rchrand/apply-theme)

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'theme)
;;; theme.el ends here
