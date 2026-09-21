;;; git-config.el --- Git workflow -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :defer t
  :bind (("s-g" . git-timemachine)))

(use-package diff-hl
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  (setq diff-hl-global-modes '(not image-mode pdf-view-mode)
        diff-hl-show-staged-changes nil
        diff-hl-update-async t
        vc-git-diff-switches '("--histogram"))
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'git-config)
;;; git-config.el ends here
