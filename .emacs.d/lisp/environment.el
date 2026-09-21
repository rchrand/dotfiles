;;; environment.el --- Shell and per-project environments -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH" "MANPATH" "LSP_USE_PLISTS" "PYENV_ROOT" "GOPATH"))))

(use-package envrc
  :diminish
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :config
  (define-key envrc-mode-map (kbd "C-c E") envrc-command-map)
  (envrc-global-mode 1))

(provide 'environment)
;;; environment.el ends here
