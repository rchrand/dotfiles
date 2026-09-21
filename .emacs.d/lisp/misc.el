;;; misc.el --- Miscellaneous languages/tools -*- lexical-binding: t; -*-

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :config
  (setq terraform-indent-level 2))

(use-package ghostel
  :straight (:type git :host github :repo "dakra/ghostel"
             :files (:defaults "etc"))
  :commands (ghostel ghostel-project ghostel-project-list-buffers)
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s" . consult-line))
  :init
  (setq ghostel-module-directory
        (expand-file-name "ghostel/" rchrand-savefile-dir)
        ghostel-module-auto-install 'download)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands
               '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-hook 'ghostel-pre-spawn-hook
            #'rchrand/ghostel-strip-direnv-environment))

(defun rchrand/ghostel-strip-direnv-environment ()
  "Let direnv initialize normally in a newly spawned Ghostel shell."
  (when (getenv "DIRENV_FILE")
    (setq-local process-environment
                (seq-remove
                 (lambda (entry) (string-prefix-p "DIRENV_" entry))
                 process-environment))))

(provide 'misc)
;;; misc.el ends here
