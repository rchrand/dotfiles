;;; python-config.el --- Python tooling -*- lexical-binding: t; -*-

(use-package python
  :straight nil
  :mode (("\\.py\\'" . python-mode))
  :config
  (setq python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil))

(use-package lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-pyright-disable-language-service nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-diagnostic-mode "workspace"))

(use-package reformatter)

(reformatter-define ruff-format
  :program "ruff"
  :args (list "format" "--stdin-filename" (buffer-file-name) "-")
  :group 'rchrand-formatters
  :lighter " RuffFmt")

(defun rchrand/python-project-executable ()
  "Return the closest project-local Python executable, when available."
  (when-let* ((root (or (locate-dominating-file default-directory "pyproject.toml")
                        (locate-dominating-file default-directory ".git")))
              (python (expand-file-name ".venv/bin/python" root))
              ((file-executable-p python)))
    python))

(defun rchrand/python-setup ()
  "Apply Python editing, formatting, and LSP preferences."
  (setq-local electric-indent-inhibit t
              tab-width 4
              python-indent-offset 4)
  (when-let* ((python (rchrand/python-project-executable)))
    (setq-local lsp-pyright-python-executable-cmd python))
  (when (executable-find "ruff")
    (ruff-format-on-save-mode 1))
  (lsp-deferred))

(add-hook 'python-mode-hook #'rchrand/python-setup)
(add-hook 'python-ts-mode-hook #'rchrand/python-setup)

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c f") #'ruff-format-buffer)
  (when (boundp 'python-ts-mode-map)
    (define-key python-ts-mode-map (kbd "C-c f") #'ruff-format-buffer)))

(provide 'python-config)
;;; python-config.el ends here
