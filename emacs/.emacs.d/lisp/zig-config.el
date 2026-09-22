;;; zig-config.el --- Zig language tooling -*- lexical-binding: t; -*-

(use-package zig-mode
  :mode (("\\.zig\\'" . zig-mode))
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . rchrand/zig-setup))
  :init
  (defun rchrand/zig-setup ()
    "Local Zig defaults: LSP, formatting, and indentation."
    (setq-local tab-width 4
                indent-tabs-mode nil)
    (add-hook 'before-save-hook #'rchrand/zig-format-buffer-save nil t))

  (defun rchrand/zig-format-buffer-save ()
    "Format current Zig buffer on save using the best available method."
    (cond
     ((and (bound-and-true-p lsp-mode)
           (lsp-feature? "textDocument/formatting"))
      (ignore-errors
        (lsp-format-buffer)
        (when (lsp-feature? "textDocument/codeAction")
          (ignore-errors (lsp-organize-imports)))))
     ((fboundp 'zig-format-buffer)
      (ignore-errors (zig-format-buffer)))))
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-zig-zls-executable (or (executable-find "zls") "/opt/homebrew/bin/zls"))
    (define-key zig-mode-map (kbd "C-c C-l r") #'lsp-rename)
    (define-key zig-mode-map (kbd "C-c C-l a") #'lsp-execute-code-action)
    (define-key zig-mode-map (kbd "C-c C-l d") #'lsp-find-definition)
    (define-key zig-mode-map (kbd "C-c C-l R") #'lsp-find-references)))

;; Helper to verify external Zig tooling
(defun rchrand/zig-verify-tools ()
  "Report availability of zig and zls, with install suggestions on macOS."
  (interactive)
  (let* ((zig (executable-find "zig"))
         (zls (executable-find "zls"))
         (os  (symbol-name system-type))
         (tips (cond
                ((string-prefix-p "darwin" os)
                 (concat (unless zig "  brew install zig\n")
                         (unless zls "  brew install zls\n")))
                ((string-prefix-p "gnu/linux" os)
                 (concat (unless zig "  See https://ziglang.org/download/\n")
                         (unless zls "  See https://github.com/zigtools/zls#installation\n")))
                (t ""))))
    (message (concat
              (format "zig: %s, zls: %s\n"
                      (if zig (format "OK (%s)" zig) "MISSING")
                      (if zls (format "OK (%s)" zls) "MISSING"))
              (if (string-empty-p tips) "" (concat "Install tips:\n" tips))))))

(with-eval-after-load 'transient
  (transient-define-prefix rchrand/zig-menu ()
    ["Zig"
     [("b" "build"    rchrand/zig-build)
      ("R" "build run" rchrand/zig-build-run)
      ("t" "test file" rchrand/zig-test-this-file)
      ("T" "test all"  rchrand/zig-test-all)
      ("r" "run file"  rchrand/zig-run-this-file)
      ("f" "fmt file"  rchrand/zig-format-now)
      ("v" "verify"    rchrand/zig-verify-tools)]])
  (global-set-key (kbd "C-c z") #'rchrand/zig-menu))

(defun rchrand/zig--project-root ()
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (ignore-errors (car (project-roots (project-current))))
      default-directory))

(defun rchrand/zig-build ()
  "Run `zig build` from project root."
  (interactive)
  (let ((default-directory (rchrand/zig--project-root)))
    (compile "zig build")))

(defun rchrand/zig-build-run ()
  "Run `zig build run` from project root."
  (interactive)
  (let ((default-directory (rchrand/zig--project-root)))
    (compile "zig build run")))

(defun rchrand/zig-test-this-file ()
  "Run `zig test` on the current buffer file."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (let ((default-directory (rchrand/zig--project-root)))
        (compile (format "zig test %s" (shell-quote-argument file))))
    (user-error "Current buffer is not visiting a file")))

(defun rchrand/zig-test-all ()
  "Run `zig build test` from project root."
  (interactive)
  (let ((default-directory (rchrand/zig--project-root)))
    (compile "zig build test")))

(defun rchrand/zig-run-this-file ()
  "Run `zig run` for the current buffer file."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (let ((default-directory (rchrand/zig--project-root)))
        (compile (format "zig run %s" (shell-quote-argument file))))
    (user-error "Current buffer is not visiting a file")))

(defun rchrand/zig-format-now ()
  "Format buffer immediately using LSP or zig-mode formatter."
  (interactive)
  (save-buffer)
  (rchrand/zig-format-buffer-save)
  (save-buffer))

(provide 'zig-config)
;;; zig-config.el ends here
