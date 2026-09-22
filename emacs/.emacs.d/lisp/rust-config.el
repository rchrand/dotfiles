;;; rust-config.el --- Rust tooling -*- lexical-binding: t; -*-

;; Environment syncing
(use-package envrc
  :commands (envrc-global-mode)
  :init (envrc-global-mode 1))

;; Justfiles common in Rust
(use-package just-mode
  :mode ("\\`[Jj]ustfile\\'" "\\.just\\'"))

;; Rust major modes
(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)))

(use-package cargo
  :hook ((rust-ts-mode . cargo-minor-mode)
         (rust-mode    . cargo-minor-mode)))

;; Snippets already enabled globally via yasnippet

;; LSP tuning for Rust
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck
        lsp-inlay-hint-enable t
        lsp-lens-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-load-out-dirs-from-check t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-exclude-dirs [".direnv" "target"]
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-completion-auto-import-enable t
        lsp-rust-analyzer-imports-prefer-namespace t
        lsp-rust-analyzer-import-merge-behavior "last"
        lsp-rust-analyzer-check-all-targets t
        lsp-rust-analyzer-diagnostics-experimental-enable t)

  (dolist (hook '(rust-mode-hook rust-ts-mode-hook))
    (add-hook hook #'lsp-deferred)
    (add-hook hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (when (and (bound-and-true-p lsp-mode)
                                       (lsp-feature? "textDocument/formatting"))
                              (lsp-format-buffer)
                              (lsp-organize-imports)))
                          nil t)))
    (when (fboundp 'lsp-inlay-hints-mode)
      (add-hook hook #'lsp-inlay-hints-mode))
    (add-hook hook
              (lambda ()
                (local-set-key (kbd "s-.")
                               (if (fboundp 'lsp-ui-peek-find-definitions)
                                   #'lsp-ui-peek-find-definitions
                                 #'lsp-find-definition))))))

(with-eval-after-load 'flycheck
  (dolist (hook '(rust-mode-hook rust-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (setq-local flycheck-disabled-checkers
                            (append '(rust-cargo rust rust-clippy)
                                    flycheck-disabled-checkers))))))

(use-package lsp-treemacs) ;; ensure available for dap later

;; Treemacs is already deferred elsewhere

;; Debugging (LLDB / CodeLLDB)
(use-package dap-mode
  :commands (dap-debug dap-debug-last)
  :init (setq dap-auto-configure-features '(locals expressions tooltip))
  :config
  (dap-auto-configure-mode 1)
  (require 'dap-lldb)
  (with-eval-after-load 'dap-mode
    (require 'dap-ui)
    (require 'dap-utils)
    (require 'dap-variables))
  (let* ((codelldb (cond
                    ((file-exists-p "/Applications/CodeLLDB.app/Contents/Resources/adapter/codelldb")
                     "/Applications/CodeLLDB.app/Contents/Resources/adapter/codelldb")
                    ((file-exists-p (expand-file-name "~/.vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb"))
                     (expand-file-name "~/.vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb"))
                    ((file-exists-p "/opt/homebrew/opt/codelldb/extension/adapter/codelldb")
                     "/opt/homebrew/opt/codelldb/extension/adapter/codelldb")))
         (lldb-vscode (or (executable-find "lldb-vscode")
                          (and (file-exists-p "/opt/homebrew/opt/llvm/bin/lldb-vscode")
                               "/opt/homebrew/opt/llvm/bin/lldb-vscode")
                          (and (file-exists-p "/usr/local/opt/llvm/bin/lldb-vscode")
                               "/usr/local/opt/llvm/bin/lldb-vscode"))))
    (cond
     (codelldb (setq dap-lldb-debug-program (list codelldb)))
     (lldb-vscode (setq dap-lldb-debug-program (list lldb-vscode)))))
  (dap-register-debug-template "Rust::Run Bin"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "Rust::Run Bin"
                                     :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
                                     :cwd "${workspaceFolder}"))
  (dap-register-debug-template "Rust::Run Tests"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "Rust::Run Tests"
                                     :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}-<test>"
                                     :cwd "${workspaceFolder}")))

;; Cargo transient
(use-package transient)
(with-eval-after-load 'transient
  (transient-define-prefix rchrand/rust-cargo-menu ()
    ["Cargo"
     [("b" "build"     cargo-process-build)
      ("r" "run"       cargo-process-run)
      ("t" "test"      cargo-process-test)
      ("c" "clippy"    cargo-process-clippy)
      ("f" "fmt"       cargo-process-fmt)
      ("d" "doc"       cargo-process-doc)
      ("a" "add dep"   cargo-process-add)
      ("R" "rm dep"    cargo-process-rm)
      ("u" "upgrade"   cargo-process-upgrade)
      ("o" "outdated"  cargo-process-outdated)]])
  (global-set-key (kbd "C-c r") #'rchrand/rust-cargo-menu))

(defun rchrand/rust-open-docs ()
  "Open documentation for symbol at point using rust-analyzer hover links."
  (interactive)
  (if (and (bound-and-true-p lsp-mode)
           (lsp-feature? "textDocument/hover"))
      (lsp-ui-doc-show)
    (message "LSP hover not available")))

(defun rchrand/rust-verify-tools ()
  "Report status of rustup, rust-analyzer, cargo, clippy, and rustfmt."
  (interactive)
  (let* ((have (lambda (exe) (when (executable-find exe) exe)))
         (rustup (funcall have "rustup"))
         (cargo  (funcall have "cargo"))
         (ra     (funcall have "rust-analyzer"))
         (clippy (and cargo (string-match-p "clippy" (shell-command-to-string "rustup component list --installed"))))
         (fmt    (and cargo (string-match-p "rustfmt" (shell-command-to-string "rustup component list --installed")))))
    (message (mapconcat #'identity
                        (delq nil (list (format "rustup: %s" (if rustup "OK" "MISSING"))
                                        (format "cargo: %s" (if cargo "OK" "MISSING"))
                                        (format "rust-analyzer: %s" (if ra "OK" "MISSING"))
                                        (format "clippy: %s" (if clippy "OK" "MISSING"))
                                        (format "rustfmt: %s" (if fmt "OK" "MISSING"))))
                        ", "))))

(provide 'rust-config)
;;; rust-config.el ends here
