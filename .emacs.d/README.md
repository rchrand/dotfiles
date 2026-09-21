# Emacs configuration

Personal Emacs 31 setup for writing and development.

## Active modules

- UI, editing, file browsing, project navigation, and search
- Startup dashboard with recent files, projects, and bookmarks
- Magit, git-timemachine, and diff-hl
- direnv/Nix environments through envrc
- YAML, Terraform, and Ghostel terminal support
- LSP, Company, Flycheck, snippets, and tree-sitter modes
- Combobulate structural editing for supported tree-sitter languages
- Plain C/Raylib editing, builds, runs, and TAGS navigation
- Common Lisp through SBCL, SLIME, and Quicklisp-aware REPL commands
- Prose, ordinary Org editing, and themes

Minibuffer completion uses Vertico, Orderless, Consult, Embark, and Marginalia.
Ghostel downloads its native module into `savefile/ghostel/` on first use.
Combobulate uses `C-c n` and activates only in supported tree-sitter buffers;
plain C remains deliberately unaffected.

The work dashboard/agenda, its popup rules, Rust, Zig, journaling, and the
legacy Org module remain disabled in `init.el`.

## External tools

The configuration expects `rg`, `fd`, `direnv`, and
`typescript-language-server`.
language runtimes and formatters. TypeScript uses the repository's own
`tsserver` and Prettier; Ruby uses Bundler, Sorbet, and RuboCop; Python uses the
nearest `.venv`, Pyright, and Ruff. C projects use Make or Just when present;
Raylib single-file builds additionally expect `pkg-config` and Raylib.

Tree-sitter grammars install on demand. Ruby, TypeScript, and TSX grammars are
required by the smoke test.

## Common Lisp

Install SBCL with `brew install sbcl`. SLIME is managed and pinned by Straight;
Quicklisp remains the Lisp-side library manager. Install Quicklisp with its
[official bootstrap](https://www.quicklisp.org/beta/), then evaluate
`(ql:add-to-init-file)` in SBCL so every SLIME session has access to it. Do not
load Quicklisp's `slime-helper.el`, as that would create a second Emacs-side
SLIME installation. Start the session with `C-c l s` or `M-x slime`; use
`,ql SYSTEM` in the REPL to quickload a system.

## Verification

Run:

```sh
bin/test-config
```

Straight package revisions are recorded in `straight/versions/default.el`.
Use `M-x straight-thaw-versions` to restore them on another machine.
