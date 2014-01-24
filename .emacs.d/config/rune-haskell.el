(require 'haskell-mode)
(add-hook 'haskell-mode-hook 
          (lambda () 
            (turn-on-haskell-doc-mode)
            (setq evil-auto-indent nil)
            (turn-on-haskell-indentation)
            (ghc-init)))

(setq haskell-font-lock-symbols t)
(setq haskell-stylish-on-save t)

(provide 'rune-haskell)
