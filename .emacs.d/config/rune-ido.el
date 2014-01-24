(require 'ignoramus)
(ignoramus-setup)

(require 'ido)
(setq ido-enable-flex-matching t) ;; enables fuzzy matching
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)

(provide 'rune-ido)
