(require 'smex)
(require 'ignoramus)
(ignoramus-setup)

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)

;(setq ido-enable-flex-matching t) ;; enables fuzzy matching
(ido-mode t)
(ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)
(setq ido-use-filename-at-point nil)

(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(smex-initialize)
(provide 'rune-ido)
