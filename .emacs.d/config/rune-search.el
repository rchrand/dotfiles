(require 'grizzl)
(require 'fiplr)
(setq fiplr-root-markers '(".git" ".svn" "node_modules"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules"))
			    (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key (kbd "C-c p") 'fiplr-find-file)

(require 'ag)
(setq-default ag-highlight-search t)

(provide 'rune-search)
