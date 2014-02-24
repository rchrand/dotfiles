;;;package --
;(require 'org-mode)

(setq org-todo-keywords '((sequence "TODO" "DONE" "ON HOLD" "REVIEW" "DOING" "BUG" "IF TIME" "CANCELED")))
(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("CANCELED" . "DarkRed")
   ("REVIEW" . "chocolate4")
   ("BUG" . "gold2")
   ("DONE" . "OliveDrab4")
   ("ON HOLD" . "tomato4")
   ("IF TIME" . "OrangeRed")
   ("DOING" . "PeachPuff")))

(setq org-agenda-files (list "~/Documents/projects/org-files/agenda/TODO.org"
                             "~/Documents/projects/org-files/agenda/CS.org"
                             "~/Documents/projects/org-files/agenda/Work.org"
                             "~/Documents/projects/org-files/agenda/Personal.org"))

(setq org-startup-with-inline-images t)

(setq org-startup-indented t)
(global-set-key (kbd "M-n") 'outline-next-visible-heading)
(global-set-key (kbd "M-p") 'outline-previous-visible-heading)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))

(setq org-agenda-start-on-weekday 0)

(provide 'rune-orgmode)
