;;;package --
;(require 'org-mode)

(setq org-todo-keywords '((sequence "TODO" "DONE" "CANCELED" "REVIEW" "DOING" "BUG" "ON HOLD")))
(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("CANCELED" . "red")
   ("REVIEW" . "chocolate4")
   ("BUG" . "gold2")
   ("DONE" . "OliveDrab4")
   ("ON HOLD" . "dark red")
   ("DOING" . "DodgetBlue3")))

(setq org-agenda-files (list "~/org/TODO.org" "~/org/CS.org" "~/org/Personal.org" "~/org/Work.org"))

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
