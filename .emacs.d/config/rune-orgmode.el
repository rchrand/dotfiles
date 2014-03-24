;;;package --
;; Org keywords with colors
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

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-startup-indented t
      org-startup-with-inline-images t
      org-fontify-done-headline t
      org-tags-column 80)

; Some navigations keybindings
(global-set-key (kbd "M-n") 'outline-next-visible-heading)
(global-set-key (kbd "M-p") 'outline-previous-visible-heading)

;; Proper keybingsings for the org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Some color changes to my org-mode
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
			     :weight normal
			     :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))


;;;; Agenda
;; My list of Agenda files
(setq org-agenda-files
      (list "~/Documents/projects/org-files/agenda/TODO.org"
	    "~/Documents/projects/org-files/agenda/CS.org"
	    "~/Documents/projects/org-files/agenda/Work.org"
	    "~/Documents/projects/org-files/agenda/Personal.org"))

(setq org-agenda-start-on-weekday 0)


					; Provides some calender options
(setq calendar-location-name "Aarhus, Denmark")
(setq calendar-latitude 56.199614)
(setq calendar-longitude 10.172997)
(provide 'rune-orgmode)
