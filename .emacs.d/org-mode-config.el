(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED" "REVIEW" "DOING" "BUG" "ON HOLD")))
(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("CANCELED" . "red")
   ("REVIEW" . "chocolate4")
   ("BUG" . "gold2")
   ("DONE" . "OliveDrab4")
   ("ON HOLD" . "dark red")
   ("DOING" . "DodgetBlue3")))

(setq org-agenda-files (list"~/Documents/Org/Everything.org" ))
