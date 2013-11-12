;; Shell mode configs

(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

;; Removes the ^M signs
 (add-hook 'comint-output-filter-functions
            'comint-strip-ctrl-m)
