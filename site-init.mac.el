(set-face-attribute 'default nil
		    :font "Source Code Pro 15" :weight 'light
		    :foreground "#ffe")
(setq-default line-spacing 0.2)
(mapc
 (lambda (face)
   (when (eq (face-attribute face :weight) 'bold)
     (set-face-attribute face nil :weight 'normal)))
 (face-list))
