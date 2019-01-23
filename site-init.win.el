(set-face-attribute 'default nil :font "Office Code Pro 13" :weight 'light)
(setq-default line-spacing 0.1)
(mapc
 (lambda (face)
   (when (eq (face-attribute face :weight) 'bold)
     (set-face-attribute face nil :weight 'normal)))
 (face-list))
