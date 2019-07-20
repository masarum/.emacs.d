(custom-theme-set-faces
 'user
 '(default ((t (:family "Victor Mono" :height 130 :weight demibold))))
 '(fixed-pitch ((t (:family "Victor Mono" :height 130 :weight demibold))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 130 :weight light)))))

;; (set-face-attribute 'default nil :font "InconsolataG 14")
;; (set-face-attribute 'default nil :family "Victor Mono" :height 130 :weight 'demibold)
;; (set-face-attribute 'default nil :font "IBM Plex Mono Light 14")
;; (setq-default line-spacing 0)

(defun voxlet/remap-faces-default-attributes ()
  (mapc
   (lambda (face)
     ;; (when (eq (face-attribute face :weight) 'bold)
     ;;   (set-face-attribute face nil :weight 'medium))
     (when (not (eq (face-attribute face :slant) 'normal))
       (set-face-attribute face nil :slant 'normal)))
   (face-list)))

(when (display-graphic-p)
  (add-hook 'minibuffer-setup-hook 'voxlet/remap-faces-default-attributes)
  (add-hook 'change-major-mode-after-body-hook 'voxlet/remap-faces-default-attributes))
