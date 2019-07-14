;; (set-face-attribute 'default nil :font "InconsolataG 14")
(set-face-attribute 'default nil :font "Victor Mono-13:demibold")
;; (set-face-attribute 'default nil :font "IBM Plex Mono Light 14")
;; (setq-default line-spacing 0)

(defun local-remap-faces-default-attributes ()
  (mapc
   (lambda (face)
     ;; (when (eq (face-attribute face :weight) 'bold)
     ;;   (set-face-attribute face nil :weight 'medium))
     (when (not (eq (face-attribute face :slant) 'normal))
       (set-face-attribute face nil :slant 'normal)))
   (face-list)))

(when (display-graphic-p)
  (add-hook 'minibuffer-setup-hook 'local-remap-faces-default-attributes)
  (add-hook 'change-major-mode-after-body-hook 'local-remap-faces-default-attributes))
