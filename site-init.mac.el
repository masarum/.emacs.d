(set-face-attribute 'default nil :family "Hack" :height 120 :weight 'regular)
(set-face-attribute 'default nil :family "Victor Mono" :height 130 :weight 'demibold)
(set-face-attribute 'default nil :family "Source Code Pro" :height 120 :weight 'regular)

(custom-theme-set-faces
 'user
 '(default ((t (:family "Source Code Pro" :height 120 :weight regular))))
 '(fixed-pitch ((t (:family "Source Code Pro" :height 120 :weight regular))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 120 :weight normal)))))

;; (set-face-attribute 'default nil :font "InconsolataG 14")
;; (set-face-attribute 'default nil :font "IBM Plex Mono Light 14")
(setq-default line-spacing 0)

;; (set-fontset-font
;;  "fontset-default" 'unicode "Hack" nil 'prepend)

;; (set-fontset-font t '(#x2500 . #x257F) "Hack")

;; (defun voxlet/remap-faces-default-attributes ()
;;   (mapc
;;    (lambda (face)
;;      ;; (when (eq (face-attribute face :weight) 'bold)
;;      ;;   (set-face-attribute face nil :weight 'medium))
;;      (when (not (eq (face-attribute face :slant) 'normal))
;;        (set-face-attribute face nil :slant 'normal)))
;;    (face-list)))

;; (when (display-graphic-p)
;;   (add-hook 'minibuffer-setup-hook 'voxlet/remap-faces-default-attributes)
;;   (add-hook 'change-major-mode-after-body-hook 'voxlet/remap-faces-default-attributes))
