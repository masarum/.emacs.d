(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cygwin-mount-cygwin-bin-directory "C:/cygwin/bin")
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Consolas")))))

(setq visible-bell 1)
(tool-bar-mode -1)

(setq
  backup-by-copying t      ; don't clobber symlinks
  backup-directory-alist
    '(("." . "~/.emacs.d/backups")))  ; don't litter my fs tree

(when
  (load
    (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list
  'load-path
  (expand-file-name "~/.emacs.d/elisp/color-theme-6.6.0"))
(require 'zenburn)
(zenburn)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(require 'auto-install)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(setq explicit-bash-args '("--login" "-i"))
(require 'setup-cygwin)
