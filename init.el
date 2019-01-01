(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package delight)

(use-package zenburn-theme
  :init (load-theme 'zenburn t)) 

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package which-key
  :delight
  :init (which-key-mode))

(use-package ivy
  :delight
  :defer 0.1
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "")
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config (ivy-mode)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
  :delight
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package company
  :delight
  :init (global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t))

(use-package company-flx
  :after company
  :config (company-flx-mode +1))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))

(use-package smartparens
  :init (smartparens-global-mode t)
  :hook (clojure-mode . turn-on-smartparens-strict-mode)
  :config (require 'smartparens-config)
  :bind
  (("C-M-f" . 'sp-forward-sexp)
   ("C-M-b" . 'sp-backward-sexp)
   ("C-M-d" . 'sp-down-sexp)
   ("C-M-a" . 'sp-backward-down-sexp)
   ("C-S-d" . 'sp-beginning-of-sexp)
   ("C-S-a" . 'sp-end-of-sexp)
   ("C-M-e" . 'sp-up-sexp)
   ("C-M-u" . 'sp-backward-up-sexp)
   ("C-M-t" . 'sp-transpose-sexp)
   ("C-M-n" . 'sp-forward-hybrid-sexp)
   ("C-M-p" . 'sp-backward-hybrid-sexp)
   ("C-M-k" . 'sp-kill-sexp)
   ("C-M-w" . 'sp-copy-sexp)
   ("M-<delete>" . 'sp-unwrap-sexp)
   ("M-<backspace>" . 'sp-backward-unwrap-sexp)
   ("C-<right>" . 'sp-forward-slurp-sexp)
   ("C-<left>" . 'sp-forward-barf-sexp)
   ("C-M-<left>" . 'sp-backward-slurp-sexp)
   ("C-M-<right>" . 'sp-backward-barf-sexp)
   ("M-D" . 'sp-splice-sexp)
   ("C-M-<delete>" . 'sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . 'sp-splice-sexp-killing-backward)
   ("C-S-<backspace>" . 'sp-splice-sexp-killing-around)
   ("C-]" . 'sp-select-next-thing-exchange)
   ("C-<left_bracket>" . 'sp-select-previous-thing)
   ("C-M-]" . 'sp-select-next-thing)
   ("M-F" . 'sp-forward-symbol)
   ("M-B" . 'sp-backward-symbol)
   ("C-\"" . 'sp-change-inner)
   ("M-i" . 'sp-change-enclosing)))

(use-package cider
  :delight " cider"
  :hook
  ((cider-repl-mode cider-mode) . cider-company-enable-fuzzy-completion)
  (clojure-mode . linum-mode)
  :custom
  (cider-repl-use-pretty-print t)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-overlays-use-font-lock t)
  :custom-face
  (clojure-keyword-face ((t (:inherit font-lock-constant-face :slant italic)))))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :config (projectile-mode +1)
  :bind-keymap ("M-p" . projectile-command-map))

(use-package counsel-projectile
  :after counsel projectile
  :config (counsel-projectile-mode))

(setq-default left-margin-width 1)
(set-window-buffer nil (current-buffer))
(setq linum-format "%d ")

(toggle-scroll-bar -1)
(setq scroll-margin 12
      scroll-conservatively 1
      mouse-wheel-scroll-amount '(1))

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(setq show-paren-delay 0)
(show-paren-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
