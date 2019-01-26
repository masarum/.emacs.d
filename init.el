(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package delight
  :config
  (delight '((eldoc-mode nil "eldoc")
	     (whitespace-mode nil "whitespace"))))

(use-package zenburn-theme
  :init (load-theme 'zenburn t)
  :custom (zenburn-override-colors-alist
           '(("zenburn-bg-1"     . "#101010")
             ("zenburn-bg-05"    . "#202020")
             ("zenburn-bg"       . "#2B2B29"))))

(use-package ace-window
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package buffer-flip
  :custom
  (buffer-flip-skip-patterns '("^\\*helm\\b"
                               "^\\*swiper\\*$"))
  :bind
  (("C-<tab>" . buffer-flip)
   ("C-S-<tab>" . buffer-flip-backward)
   ("C-S-<iso-lefttab>" . buffer-flip-backward)
   :map buffer-flip-map
   ("C-<tab>" . buffer-flip-forward)
   ("C-S-<tab>" . buffer-flip-backward)
   ("C-S-<iso-lefttab>" . buffer-flip-backward)
   ("ESC" . buffer-flip-abort)))

(use-package golden-ratio-scroll-screen
  :custom-face
  (golden-ratio-scroll-highlight-line-face ((t (:inherit highlight))))
  :bind
  ([remap scroll-down-command] . golden-ratio-scroll-screen-down)
  ([remap scroll-up-command] . golden-ratio-scroll-screen-up))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer))

(use-package paren-face
  :custom (paren-face-regexp "[][{}()]")
  :custom-face (parenthesis ((t (:inherit shadow :foreground "gray48"))))
  :init (global-paren-face-mode))

(use-package smart-mode-line
  :config
  (column-number-mode t)
  (sml/setup))

(use-package popwin
  :config
  (push '("*cider-error*"
	  :dedicated t :position bottom :stick t :noselect t :height 0.4)
	popwin:special-display-config)
  (push '("*cider-doc*"
	  :dedicated t :position bottom :stick t :noselect t :height 0.4)
	popwin:special-display-config)
  (popwin-mode 1))

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
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t))

(use-package company-flx
  :after company
  :config (company-flx-mode +1))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :config (projectile-mode +1)
  :bind-keymap ("M-p" . projectile-command-map))

(use-package counsel-projectile
  :after counsel projectile
  :init (counsel-projectile-mode))

(use-package column-enforce-mode
  :delight
  :init (global-column-enforce-mode t))

(use-package expand-region
  :bind
  (("M-S-<up>" . 'er/expand-region)
   ("M-S-<down>" . 'er/contract-region)))

(use-package hungry-delete
  :delight
  :init (global-hungry-delete-mode))

(use-package aggressive-indent
  :delight
  :init (global-aggressive-indent-mode 1))

(use-package whitespace-cleanup-mode
  :delight
  :init (global-whitespace-cleanup-mode))

(use-package smartparens
  :delight
  :init (smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  :bind
  (:map smartparens-mode-map
        (("M-<right>" . 'sp-forward-sexp)
         ("M-<left>" . 'sp-backward-sexp)
         ("M-<up>" . 'sp-backward-up-sexp)
         ("M-<down>" . 'sp-up-sexp)))
  :hook (clojure-mode . turn-on-smartparens-strict-mode))

(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :custom (flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  :init (flycheck-pos-tip-mode))

(use-package org)

(use-package magit
  :bind (("C-x g")))

;; Clojure

(use-package clojure-mode
  :hook (clojure-mode . display-line-numbers-mode))

(use-package cider
  ;;:pin melpa-stable
  :delight " cider"
  :custom
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-use-pretty-print t)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-overlays-use-font-lock t)
  (cider-prompt-save-file-on-load 'always-save)
  :hook
  ((cider-repl-mode cider-mode) . cider-company-enable-fuzzy-completion)
  (cider-mode . eldoc-mode)
  :custom-face
  (clojure-keyword-face ((t (:inherit font-lock-constant-face :slant italic)))))

(use-package yasnippet
  :delight (yas-minor-mode)
  :config (yas-minor-mode 1))

(use-package clj-refactor
  :delight
  :custom (cljr-warn-on-eval nil)
  :config (cljr-add-keybindings-with-prefix "C-c f")
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck-clojure
  :after flycheck
  :hook (flycheck-mode . flycheck-clojure-setup))

;; Rust

(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after flycheck
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(setq c-default-style "linux")

(toggle-scroll-bar -1)
(setq scroll-margin 1
      scroll-conservatively 1
      mouse-wheel-scroll-amount '(1))

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(global-auto-revert-mode)
(setq whitespace-style '(face trailing))
(whitespace-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(delete-selection-mode 1)

(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2
      c-basic-offset 2)

(let ((site-init "~/.emacs.d/site-init.el"))
  (when (file-exists-p site-init)
    (load-file site-init)))

(setq custom-file "~/.emacs.d/custom.el")
