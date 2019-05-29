(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq use-dialog-box nil)
(tooltip-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq scroll-margin 1
      scroll-conservatively 1
      mouse-wheel-scroll-amount '(1))
(winner-mode 1)

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(global-auto-revert-mode)
(defvar whitespace-style '(face trailing))
(whitespace-mode 1)
(defvar show-paren-delay 0)
(show-paren-mode 1)
(delete-selection-mode 1)

(global-set-key (kbd "C-z") 'undo)

(defun my-pop-local-mark-ring ()
  "Pop mark ring."
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "C-.") 'unpop-to-mark-command)

(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvar c-default-style "linux")
(defvaralias 'c-basic-offset 'tab-width)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
           '(("zenburn-bg-1"  . "#101010")
             ("zenburn-bg-05" . "#202020")
             ("zenburn-bg"    . "#2B2B29")
             ("zenburn-fg"    . "#EFEFDF"))))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  :bind (("M-o" . ace-window)))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle))
  :hook (dired-mode . dired-hide-details-mode))

(use-package neotree
  :custom (neo-smart-open t))

(use-package buffer-flip
  :custom
  (buffer-flip-skip-patterns '("^\\*helm\\b"
                               "^\\*swiper\\*$"))
  :bind
  (("C-S-<tab>" . buffer-flip)
   ("C-S-<iso-lefttab>" . buffer-flip)
   :map buffer-flip-map
   ("C-S-<tab>" . buffer-flip-forward)
   ("C-S-<iso-lefttab>" . buffer-flip-forward)
   ("C-<tab>" . buffer-flip-backward)
   ("C-g" . buffer-flip-abort)))

(use-package golden-ratio-scroll-screen
  :custom-face
  (golden-ratio-scroll-highlight-line-face ((t (:inherit highlight))))
  :bind
  ([remap scroll-down-command] . golden-ratio-scroll-screen-down)
  ([remap scroll-up-command] . golden-ratio-scroll-screen-up))

(use-package avy
  :bind ("C-;" . avy-goto-char-2))

(use-package paren-face
  :custom (paren-face-regexp "[][{}()]")
  :custom-face (parenthesis ((t (:inherit shadow :foreground "gray48"))))
  :init (global-paren-face-mode))

(use-package smart-mode-line
  :config
  (column-number-mode t)
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
  (ivy-re-builders-alist '((swiper-isearch . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  :config (ivy-mode)
  :bind
  (("C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
  :delight
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper-isearch)))

(use-package company
  :delight
  :init (global-company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  :bind (("TAB" . company-indent-or-complete-common)))

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

(defun add-space-after-insert (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (length (plist-get (sp-get-pair id) :close)))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp
                             (sp--get-allowed-pair-list))))
        (insert " ")))))

(use-package smartparens
  :delight
  :init (smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "(" nil :post-handlers '(:add add-space-after-insert))
    (sp-local-pair "[" nil :post-handlers '(:add add-space-after-insert))
    (sp-local-pair "{" nil :post-handlers '(:add add-space-after-insert)))
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
  :init (global-flycheck-mode)
  :custom (sentence-end-double-space nil))

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
  (cider-font-lock-dynamically '(macro core function var))
  (cider-overlays-use-font-lock t)
  (cider-save-file-on-load t)
  :hook
  ((cider-repl-mode cider-mode) . cider-company-enable-fuzzy-completion)
  (cider-mode . eldoc-mode)
  :custom-face
  (clojure-keyword-face ((t (:inherit font-lock-constant-face :slant italic)))))

(use-package yasnippet
  :delight (yas-minor-mode))

(use-package clj-refactor
  :delight
  :custom
  (cljr-warn-on-eval nil)
  (cljr-eagerly-build-asts-on-startup nil)
  :config (cljr-add-keybindings-with-prefix "C-c r")
  :hook
  (clojure-mode . clj-refactor-mode)
  (clojure-mode . yas-minor-mode))

(use-package flycheck-clojure
  :after flycheck
  :hook (flycheck-mode . flycheck-clojure-setup))

;; JavaScript/TypeScript
(use-package js2-mode
  :mode "\\.js\\'"
  :custom (js2-basic-offset 2)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . eldoc-mode))

(use-package json-mode
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

(use-package js2-refactor
  :config (js2r-add-keybindings-with-prefix "C-c r")
  :bind (:map js2-mode-map (("C-k" . js2r-kill)))
  :hook (js2-mode . js2-refactor-mode))

(use-package indium
  :hook (js2-mode . indium-interaction-mode))

(use-package typescript-mode
  :custom (typescript-indent-level 2)
  :hook
  (typescript-mode . eldoc-mode)
  (typescript-mode . display-line-numbers-mode))

(use-package tide
  :hook
  ((js2-mode typescript-mode) . tide-setup)
  ((js2-mode typescript-mode) . tide-hl-identifier-mode))

(use-package prettier-js
  :hook ((json-mode js2-mode typescript-mode) . prettier-js-mode))

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

(use-package plantuml-mode
  :custom (plantuml-jar-path "~/bin/plantuml.jar"))

(let ((site-init "~/.emacs.d/site-init.el"))
  (when (file-exists-p site-init)
    (load-file site-init)))

