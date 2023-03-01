;;; general usability

;; set up package archives
(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
		  ("melpa" . "https://melpa.org/packages/")
		  ("org" . "https://orgmode.org/elpa/")))
	(add-to-list 'package-archives archive))

;; pull latest package info
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; keep yes/no prompts brief
;; TODO: this doesn't work?
;; (defalias 'yes-or-no-p 'y-or-no-p)

;; pls no more ring
(setq ring-bell-function 'ignore)

;; pls no more C-M-i
(setq-default indent-tabs-mode nil)

;; pls no more ugly wrap
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :init
  (global-visual-line-mode 1))

;; keep transient files tidy
(setq tn/transient-files-backup-dir (expand-file-name "tmp/backups/" user-emacs-directory)
      tn/transient-files-auto-save-dir (expand-file-name "tmp/auto-saves/" user-emacs-directory)
      tn/transient-files-auto-save-prefix (expand-file-name "sessions" tn/transient-files-auto-save-dir))

(make-directory tn/transient-files-auto-save-dir t)
(make-directory tn/transient-files-backup-dir t)

(setq backup-directory-alist `(("." . ,tn/transient-files-backup-dir))
      auto-save-file-name-transforms `((".*" ,tn/transient-files-auto-save-dir t))
      auto-save-list-file-prefix tn/transient-files-auto-save-prefix
      create-lockfiles nil)

;; adjust transient buffer behaviors to personal preference

;;; mac usability tweaks

;; load system path on startup
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; comand -> meta and option -> super
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

;;; visual tweaks

;; preferred fonts
(setq tn/default-fixed-pitch-font "Iosevka Fixed"
      tn/default-variable-pitch-font "Iosevka Aile")

(defun my/apply-preferred-fonts ()
  (set-face-attribute 'default nil :font tn/default-fixed-pitch-font :weight 'light :height 200)
  (set-face-attribute 'fixed-pitch nil :font tn/default-fixed-pitch-font :weight 'light :height 200)
  (set-face-attribute 'variable-pitch nil :font tn/default-variable-pitch-font :weight 'light :height 200)
  (set-face-attribute 'bold nil :weight 'normal))

;; create after theme hook
(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

;; run font tweaks after theme change
(add-hook 'after-enable-theme-hook #'my/apply-preferred-fonts)

;; add minimal tab bar
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints nil)

(tab-bar-mode 1)

;; remove right fringe
(fringe-mode (cons nil 0))

;; remove tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; highlight line everywhere
(global-hl-line-mode 1)

;; display line numbers for code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; color theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

  (load-theme 'modus-operandi t)

  (define-key global-map (kbd "C-c t t") #'modus-themes-toggle))

;; pretty mode line
(use-package mood-line
  :ensure t
  :config
  (setq mood-line-glyph-alist  mood-line-glyphs-fira-code)
  (mood-line-mode))

;;; git tooling

;; magical git porcelean
(use-package magit
  :ensure t
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
	magit-pre-display-buffer-hook 'magit-save-window-configuration)

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (define-key global-map (kbd "C-c g b") #'magit-blame-addition)
  (define-key global-map (kbd "C-c g g") #'magit-status))

;; forge integration
(use-package forge
  :ensure t
  :after magit)

;; see changes in fringe
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))

;; travel through time
(use-package git-timemachine
  :ensure t
  :config
  (define-key global-map (kbd "C-c g t") #'git-timemachine))

;; jump to buffer or region in forge
(use-package browse-at-remote
  :ensure t
  :config
  (define-key global-map (kbd "C-c g r") #'browse-at-remote))

;;; tree-sitter tweaks

;; add mac-ports installed tree-sitter grammars
(setq treesit-extra-load-path '("/opt/local/lib"))

;;; language server

;; shared keymap setup
(setq my/code-keymap (make-sparse-keymap))
(define-key prog-mode-map (kbd "C-c c") `("code" . ,my/code-keymap))

;; setup eglot
(use-package eglot
  :after project
  :custom
  (eglot-autoshutdown
   t
   "Automatically shutdown when there are no buffers to manage")
  (eglot-extend-to-xref
   t
   "Apply eglot on out-of-project xref jumps"))

;; common use lsp operations
(setq my/lsp-keymap (make-sparse-keymap))
(define-key my/code-keymap (kbd "l") `("lsp" . ,my/lsp-keymap))
(define-key my/lsp-keymap (kbd "c") '("lsp-connect" . eglot-reconnect))
(define-key my/lsp-keymap (kbd "s") '("lsp-shutdown" . eglot-shutdown))

;; lsp powered code actions
(define-key my/code-keymap (kbd "r") '("rename-symbol" . eglot-rename))
(define-key my/code-keymap (kbd "e") '("error-list" . flymake-show-buffer-diagnostics))
(define-key my/code-keymap (kbd "d") '("jump-to-definition" . xref-find-definitions))
(define-key my/code-keymap (kbd "n") '("navigate" . imenu))
(define-key my/code-keymap (kbd "g") '("jump-to-references" . xref-find-references))

;;; language support

;;; general

;; combobulate

;; insert closing delimiters
(electric-pair-mode 1)

;; completions nice-to-haves
(setq tab-always-indent 'complete
      completion-auto-help 'always
      completions-auto-wrap t
      completions-auto-select 'second-tab)

;;; elisp

;;; python

;; use tree-sitter mode by default
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; start eglot automatically
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; make eglot unquestioningly use pyright
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode)
                 . ("pyright-langserver" "--stdio"))))

;; formatting
(use-package python-black
  :ensure t
  :demand t
  :after python-ts-mode
  :config
  (define-key python-ts-mode-map (kbd "C-c c f") '("format-buffer" . python-black-buffer))
  (define-key python-ts-mode-map (kbd "C-c c F") '("format-region" . python-black-region)))

(use-package python-isort
  :ensure t
  :demand t
  :after python-ts-mode
  :config
  (define-key python-ts-mode-map (kbd "C-c c o") '("organize-imports" . python-isort-buffer)))

;; virtual environments
(use-package pyvenv
  :ensure t
  :after python-ts-mode
  :init
  (setq my/python-venv-map (make-sparse-keymap))
  (define-key python-ts-mode-map (kbd "C-c c v") `("virtualenv" . ,my/python-venv-map))
  (define-key my/python-venv-map (kbd "a") '("activate" . pyvenv-activate))
  (define-key my/python-venv-map (kbd "d") '("de-activate" . pyvenv-deactivate)))

;; test running

;; c

;; elixir

;; javascript

;;; org

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" default))
 '(package-selected-packages '(adaptive-wrap mood-line modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
